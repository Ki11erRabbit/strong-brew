use petgraph::algo;
use petgraph::graph::UnGraph;
use sb_ast::core_annotated::{self, BinaryOperator, BuiltinType, Call, CallArg, Enum, Expression, ExpressionRaw, Field, IfExpr, Import, Literal, Param, PathName, TopLevelStatement, Type, UnaryOperator};
use sb_ast::core_lang;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;
use either::Either;

#[derive(Debug)]
pub enum TypeError {
    TypeDoesNotExist(Vec<String>, usize, usize),
    PatternTypeMismatch(String),
    ParameterizedTypeWithoutName(usize, usize),
    TypeMismatch(Type, Type, usize, usize),
    ExpectedType(Type, Type),
    NotAType,
    UnaryOperatorNotDefined(String, usize, usize),
    BinaryOperatorNotDefined(String, usize, usize),
    FunctionDoesNotExist(Vec<String>, usize, usize),
}

fn generate_internal_globals() -> HashMap<Vec<String>, Rc<RefCell<Type>>> {
    let mut globals = HashMap::new();
    globals.insert(vec!["nat", "Zero"].into_iter().map(|x| x.to_string()).collect(),
                   Rc::new(RefCell::new(
                           Type::Builtin(BuiltinType::Function {
                               params: vec![],
                               return_type: Box::new(Type::Builtin(BuiltinType::Nat)),
                           }),
                       )));
    globals.insert(vec!["nat", "Succ"].into_iter().map(|x| x.to_string()).collect(),
                   Rc::new(RefCell::new(
                           Type::Builtin(BuiltinType::Function {
                               params: vec![
                                       Type::Builtin(BuiltinType::Nat)
                               ],
                               return_type: Box::new(
                                       Type::Builtin(BuiltinType::Nat))
                           }),
                       )));
    globals.insert(vec!["String"].into_iter().map(|x| x.to_string()).collect(),
                   Rc::new(RefCell::new(
                           Type::User(PathName::new(vec!["String"], 0, 0)),
                       )));



    globals
}

fn create_constructor_type(fields: Vec<Field>, return_type: Type) -> Type {
    let fields = fields.into_iter().map(|x| x.ty).collect();
    Type::Builtin(BuiltinType::Function {
        params: fields,
        return_type: Box::new(return_type),
    })
}

fn create_function_type(params: Vec<Param>, return_type: Type) -> Type {
    let params = params.into_iter().map(|x| x.ty).collect();
    Type::Builtin(BuiltinType::Function {
        params,
        return_type: Box::new(return_type),
    })
}

pub struct TypeChecker {
    overloads: HashMap<String, Vec<Vec<String>>>,
    global_types: HashMap<Vec<String>, Rc<RefCell<Type>>>,
    local_types: Vec<HashMap<Vec<String>, Rc<RefCell<Type>>>>,
    seen_files: HashSet<String>,
    current_return_type: Option<Type>,
    current_param_type: Option<Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            overloads: HashMap::new(),
            global_types: generate_internal_globals(),
            local_types: Vec::new(),
            seen_files: HashSet::new(),
            current_return_type: None,
            current_param_type: None,
        }
    }
    pub fn add_overload<S, R>(&mut self, name: S, overload: &Vec<R>)
    where S: AsRef<str>, R: AsRef<str> {
        let name = name.as_ref();
        let overload = overload.into_iter().map(|x| x.as_ref().to_string()).collect();
        match self.overloads.entry(name.to_string()) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                entry.get_mut().push(overload);
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(vec![overload]);
            }
        }
    }

    pub fn get_overloads<S>(&self, name: S) -> Option<&Vec<Vec<String>>>
    where S: AsRef<str> {
        self.overloads.get(name.as_ref())
    }

    pub fn add_global_type<S>(&mut self, name: &Vec<S>, ty: Rc<RefCell<Type>>)
    where S: AsRef<str> {
        let name = name.into_iter().map(|x| x.as_ref().to_string()).collect();
        self.global_types.insert(name, ty);
    }

    pub fn add_local_type<S>(&mut self, name: &Vec<S>, ty: Rc<RefCell<Type>>)
    where S: AsRef<str>{
        let name = name.into_iter().map(|x| x.as_ref().to_string()).collect();
        self.local_types.last_mut().unwrap().insert(name, ty);
    }

    pub fn push_local_scope(&mut self) {
        self.local_types.push(HashMap::new());
    }

    pub fn pop_local_scope(&mut self) {
        self.local_types.pop();
    }

    pub fn get_return_type(&self) -> Option<Type> {
        self.current_return_type.clone()
    }

    pub fn set_return_type(&mut self, ty: Type) {
        self.current_return_type = Some(ty);
    }

    pub fn unset_return_type(&mut self) {
        self.current_return_type = None;
    }

    pub fn get_param_type(&self) -> Option<Type> {
        self.current_param_type.clone()
    }

    pub fn set_param_type(&mut self, ty: Type) {
        self.current_param_type = Some(ty);
    }

    pub fn unset_param_type(&mut self) {
        self.current_param_type = None;
    }

    pub fn get_type<S>(&self, name: &Vec<S>) -> Option<Rc<RefCell<Type>>>
    where S: AsRef<str> {
        let name: Vec<String> = name.into_iter().map(|x| x.as_ref().to_string()).collect();
        for scope in self.local_types.iter().rev() {
            if let Some(ty) = scope.get(&name) {
                return Some(ty.clone());
            }
        }
        self.global_types.get(&name).map(|ty| ty.clone())
    }

    fn convert_visibility(visibility: &core_lang::Visibility) -> core_annotated::Visibility {
        match visibility {
            core_lang::Visibility::Public => core_annotated::Visibility::Public,
            core_lang::Visibility::Private => core_annotated::Visibility::Private,
        }
    }

    pub fn check_files<S>(
        &mut self,
        files: &[(S, core_lang::File)]
    ) -> Result<Vec<core_annotated::File>, TypeError>
    where S: AsRef<str> {
        let mut result = Vec::new();
        for (name, file) in files {
            if self.seen_files.contains(name.as_ref()) {
                continue;
            }
            result.push(self.check_file(name.as_ref(), file)?);
        }
        Ok(result)
    }
    
    fn check_file(
        &mut self,
        name: &str,
        file: &core_lang::File
    ) -> Result<core_annotated::File, TypeError> {
        self.push_local_scope();
        let core_lang::PathName { segments, start, end } = &file.path;
        let path = PathName::new(segments.clone(), *start, *end);

        println!("Checking file: {}", name);

        let mut imports = Vec::new();
        let mut enums = Vec::new();
        let mut consts = Vec::new();
        let mut functions = Vec::new();
        let mut externs = Vec::new();
        
        
        for decl in &file.content {
            match decl {
                core_lang::TopLevelStatement::Import(_) => {
                    imports.push(decl);
                }
                core_lang::TopLevelStatement::Enum(_) => {
                    enums.push(decl);
                }
                core_lang::TopLevelStatement::Const(_) => {
                    consts.push(decl);
                }
                core_lang::TopLevelStatement::Function(_) => {
                    functions.push(decl);
                }
                core_lang::TopLevelStatement::Extern(lang, body) => {
                    externs.push(TopLevelStatement::Extern(lang.to_string(), body.to_string()));
                }
            }
        }

        let imports = Self::convert_imports(imports);
        let enums = self.check_enums(enums)?;
        let consts = self.check_consts(consts)?;
        let functions = self.check_functions(functions)?;

        let mut decl = imports;
        decl.extend(enums);
        decl.extend(consts);
        decl.extend(functions);
        decl.extend(externs);


        let file = core_annotated::File::new(path, decl);
        self.pop_local_scope();
        Ok(file)
    }

    fn convert_imports(imports: Vec<&core_lang::TopLevelStatement>) -> Vec<TopLevelStatement> {
        imports.iter().map(|x| match x {
            core_lang::TopLevelStatement::Import(x) => {
                let core_lang::Import { path, start: istart, end: iend } = x;
                let core_lang::PathName { segments, start, end } = path;
                let path = PathName::new(segments.clone(), *start, *end);
                Import::new(path.clone(), *istart, *iend)
            },
            _ => unreachable!(),
        }).collect()
    }


    fn check_enums(&mut self, enums: Vec<&core_lang::TopLevelStatement>) -> Result<Vec<TopLevelStatement>, TypeError> {
        let mut result = Vec::new();

        let mut names_to_positions = HashMap::new();

        for (i, enum_) in enums.iter().enumerate() {
            let core_lang::TopLevelStatement::Enum(enum_) = enum_ else {
                unreachable!("Encountered a non-enum after filtering only enums")
            };
            names_to_positions.insert(enum_.name, i);
        }
        
        let mut edges = Vec::new();

        for (i, enum_) in enums.iter().enumerate() {
            let core_lang::TopLevelStatement::Enum(enum_) = enum_ else {
                unreachable!("Encountered a non-enum after filtering only enums")
            };
            let core_lang::Enum { variants, .. } = enum_;
            for variant in variants {
                let core_lang::Variant { name, ..} = variant;
                if let Some(j) = names_to_positions.get(name) {
                    edges.push((i, *j));
                }
            }
        }
        let edges = edges.iter().map(|(x, y)| (*x as u32, *y as u32)).collect::<Vec<(u32, u32)>>();

        let graph = UnGraph::<u32, ()>::from_edges(&edges);

        let nodes = algo::toposort(&graph, None).unwrap();

        let nodes = nodes.into_iter().map(|x| x.index()).collect::<Vec<usize>>();

        //let nodes = nodes.into_iter().flatten().map(|x| x.index()).collect::<Vec<usize>>();

        let nodes = if nodes.len() < enums.len() {
            (0..enums.len()).collect::<Vec<usize>>()
        } else {
            nodes
        };

        let mut enums = enums.into_iter()
            .map(Some)
            .collect::<Vec<Option<&core_lang::TopLevelStatement>>>();

        for node in nodes {
            let enum_ = enums[node].take().unwrap();

            let core_lang::TopLevelStatement::Enum(enum_) = enum_ else {
                unreachable!("Encountered a non-enum after filtering only enums")
            };
            result.push(self.check_enum(enum_)?);
        }

        Ok(result)
    }

    fn check_enum(&mut self, enum_: &core_lang::Enum) -> Result<TopLevelStatement, TypeError> {
        let core_lang::Enum {
            visibility,
            name,
            generic_params,
            variants,
            start,
            end,
        } = enum_;

        self.push_local_scope();

        let visibility = Self::convert_visibility(visibility);
        let pairs = generic_params
            .iter()
            .map(|x| {
                let (p, (names, ty)) = self.convert_generic_parameter(x)?;
                for name in names {
                    self.add_local_type(&vec![name], Rc::new(RefCell::new(ty.clone())));
                }
                Ok((p, ty))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut generic_params = Vec::new();
        let mut generic_args = Vec::new();
        for (param, ty) in pairs {
            generic_params.push(param);
            generic_args.push(ty);
        }

        let ty = if generic_params.is_empty() {
            let ty = Type::User(PathName::new(vec![name], *start, *end));
            self.add_global_type(&vec![name], Rc::new(RefCell::new(
                Type::User(PathName::new(vec![name], *start, *end)),
            )));
            ty
        } else {
            let args = generic_args.into_iter().map(|x| {
                x
            }).collect();

            let ty = Type::Parameterized(
                        Box::new(Type::User(PathName::new(vec![name], *start, *end))),
                        args,
            );

            let rc_ty = Rc::new(RefCell::new(ty.clone()));
            self.add_global_type(&vec![name], rc_ty);
            ty
        };

        let variants = self.check_variants(variants, ty, name)?;

        self.pop_local_scope();

        
        Ok(Enum::new(visibility, name, generic_params, variants, *start, *end))
    }

    fn check_variants(
        &mut self,
        variants: &Vec<core_lang::Variant>,
        enum_type: Type,
        enum_name: &str
    ) -> Result<Vec<core_annotated::Variant>, TypeError> {
        let mut result = Vec::new();
        for variant in variants {
            let core_lang::Variant { name, fields, start, end } = variant;
            let fields = fields.iter().map(|x| self.check_field(x)).collect::<Result<Vec<_>, _>>()?;

            let constructor_type = create_constructor_type(fields.clone(), enum_type.clone());

            self.add_global_type(&vec![enum_name, name], Rc::new(RefCell::new(constructor_type)));
            
            
            result.push(core_annotated::Variant::new(name, fields, *start, *end));
        }
        Ok(result)
    }

    fn check_field(&mut self, field: &core_lang::Field) -> Result<core_annotated::Field, TypeError> {
        let core_lang::Field { visibility, name, ty, start, end } = field;
        let visibility = Self::convert_visibility(visibility);
        //println!("check_field: {:?}", ty);
        let ty = self.reduce_to_type_expression_type(ty)?;
        let ty = self.does_type_exist_annotated(ty)?;
        //println!("check_field: {:?}", ty);
        Ok(core_annotated::Field::new(visibility, name, ty, *start, *end))
    }

    fn convert_generic_parameter(
        &mut self,
        param: &core_lang::GenericParam
    ) -> Result<(core_annotated::GenericParam, (Vec<String>, Type)), TypeError> {
        let core_lang::GenericParam { name, constraint, start, end } = param;
        let constraint = constraint.as_ref().map(|x| self.does_type_exist_type(&x)).transpose()?;


        let ty = constraint.unwrap_or_else(|| {
            Type::Builtin(BuiltinType::Type)
        });

        let bound_names = vec![name.to_string()];
        
        let param = core_annotated::GenericParam::new(name, Some(ty.clone()), *start, *end);
        
        Ok((param, (bound_names, ty)))
    }

    fn does_type_exist(&mut self, ty: &core_lang::Type) -> Result<core_annotated::Type, TypeError> {
        let ty = self.convert_type(ty)?;

        match ty {
            Type::User(PathName { segments, start, end }) => {
                if let Some(ty) = self.get_type(&segments) {
                    return Ok(ty.borrow().clone());
                }
                Err(TypeError::TypeDoesNotExist(segments, start, end))
            }
            ty => Ok(ty),
        }
    }

    /// Here we check to see if the type exists in the global types
    fn does_type_exist_annotated(&mut self, ty: Type) -> Result<core_annotated::Type, TypeError> {
        match ty {
            Type::User(PathName { ref segments, ref start, ref end }) => {
                if let Some(_) = self.get_type(&segments) {
                    return Ok(ty);
                }
                Err(TypeError::TypeDoesNotExist(segments.clone(), *start, *end))
            }
            ty => Ok(ty),
        }
    }

    fn does_type_exist_type(&mut self, ty: &core_lang::ExpressionType) -> Result<core_annotated::Type, TypeError> {

        let core_lang::ExpressionType { expression, variadic } = ty;
        let ty = self.reduce_to_type(expression)?;
        let ty = self.does_type_exist_annotated(ty)?;

        if *variadic {
            let ty = Type::Parameterized(Box::new(
                Type::User(PathName::new(vec!["Array"], 0, 0))), vec![ty]);
            return Ok(ty);
        }

        Ok(ty)
    }

    fn convert_type(&mut self, ty: &core_lang::Type) -> Result<core_annotated::Type, TypeError> {
        let ty = match ty {
            core_lang::Type::Builtin(builtin) => {
                let ty = match builtin {
                    core_lang::BuiltinType::Bool => BuiltinType::Bool,
                    core_lang::BuiltinType::Nat => BuiltinType::Nat,
                    core_lang::BuiltinType::Type => BuiltinType::Type,
                    core_lang::BuiltinType::Int => BuiltinType::Int,
                    core_lang::BuiltinType::I8 => BuiltinType::I8,
                    core_lang::BuiltinType::I16 => BuiltinType::I16,
                    core_lang::BuiltinType::I32 => BuiltinType::I32,
                    core_lang::BuiltinType::I64 => BuiltinType::I64,
                    core_lang::BuiltinType::Char => BuiltinType::Char,
                    core_lang::BuiltinType::F32 => BuiltinType::F32,
                    core_lang::BuiltinType::F64 => BuiltinType::F64,
                    core_lang::BuiltinType::Unit => BuiltinType::Unit,
                    core_lang::BuiltinType::Never => BuiltinType::Never,
                    core_lang::BuiltinType::Function { params, return_type } => {
                        let params = params.iter()
                            .map(|x| self.does_type_exist_type(x))
                            .collect::<Result<Vec<_>, _>>()?;
                        let return_type = self.does_type_exist_type(return_type)?;
                        BuiltinType::Function { params, return_type: Box::new(return_type) }
                    }
                    
                };
                Type::Builtin(ty)
            }
            core_lang::Type::User(path) => {
                let core_lang::PathName { segments, start, end } = path;
                let segments = segments.clone();
                Type::User(PathName::new(segments, *start, *end))
            }
            core_lang::Type::Parameterized(main, exprs) => {
                let main = Box::new(self.convert_type(main)?);
                let exprs = exprs.iter()
                    .map(|x| self.reduce_to_type(x))
                    .collect::<Result<Vec<_>, _>>()?;
                Type::Parameterized(main, exprs)
            }
        };
        Ok(ty)
    }

    fn reduce_to_type_expression_type(&mut self, expr: &core_lang::ExpressionType) -> Result<Type, TypeError> {

        let core_lang::ExpressionType { expression, variadic } = expr;

        let ty = match expression {
            core_lang::Expression::Type(ty) => {
                self.convert_type(ty)?
            }
            _ => {
                self.reduce_to_type(expression)?
            }
        };

        if *variadic {
            let ty = Type::Parameterized(Box::new(
                Type::User(PathName::new(vec!["Array"], 0, 0))), vec![ty]);
            return Ok(ty);
        }
        Ok(ty)
    }
    
    fn reduce_to_type(&mut self, expr: &core_lang::Expression) -> Result<Type, TypeError> {

        match expr {
            core_lang::Expression::Type(ty) => {
                self.convert_type(ty)
            }
            core_lang::Expression::Literal(lit) => {
                match lit {
                    core_lang::Literal::Unit => return Ok(Type::Builtin(BuiltinType::Unit)),
                    _ => {}
                }
                Err(TypeError::NotAType)
            }
            core_lang::Expression::Variable(var) => {
                let core_lang::PathName { segments, start, end } = var;
                let segments = segments.clone();
                let ty = Type::User(PathName::new(segments, *start, *end));
                Ok(ty)
            }
            core_lang::Expression::Constant(constant) => {
                let core_lang::PathName { segments, start, end } = constant;
                let segments = segments.clone();
                let ty = Type::User(PathName::new(segments, *start, *end));
                Ok(ty)
            }
            core_lang::Expression::IfExpression(_) => {
                todo!("implement type reducing for if expressions")
            }
            core_lang::Expression::MatchExpression(_) => {
                todo!("implement type reducing for match expressions")
            }
            core_lang::Expression::Call(_) => {
                todo!("implement type reducing for calls")
            }
            core_lang::Expression::Tuple(tuple) => {
                let tuple = tuple.iter().map(|x| self.convert_expression_type(x, true)).collect::<Result<Vec<_>, _>>()?;

                let tuple = tuple.into_iter().map(|x| {
                    match x {
                        ExpressionRaw::Type(ty) => Ok(ty),
                        _ => {
                            Err(TypeError::NotAType)
                        },
                    }
                }).collect::<Result<Vec<_>, _>>()?;
                
                Ok(Type::Tuple(tuple))
            }
            core_lang::Expression::Parenthesized(expr) => {
                let ty = self.reduce_to_type(expr)?;
                Ok(ty)
            }
            _ => {
                Err(TypeError::NotAType)
            }
        }
    }

    fn convert_pattern(
        &mut self,
        name: &core_lang::Pattern,
    ) -> Result<core_annotated::Pattern, TypeError> {
        match name {
            core_lang::Pattern::Variable(var) => {
                match *var {
                    "_" => {
                        return Ok(core_annotated::Pattern::Wildcard);
                    }
                    _ => {}
                }
                Ok(core_annotated::Pattern::Variable(var.to_string()))
            }
            core_lang::Pattern::Literal(lit) => {
                let lit = self.convert_literal(lit)?;
                Ok(core_annotated::Pattern::Literal(lit))
            }
            core_lang::Pattern::Tuple(patterns) => {
                let patterns = patterns.iter().map(|x| self.convert_pattern(x)).collect::<Result<Vec<_>, _>>()?;
                Ok(core_annotated::Pattern::Tuple(patterns))
            }
            core_lang::Pattern::Constructor { name, fields } => {
                let core_lang::PathName { segments, start, end } = name;
                let segments = segments.clone();
                let name = PathName::new(segments, *start, *end);
                let fields = fields.iter().map(|x| self.convert_pattern(x)).collect::<Result<Vec<_>, _>>()?;
                Ok(core_annotated::Pattern::Constructor { name, fields })
            }
        }
    }
    

    fn convert_literal(&mut self, lit: &core_lang::Literal) -> Result<Literal, TypeError> {
        match lit {
            core_lang::Literal::Bool(b) => Ok(Literal::Bool(*b)),
            core_lang::Literal::Float(f) => Ok(Literal::Float(f.to_string())),
            core_lang::Literal::Int(i) => Ok(Literal::Int(i.to_string())),
            core_lang::Literal::Char(c) => Ok(Literal::Char(c.to_string())),
            core_lang::Literal::String(s) => Ok(Literal::String(s.to_string())),
            core_lang::Literal::Unit => Ok(Literal::Unit),
            core_lang::Literal::List(l) => {
                let l = l.iter().map(|x| self.convert_expression(x, true)).collect::<Result<Vec<_>, _>>()?;
                Ok(Literal::List(l))
            }
        }
    }

    fn get_literal_type(&mut self, lit: &Literal) -> Type {
        match lit {
            Literal::Bool(_) => Type::Builtin(BuiltinType::Bool),
            Literal::Float(_) => Type::PossibleType(vec![
                Type::Builtin(BuiltinType::F32),
                Type::Builtin(BuiltinType::F64),
            ]),
            Literal::Int(_) => Type::PossibleType(vec![
                Type::Builtin(BuiltinType::Int),
                Type::Builtin(BuiltinType::I8),
                Type::Builtin(BuiltinType::I16),
                Type::Builtin(BuiltinType::I32),
                Type::Builtin(BuiltinType::I64),
                Type::Builtin(BuiltinType::Nat),
                Type::Builtin(BuiltinType::F32),
                Type::Builtin(BuiltinType::F64),
            ]),
            Literal::Char(_) => Type::Builtin(BuiltinType::Char),
            Literal::String(_) => Type::User(PathName::new(vec!["String"], 0, 0)),
            Literal::Unit => Type::Builtin(BuiltinType::Unit),
            Literal::List(_) => {
                todo!("check for overloads of create[]");
            }
        }
    }

    fn check_consts(&mut self, consts: Vec<&core_lang::TopLevelStatement>) -> Result<Vec<TopLevelStatement>, TypeError> {
        consts.iter().map(|x| match x {
            core_lang::TopLevelStatement::Const(x) => {
                let core_lang::Const { visibility, name, ty, value, start, end } = x;
                let core_lang::PathName { segments, start: tstart, end: tend } = name;
                let name = PathName::new(segments.clone(), *tstart, *tend);
                let visibility = Self::convert_visibility(visibility);
                let ty = self.does_type_exist_type(ty)?;
                let value = self.convert_expression_type(value, true)?;

                let value = self.check_expressions_type(value, &ty, true)?;

                self.add_global_type(&segments, Rc::new(RefCell::new(ty.clone())));
                
                Ok(core_annotated::Const::new(visibility, name, ty, value, *start, *end))
            }
            _ => unreachable!(),
        }).collect()
    }

    fn convert_expression_type(&mut self, expr: &core_lang::ExpressionType, rhs: bool) -> Result<ExpressionRaw, TypeError> {
        let core_lang::ExpressionType { expression, variadic } = expr;
        let expr = self.convert_expression(expression, rhs)?;

        if *variadic {
            match expr {
                ExpressionRaw::Type(ty) => {
                    return Ok(ExpressionRaw::Type(Type::Parameterized(Box::new(Type::User(PathName::new(vec!["Array"], 0, 0))), vec![ty])));
                }
                _ => {}
            }
        }

        Ok(expr)
    }

    fn convert_expression(
        &mut self,
        expr: &core_lang::Expression,
        rhs: bool,
    ) -> Result<ExpressionRaw, TypeError> {

        match expr {
            core_lang::Expression::Type(ty) => {
                let ty = self.convert_type(ty)?;
                Ok(ExpressionRaw::Type(ty))
            }
            core_lang::Expression::Literal(lit) => {
                let lit = self.convert_literal(lit)?;
                Ok(ExpressionRaw::Literal(lit))
            }
            core_lang::Expression::Variable(var) => {
                let core_lang::PathName { segments, start, end } = var;
                let segments = segments.clone();
                let name = PathName::new(segments, *start, *end);
                Ok(ExpressionRaw::Variable(name))
            }
            core_lang::Expression::Constant(constant) => {
                let core_lang::PathName { segments, start, end } = constant;
                let segments = segments.clone();
                let name = PathName::new(segments, *start, *end);
                Ok(ExpressionRaw::Constant(name))
            }
            core_lang::Expression::Call(call) => {
                let core_lang::Call { name, type_args, args, start, end } = call;
                let function = self.convert_expression(name, rhs)?;
                let type_args = type_args.iter().map(|x| self.reduce_to_type_expression_type(x)).collect::<Result<Vec<_>, _>>()?;
                let args = args.iter().map(|x| {
                    let core_lang::CallArg { name, value, start, end } = x;
                    let name = name.map(|x| x.to_string());
                    let value = self.convert_expression(value, rhs)?;
                    Ok(CallArg::new(name, Either::Left(value), *start, *end))
                }).collect::<Result<Vec<_>, _>>()?;

                
                Ok(ExpressionRaw::Call(Call::new(Box::new(function), type_args,  args, *start, *end)))
            }
            core_lang::Expression::Return(expr) => {
                let expr = expr.as_ref().map(|x| {
                    match self.convert_expression(&x, rhs) {
                        Ok(x) => Ok(Either::Left(Box::new(x))),
                        Err(e) => Err(e),
                    }
                }).transpose()?;
                Ok(ExpressionRaw::Return(expr))
            }
            core_lang::Expression::Closure(closure) => {
                let core_lang::Closure { params, return_type, body, start, end } = closure;
                self.push_local_scope();
                let params = params.iter().map(|x| {
                    let core_lang::Param { implicit, name, ty, start, end,  } = x;
                    let ty = self.does_type_exist_type(ty)?;
                    self.add_local_type(&vec![name], Rc::new(RefCell::new(ty.clone())));
                    Ok(Param::new(*implicit, name, ty, *start, *end))
                }).collect::<Result<Vec<_>, _>>()?;
                let return_type = return_type.as_ref().map(|x| self.does_type_exist_type(x)).transpose()?;
                let is_return_type_none = return_type.is_none();
                let current_return_type = return_type.clone().unwrap_or_else(|| {
                    Type::Builtin(BuiltinType::Unit)
                });
                let outer_return_type = self.get_return_type();
                self.set_return_type(current_return_type);
                let return_type = return_type.map(Box::new);
                let body = self.check_statements(body)?;
                self.pop_local_scope();
                self.set_return_type(outer_return_type.unwrap());
                let return_type = if is_return_type_none {
                    None
                } else {
                    return_type
                };
                Ok(core_annotated::Closure::new(params, return_type, body, *start, *end))
            }
            core_lang::Expression::Parenthesized(expr) => {
                let expr = self.convert_expression(expr, rhs)?;
                Ok(ExpressionRaw::Parenthesized(Either::Left(Box::new(expr))))
            }
            core_lang::Expression::Tuple(exprs) => {
                let exprs = exprs.iter()
                    .map(|x| self.convert_expression_type(&x, rhs))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ExpressionRaw::Tuple(Either::Left(exprs)))
            }
            core_lang::Expression::IfExpression(if_) => {
                Ok(ExpressionRaw::IfExpression(self.convert_if_expression(if_, rhs)?))
            }
            core_lang::Expression::MatchExpression(match_) => {
                todo!("convert match")
            }
            core_lang::Expression::UnaryOperation { operator, operand, start, end } => {
                let operator = match operator {
                    core_lang::UnaryOperator::Neg => UnaryOperator::Neg,
                    core_lang::UnaryOperator::Not => UnaryOperator::Not,
                    core_lang::UnaryOperator::Try => UnaryOperator::Try,
                };
                let operand = self.convert_expression(operand, rhs)?;
                Ok(ExpressionRaw::UnaryOperation { operator, operand: Box::new(operand), start: *start, end: *end })
            }
            core_lang::Expression::BinaryOperation { operator, left, right, start, end } => {
                let operator = match operator {
                    core_lang::BinaryOperator::Add => BinaryOperator::Add,
                    core_lang::BinaryOperator::Sub => BinaryOperator::Sub,
                    core_lang::BinaryOperator::Mul => BinaryOperator::Mul,
                    core_lang::BinaryOperator::Div => BinaryOperator::Div,
                    core_lang::BinaryOperator::Mod => BinaryOperator::Mod,
                    core_lang::BinaryOperator::And => BinaryOperator::And,
                    core_lang::BinaryOperator::Or => BinaryOperator::Or,
                    core_lang::BinaryOperator::Eq => BinaryOperator::Eq,
                    core_lang::BinaryOperator::Ne => BinaryOperator::Ne,
                    core_lang::BinaryOperator::Lt => BinaryOperator::Lt,
                    core_lang::BinaryOperator::Le => BinaryOperator::Le,
                    core_lang::BinaryOperator::Gt => BinaryOperator::Gt,
                    core_lang::BinaryOperator::Ge => BinaryOperator::Ge,
                    core_lang::BinaryOperator::Concat => BinaryOperator::Concat,
                    core_lang::BinaryOperator::Index => BinaryOperator::Index,
                };

                let left = self.convert_expression(left, rhs)?;
                let right = self.convert_expression(right, rhs)?;

                Ok(ExpressionRaw::BinaryOperation { operator, left: Box::new(left), right: Box::new(right), start: *start, end: *end })
            }
            _ => {
                todo!("remove member access from core_lang")
            }
            
        }
    }

    fn convert_if_expression(
        &mut self,
        if_: &core_lang::IfExpr,
        rhs: bool,
    ) -> Result<IfExpr, TypeError> {
        let core_lang::IfExpr { condition, then_branch, else_branch, start, end } = if_;

        let condition = self.convert_expression(&condition.as_ref(), rhs)?;
        let condition = self.check_expressions_type(condition, &Type::Builtin(BuiltinType::Bool), rhs)?;
        let condition = Box::new(condition);
        let statements = self.check_statements(then_branch)?;
        match else_branch {
            Some(Either::Left(if_)) => {
                let if_ = self.convert_if_expression(if_, rhs)?;
                Ok(IfExpr::new(condition, statements, Some(Either::Left(Box::new(if_))), *start, *end))
            }
            Some(Either::Right(else_)) => {
                let else_ = self.check_statements(else_)?;
                Ok(IfExpr::new(condition, statements, Some(Either::Right(else_)), *start, *end))
            }
            None => {
                Ok(IfExpr::new(condition, statements, None, *start, *end))
            }
        }
    }
    
    fn get_expressions_type(
        &mut self,
        expr: ExpressionRaw,
        rhs: bool,
    ) -> Result<Type, TypeError> {

        match expr {
            ExpressionRaw::Type(ref tty) => {
                return Ok(tty.clone());
            }
            ExpressionRaw::Variable(ref name) => {
                let vty = self.get_type(&name.segments).unwrap();
                let vty = vty.clone();
                let vty = vty.borrow();
                return Ok(vty.clone());
            }
            ExpressionRaw::Constant(ref name) => {
                let vty = self.get_type(&name.segments).unwrap();
                let vty = vty.clone();
                let vty = vty.borrow();
                return Ok(vty.clone());
            }
            ExpressionRaw::Literal(ref lit) => {
                Ok(self.get_literal_type(&lit))
            }
            ExpressionRaw::Call(call) => {
                let core_annotated::Call { name, type_args, args, start: cstart, end: cend } = call;
                let ExpressionRaw::Variable(name) = *name else {
                    unreachable!("Call name must be a variable")
                };

                let mut function_rc = self.get_type(&name.segments);

                if name.segments.len() == 1 {
                    let name = name.segments[0].clone();
                    if let Some(overloads) = self.get_overloads(&name) {
                        for overload in overloads {
                            let function = self.get_type(&overload);
                            if function.is_none() {
                                continue;
                            }
                            let function_rc_inner = function.unwrap();
                            let function = function_rc_inner.borrow();

                            let Type::Builtin(BuiltinType::Function { params, .. }) = &*function else {
                                continue;
                            };

                            if params.len() != args.len() {
                                continue;
                            }

                            function_rc = Some(function_rc_inner.clone());
                        }
                    }
                }
                let function_rc = function_rc.unwrap();
                let function = function_rc.clone();
                let function = function.borrow();
                
                
                let Type::Builtin(BuiltinType::Function { params, return_type }) = &*function else {
                    todo!("report not a function error")
                };

                if params.len() != args.len() {
                    todo!("report wrong number of arguments")
                }

                /*let args = args.into_iter().enumerate().map(|(i, x)| {
                    let core_annotated::CallArg { name, value, start, end } = x;
                    let Either::Left(value) = value else {
                        unreachable!("CallArg value must not be annotated at this time")
                    };
                    let value = self.check_expressions_type(value, &params[i], rhs)?;
                    Ok(CallArg::new(name, Either::Right(value), start, end))
                }).collect::<Result<Vec<_>, _>>()?;*/

                let return_type = return_type.clone();
                Ok(*return_type.clone())
            }
            ExpressionRaw::Return(_) => {
                Ok(Type::Builtin(BuiltinType::Unit))
            }
            ExpressionRaw::Closure(closure) => {
                let core_annotated::Closure { params, return_type, .. } = closure;
                let params = params.iter().map(|x| {
                    let core_annotated::Param { ty, .. } = x;
                    let ty = ty.clone();
                    Ok(ty)
                }).collect::<Result<Vec<_>, _>>()?;
                let return_type = return_type.as_ref().map(|x| x.clone());
                let return_type = return_type.unwrap_or_else(|| Box::new(Type::Builtin(BuiltinType::Unit)));
                Ok(Type::Builtin(BuiltinType::Function { params, return_type }))
            }
            ExpressionRaw::Parenthesized(Either::Left(expr)) => {
                let ty = self.get_expressions_type(*expr, rhs)?;
                Ok(ty)
            }
            ExpressionRaw::Parenthesized(Either::Right(expr)) => unreachable!("Parenthesized should not be annotated at this time"),
            ExpressionRaw::Tuple(Either::Left(exprs)) => {
                let exprs = exprs.into_iter().map(|x| self.get_expressions_type(x, rhs)).collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Tuple(exprs))
            }
            ExpressionRaw::Tuple(Either::Right(exprs)) => unreachable!("Tuple should not be annotated at this time"),
            ExpressionRaw::IfExpression(if_) => {
                self.get_if_expression_type(if_)
            }
            ExpressionRaw::MatchExpression(_) => {
                todo!("implement typechecking for match expressions")
            }
            ExpressionRaw::UnaryOperation { operator, operand, start, end } => {
                let operator = match operator {
                    UnaryOperator::Neg => "-",
                    UnaryOperator::Not => "!",
                    UnaryOperator::Try => "?",
                };
                let operand_ty = self.get_expressions_type(*operand.clone(), true)?;

                let overloads = self.get_overloads(operator);
                if overloads.is_none() {
                    return Err(TypeError::UnaryOperatorNotDefined(operator.to_string(), start, end));
                }
                let overloads = overloads.unwrap();
                for overload in overloads {
                    let function = self.get_type(&overload);
                    if function.is_none() {
                        continue;
                    }
                    let function = function.unwrap();
                    let function = function.borrow();

                    let Type::Builtin(BuiltinType::Function { params, return_type }) = &*function else {
                        continue;
                    };

                    if params.len() == 1 && params[0] == operand_ty {
                        return Ok(*return_type.clone());
                    }
                }

                return Err(TypeError::UnaryOperatorNotDefined(operator.to_string(), start, end));
            }
            ExpressionRaw::BinaryOperation { operator, left, right, start, end } => {
                let operator = match operator {
                    BinaryOperator::Add => "+",
                    BinaryOperator::Sub => "-",
                    BinaryOperator::Mul => "*",
                    BinaryOperator::Div => "/",
                    BinaryOperator::Mod => "%",
                    BinaryOperator::And => "&&",
                    BinaryOperator::Or => "||",
                    BinaryOperator::Eq => "==",
                    BinaryOperator::Ne => "!=",
                    BinaryOperator::Lt => "<",
                    BinaryOperator::Le => "<=",
                    BinaryOperator::Gt => ">",
                    BinaryOperator::Ge => ">=",
                    BinaryOperator::Concat => "++",
                    BinaryOperator::Index => if rhs { "get[]" } else { "set[]" },
                };

                let left_ty = self.get_expressions_type(*left.clone(), true)?;
                let right_ty = self.get_expressions_type(*right.clone(), true)?;

                let overloads = self.get_overloads(operator);
                if overloads.is_none() {
                    return Err(TypeError::BinaryOperatorNotDefined(operator.to_string(), start, end));
                }
                let overloads = overloads.unwrap();

                for overload in overloads {
                    let function = self.get_type(&overload);
                    if function.is_none() {
                        continue;
                    }
                    let function = function.unwrap();
                    let function = function.borrow();

                    let Type::Builtin(BuiltinType::Function { params, return_type }) = &*function else {
                        continue;
                    };

                    if params.len() == 2 && params[0] == left_ty && params[1] == right_ty {
                        return Ok(*return_type.clone());
                    }
                }

                return Err(TypeError::BinaryOperatorNotDefined(operator.to_string(), start, end));
            }

        }
    }

    fn get_if_expression_type(
        &mut self,
        if_: IfExpr
    ) -> Result<Type, TypeError> {
        let IfExpr { then_branch, else_branch, start, end, .. } = if_;

        let then_branch = self.get_statements_type(&then_branch.last().unwrap())?;
        let else_branch = match else_branch {
            Some(Either::Left(if_)) => {
                let else_branch = self.get_if_expression_type(*if_)?;
                Some(Either::Left(Box::new(else_branch)))
            }
            Some(Either::Right(else_)) => {
                let else_branch = self.get_statements_type(&else_.last().unwrap())?;
                Some(Either::Right(else_branch))
            }
            None => None,
        };

        if let Some(Either::Right(else_branch)) = else_branch {
            if then_branch != else_branch {
                return Err(TypeError::TypeMismatch(then_branch, else_branch.clone(), start, end));
            }
        }

        Ok(then_branch)
    }

    fn check_expressions_type(
        &mut self,
        expr: ExpressionRaw,
        ty: &Type,
        rhs: bool

    ) -> Result<Expression, TypeError> {

        match expr {
            ExpressionRaw::Type(ref tty) => {
                if *ty == Type::Builtin(BuiltinType::Type) {
                    return Ok(Expression::new(expr, ty));
                }
                println!("check_expressions_type: {:?} {:?}", ty, tty);
                Err(TypeError::TypeMismatch(ty.clone(), tty.clone(), 0, 0))
            }
            ExpressionRaw::Variable(ref name) => {
                let vty = self.get_type(&name.segments).unwrap();
                let vty = vty.clone();
                let vty = vty.borrow();
                if *vty == *ty {
                    if vty.is_mut() {
                        return Ok(Expression::new(expr, &vty));
                    }
                    return Ok(Expression::new(expr, ty));
                }
                Err(TypeError::TypeMismatch(ty.clone(), vty.clone(), 0, 0))
            }
            ExpressionRaw::Constant(ref name) => {
                let vty = self.get_type(&name.segments).unwrap();
                let vty = vty.clone();
                let vty = vty.borrow();
                if *vty == *ty {
                    if vty.is_mut() {
                        return Ok(Expression::new(expr, &vty));
                    }
                    return Ok(Expression::new(expr, ty));
                }
                Err(TypeError::TypeMismatch(ty.clone(), vty.clone(), 0, 0))
            }
            ExpressionRaw::Literal(ref lit) => {
                let lit_ty = self.get_literal_type(&lit);
                match &lit_ty {
                    Type::PossibleType(types) => {
                        if types.contains(&ty) {
                            return Ok(Expression::new(expr, ty));
                        }
                    }
                    _ => {
                        if lit_ty == *ty {
                            if lit_ty.is_mut() {
                                return Ok(Expression::new(expr, &lit_ty));
                            }
                            return Ok(Expression::new(expr, ty));
                        }
                    }
                }
                Err(TypeError::TypeMismatch(ty.clone(), lit_ty, 0, 0))
            }
            ExpressionRaw::Call(call) => {
                let core_annotated::Call { name, type_args, args, start: cstart, end: cend } = call;
                let ExpressionRaw::Variable(name) = *name else {
                    unreachable!("Call name must be a variable")
                };

                let function = self.get_type(&name.segments);
                let function = if let Some(function) = function {
                    function
                } else {
                    let overloads = self.get_overloads(&name.segments.last().unwrap());
                    if overloads.is_none() {
                        return Err(TypeError::FunctionDoesNotExist(name.segments.clone(), cstart, cend));
                    }
                    let overloads = overloads.unwrap().clone();
                    for overload in overloads {
                        let function = self.get_type(&overload);
                        if function.is_none() {
                            continue;
                        }
                        let function = function.unwrap();
                        let function = function.borrow();

                        let Type::Builtin(BuiltinType::Function { params, return_type }) = &*function else {
                            continue;
                        };


                        if params.len() == args.len() {


                            let args = args.clone().into_iter().enumerate().map(|(i, x)| {
                                let core_annotated::CallArg { name, value, start, end } = x;
                                let Either::Left(value) = value else {
                                    unreachable!("CallArg value must not be annotated at this time")
                                };
                                let backup = self.get_param_type();
                                self.set_param_type(params[i].clone());
                                let value = self.check_expressions_type(value, &params[i], rhs)?;
                                if let Some(backup) = backup {
                                    self.set_param_type(backup);
                                } else {
                                    self.unset_param_type();
                                }
                                Ok(CallArg::new(name, Either::Right(value), start, end))
                            }).collect::<Result<Vec<_>, TypeError>>();

                            if args.is_err() {
                                continue;
                            }
                            let args = args.unwrap();
                            let name = PathName { segments: overload.clone(), start: cstart, end: cend };

                            let expression = ExpressionRaw::Call(Call::new(
                                Box::new(ExpressionRaw::Variable(name)),
                                type_args,
                                args,
                                cstart,
                                cend
                            ));
                            if *return_type.as_ref() == *ty {
                                return Ok(Expression::new(expression, return_type.as_ref()));
                            }
                            return Err(TypeError::TypeMismatch(ty.clone(), return_type.as_ref().clone(), cstart, cend));
                        }
                    }
                    return Err(TypeError::FunctionDoesNotExist(name.segments.clone(), cstart, cend));
                };
                let function = function.clone();
                let function = function.borrow();
                let Type::Builtin(BuiltinType::Function { params, return_type }) = &*function else {
                    todo!("report not a function error")
                };

                if params.len() != args.len() {
                    todo!("report wrong number of arguments")
                }

                let args = args.into_iter().enumerate().map(|(i, x)| {
                    let core_annotated::CallArg { name, value, start, end } = x;
                    let Either::Left(value) = value else {
                        unreachable!("CallArg value must not be annotated at this time")
                    };
                    let backup = self.get_param_type();
                    self.set_param_type(params[i].clone());
                    let value = self.check_expressions_type(value, &params[i], rhs)?;
                    if let Some(backup) = backup {
                        self.set_param_type(backup);
                    } else {
                        self.unset_param_type();
                    }
                    Ok(CallArg::new(name, Either::Right(value), start, end))
                }).collect::<Result<Vec<_>, _>>()?;

                let return_type = return_type.clone();
                Ok(Expression::new(
                    ExpressionRaw::Call(
                        Call::new(
                            Box::new(ExpressionRaw::Variable(name)),
                            type_args,
                            args,
                            cstart,
                            cend)), &return_type))
            }
            ExpressionRaw::Return(expr) => {
                let Some(Either::Left(raw)) = expr else {
                    unreachable!("Return must have an expression")
                };
                let Some(return_type) = self.get_return_type() else {
                    unreachable!("Return must be in a function");
                };
                let expr = self.check_expressions_type(*raw, &return_type, rhs)?;

                Ok(Expression::new(ExpressionRaw::Return(Some(Either::Right(Box::new(expr)))), ty))
            }
            ExpressionRaw::Closure(closure) => {
                let current_param_type = self.get_param_type();

                let current_param_type = current_param_type.unwrap_or_else(|| {
                    Type::Builtin(BuiltinType::Unit)
                });
                
                let core_annotated::Closure { params, return_type, body, start, end } = closure;
                let params = params.iter().map(|x| {
                    let core_annotated::Param { implicit, name, ty, start, end } = x;
                    let ty = ty.clone();
                    Ok(Param::new(*implicit, name, ty, *start, *end))
                }).collect::<Result<Vec<_>, _>>()?;

                let return_type = if return_type.is_none() {
                    let Type::Builtin(BuiltinType::Function { params: _, return_type }) = &current_param_type else {
                        todo!("report not a function error")
                    };
                    return_type.clone()
                } else {
                    return_type.as_ref().map(|x| x.clone()).unwrap()
                };
                let outer_return_type = self.get_return_type();
                self.set_return_type(*return_type.clone());
                let body = self.statements_add_tail_return(body)?;
                self.set_return_type(outer_return_type.unwrap());
                
                let return_type = Some(return_type);
                Ok(Expression::new(core_annotated::Closure::new(params.clone(), return_type.clone(), body, start, end), &Type::Builtin(BuiltinType::Function { params: params.iter().map(|x| x.ty.clone()).collect(), return_type: return_type.unwrap() })))
                
                
            }
            ExpressionRaw::Parenthesized(Either::Left(expr)) => {
                let expr = self.check_expressions_type(*expr, ty, rhs)?;
                Ok(Expression::new(ExpressionRaw::Parenthesized(Either::Right(Box::new(expr))), ty))
            }
            ExpressionRaw::Parenthesized(Either::Right(_)) => unreachable!("Parenthesized should not be annotated at this time"),
            ExpressionRaw::Tuple(Either::Left(exprs)) => {
                
                let exprs = exprs.into_iter().map(|x| {
                    let ty = self.get_expressions_type(x.clone(), rhs)?;
                    self.check_expressions_type(x.clone(), &ty, rhs)
                }).collect::<Result<Vec<_>, _>>()?;
                Ok(Expression::new(ExpressionRaw::Tuple(Either::Right(exprs)), ty))
            }
            ExpressionRaw::Tuple(Either::Right(_)) => unreachable!("Tuple should not be annotated at this time"),
            ExpressionRaw::IfExpression(if_) => {
                let (if_, out_ty) = self.check_if_expression_type(if_, ty)?;
                Ok(Expression::new(ExpressionRaw::IfExpression(if_), &out_ty))
            }
            ExpressionRaw::MatchExpression(_) => {
                todo!("implement typechecking for match expressions")
            }
            ExpressionRaw::UnaryOperation { operator, operand, start, end } => {
                let operator = match operator {
                    UnaryOperator::Neg => "-",
                    UnaryOperator::Not => "!",
                    UnaryOperator::Try => "?",
                };
                let operand_ty = self.get_expressions_type(*operand.clone(), true)?;

                let overloads = self.get_overloads(operator);
                if overloads.is_none() {
                    return Err(TypeError::UnaryOperatorNotDefined(operator.to_string(), start, end));
                }
                let overloads = overloads.unwrap();
                let mut operand_type = None;
                let mut overloaded_function = None;
                for overload in overloads {
                    let function = self.get_type(&overload);
                    if function.is_none() {
                        continue;
                    }
                    let function = function.unwrap();
                    let function = function.borrow();

                    let Type::Builtin(BuiltinType::Function { params, return_type }) = &*function else {
                        continue;
                    };

                    if params.len() == 1 && params[0] == operand_ty {
                        operand_type = Some(return_type.clone());
                        overloaded_function = Some(overload.clone());
                        if *return_type.as_ref() == *ty {
                            break;
                        }
                    }
                }
                if let Some(operand_type) = operand_type {
                    let Some(overload) = overloaded_function else {
                        unreachable!("You forgot to set the overloaded function")
                    };
                    let operand = self.check_expressions_type(*operand, &operand_type, true)?;
                    let expression = ExpressionRaw::Call(Call::new(
                        Box::new(ExpressionRaw::Variable(PathName { segments: overload.clone(), start: 0, end: 0 })),
                        vec![],
                        vec![CallArg::new(None, Either::Right(operand), 0, 0)],
                        start,
                        end
                    ));
                    return Ok(Expression::new(expression, operand_type.as_ref()));
                }

                return Err(TypeError::UnaryOperatorNotDefined(operator.to_string(), start, end));
            }
            ExpressionRaw::BinaryOperation { operator, left, right, start, end } => {
                let operator = match operator {
                    BinaryOperator::Add => "+",
                    BinaryOperator::Sub => "-",
                    BinaryOperator::Mul => "*",
                    BinaryOperator::Div => "/",
                    BinaryOperator::Mod => "%",
                    BinaryOperator::And => "&&",
                    BinaryOperator::Or => "||",
                    BinaryOperator::Eq => "==",
                    BinaryOperator::Ne => "!=",
                    BinaryOperator::Lt => "<",
                    BinaryOperator::Le => "<=",
                    BinaryOperator::Gt => ">",
                    BinaryOperator::Ge => ">=",
                    BinaryOperator::Concat => "++",
                    BinaryOperator::Index => if rhs { "get[]" } else { "set[]" },
                };

                let left_ty = self.get_expressions_type(*left.clone(), true)?;
                let right_ty = self.get_expressions_type(*right.clone(), true)?;

                let overloads = self.get_overloads(operator);
                if overloads.is_none() {
                    return Err(TypeError::BinaryOperatorNotDefined(operator.to_string(), start, end));
                }
                let overloads = overloads.unwrap();

                let mut left_type = None;
                let mut right_type = None;
                let mut overloaded_function = None;

                for overload in overloads {
                    let function = self.get_type(&overload);
                    if function.is_none() {
                        continue;
                    }
                    let function = function.unwrap();
                    let function = function.borrow();

                    let Type::Builtin(BuiltinType::Function { params, return_type }) = &*function else {
                        continue;
                    };

                    if params.len() == 2 && params[0] == left_ty && params[1] == right_ty {
                        left_type = Some(params[0].clone());
                        right_type = Some(params[1].clone());
                        overloaded_function = Some(overload.clone());
                        if *return_type.as_ref() == *ty {
                            break;
                        }
                    }
                }

                if let Some(overloaded_function) = overloaded_function {
                    let left_ty = left_type.expect("You forgot to set the left type");
                    let right_ty = right_type.expect("You forgot to set the right type");
                    let left = self.check_expressions_type(*left, &left_ty, true)?;
                    let right = self.check_expressions_type(*right, &right_ty, true)?;
                    let expression = ExpressionRaw::Call(Call::new(
                        Box::new(ExpressionRaw::Variable(PathName { segments: overloaded_function, start: 0, end: 0 })),
                        vec![],
                        vec![
                            CallArg::new(None, Either::Right(left), 0, 0),
                            CallArg::new(None, Either::Right(right), 0, 0),
                        ],
                        start,
                        end
                    ));
                    return Ok(Expression::new(expression, &ty));
                }

                return Err(TypeError::BinaryOperatorNotDefined(operator.to_string(), start, end));
            }

        }
    }

    fn check_if_expression_type(
        &mut self,
        if_: IfExpr,
        ty: &Type
    ) -> Result<(IfExpr, Type), TypeError> {
        let IfExpr { condition, then_branch, else_branch, start, end, .. } = if_;

        let (else_branch, else_type) = match else_branch {
            Some(Either::Left(if_)) => {
                let (else_branch, else_type) = self.check_if_expression_type(*if_, ty)?;
                (Some(Either::Left(Box::new(else_branch))), else_type)
            }
            Some(Either::Right(else_)) => {
                let (else_, else_type) = self.check_statements_type(else_, ty)?;
                (Some(Either::Right(else_)), else_type)
            }
            None => (None, Type::Builtin(BuiltinType::Unit)),
        };

        let (then_branch, then_type) = self.check_statements_type(then_branch, ty)?;


        if else_type == *ty && then_type == *ty {
            return Ok((IfExpr::new(condition, then_branch, else_branch, start, end), ty.clone()));
        }
        Err(TypeError::TypeMismatch(ty.clone(), then_type, start, end))
    }

    /// This function checks if the last statement's type matches the expected type
    fn check_statements_type(
        &mut self,
        statements: Vec<core_annotated::Statement>,
        ty: &Type
    ) -> Result<(Vec<core_annotated::Statement>, Type), TypeError> {
        let mut output = Vec::new();
        let statements_len = statements.len();
        for (i, statement) in statements.into_iter().enumerate() {
            if i == statements_len - 1 {
                let statement = self.check_statement_type(&statement, ty)?;
                output.push(statement);

                return Ok((output, ty.clone()));
            }
            output.push(statement);
        }

        unreachable!("The last statement should have been returned")
    }

    fn check_statement_type(
        &mut self,
        statement: &core_annotated::Statement,
        ty: &Type
    ) -> Result<core_annotated::Statement, TypeError> {
        match statement {
            core_annotated::Statement::Expression(expr) => {
                let Expression { ty: e_ty, raw } = self.check_expressions_type(expr.raw.clone(), ty, true)?;
                if *ty == *e_ty.borrow() {
                    let statement = core_annotated::Statement::Expression(Expression::new(raw.clone(), ty));
                    return Ok(statement);
                } else {
                    return Err(TypeError::TypeMismatch(ty.clone(), e_ty.borrow().clone(), 0, 0));
                }
            }
            core_annotated::Statement::Assignment { .. } => {
                if ty == &Type::Builtin(BuiltinType::Unit) {
                    return Ok(statement.clone());
                }
            }
            core_annotated::Statement::Let { .. } => {
                if ty == &Type::Builtin(BuiltinType::Unit) {
                    return Ok(statement.clone());
                }
            }
        }
        Err(TypeError::TypeMismatch(ty.clone(), Type::Builtin(BuiltinType::Unit), 0, 0))
        
    }
            

    fn check_functions(&mut self, functions: Vec<&core_lang::TopLevelStatement>) -> Result<Vec<TopLevelStatement>, TypeError> {
        let mut result = Vec::new();

        for function in functions {
            let core_lang::TopLevelStatement::Function(function) = function else {
                unreachable!("Encountered a non-function after filtering only functions")
            };
            result.push(self.check_function(function)?);
        }

        Ok(result)
    }

    fn check_function(&mut self, function: &core_lang::Function) -> Result<TopLevelStatement, TypeError> {
        match function {
            core_lang::Function::Regular {
                visibility,
                name,
                generic_params,
                params,
                return_type,
                body,
                start,
                end
            } => {
                let visibility = Self::convert_visibility(&visibility);
                let core_lang::PathName { segments, start: nstart, end: nend } = name;
                self.push_local_scope();

                if name.segments.len() > 1 {
                    self.add_overload(segments.last().unwrap(), &segments);
                }

                let name = PathName::new(segments.clone(), *nstart, *nend);
                let pairs = generic_params
                    .iter()
                    .map(|x| {
                        let (p, (names, ty)) = self.convert_generic_parameter(x)?;
                        for name in names {
                            self.add_local_type(&vec![name], Rc::new(RefCell::new(ty.clone())));
                        }
                        Ok((p, ty))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let mut generic_params = Vec::new();
                let mut generic_args = Vec::new();
                for (param, ty) in pairs {
                    generic_params.push(param);
                    generic_args.push(ty);
                }

                let params = params.iter().map(|x| {
                    let core_lang::Param { implicit, name, ty, start, end } = x;
                    let ty = self.does_type_exist_type(ty)?;
                    self.add_local_type(&vec![name], Rc::new(RefCell::new(ty.clone())));
                    Ok(core_annotated::Param::new(*implicit, name, ty, *start, *end))
                }).collect::<Result<Vec<_>, _>>()?;

                let return_type = self.does_type_exist_type(&return_type)?;

                self.set_return_type(return_type.clone());

                self.add_global_type(&segments, Rc::new(RefCell::new(
                    create_function_type(
                        params.clone(),
                        return_type.clone(),
                    )
                )));
                

                let body = self.check_statements(body)?;

                self.pop_local_scope();
                self.unset_return_type();

                Ok(core_annotated::Function::new(
                    visibility,
                    name,
                    generic_params,
                    params,
                    return_type,
                    body,
                    *start,
                    *end,
                ))
            }
            core_lang::Function::Extern {
                visibility,
                language,
                name,
                generic_params,
                params,
                return_type,
                body,
                start,
                end
            } => {
                let visibility = Self::convert_visibility(&visibility);
                let core_lang::PathName { segments, start: nstart, end: nend } = name;
                self.push_local_scope();

                if name.segments.len() > 1 {
                    self.add_overload(segments.last().unwrap(), &segments);
                }

                let name = PathName::new(segments.clone(), *nstart, *nend);
                let pairs = generic_params
                    .iter()
                    .map(|x| {
                        let (p, (names, ty)) = self.convert_generic_parameter(x)?;
                        for name in names {
                            self.add_local_type(&vec![name], Rc::new(RefCell::new(ty.clone())));
                        }
                        Ok((p, ty))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let mut generic_params = Vec::new();
                let mut generic_args = Vec::new();
                for (param, ty) in pairs {
                    generic_params.push(param);
                    generic_args.push(ty);
                }

                let params = params.iter().map(|x| {
                    let core_lang::Param { implicit, name, ty, start, end } = x;
                    let ty = self.does_type_exist_type(ty)?;
                    Ok(core_annotated::Param::new(*implicit, name, ty, *start, *end))
                }).collect::<Result<Vec<_>, _>>()?;

                let return_type = self.does_type_exist_type(&return_type)?;

                self.add_global_type(&segments, Rc::new(RefCell::new(
                    create_function_type(
                        params.clone(),
                        return_type.clone(),
                    )
                )));

                self.pop_local_scope();
                Ok(core_annotated::Function::new_extern(
                    visibility,
                    language,
                    name,
                    generic_params,
                    params,
                    return_type,
                    body,
                    *start,
                    *end,
                ))
            }
        }
    }

    fn check_statements(&mut self, statements: &Vec<core_lang::Statement>) -> Result<Vec<core_annotated::Statement>, TypeError> {
        let mut output = Vec::new();

        for (i, statement) in statements.iter().enumerate() {
            if i == statements.len() - 1 {
                output.push(self.check_statement(statement, true)?);

                return Ok(output);
            }
            output.push(self.check_statement(statement, false)?);
        }

        Ok(output)
    }

    fn check_statement(&mut self, statement: &core_lang::Statement, tail: bool) -> Result<core_annotated::Statement, TypeError> {
        match statement {
            core_lang::Statement::Assignment { target, value, start, end } => {
                let target = self.convert_expression_type(target, true)?;
                let value = self.convert_expression_type(value, true)?;

                let target_type = self.get_expressions_type(target.clone(), true)?;
                let value_type = self.get_expressions_type(value.clone(), true)?;

                let target = self.check_expressions_type(target, &value_type, false)?;
                let value = self.check_expressions_type(value, &target_type, true)?;

                Ok(core_annotated::Statement::new_assignment(target, value, *start, *end))
            }
            core_lang::Statement::Expression(expr) => {
                let expr = self.convert_expression_type(&expr, true)?;
                let expr_type = self.get_expressions_type(expr.clone(), true)?;

                let expr = self.check_expressions_type(expr, &expr_type, true)?;

                if tail {
                    let Expression { raw, ty } = expr;

                    match &raw {
                        ExpressionRaw::Return(_) => {
                            return Ok(core_annotated::Statement::Expression(Expression { raw, ty }));
                        }
                        _ => {} 
                    }

                    let current_return_type = self.get_return_type().unwrap();

                    if current_return_type == *ty.borrow() && current_return_type != Type::Builtin(BuiltinType::Unit) {
                        let raw = ExpressionRaw::Return(Some(Either::Right(Box::new(Expression { raw, ty }))));
                        return Ok(core_annotated::Statement::Expression(Expression::new(raw, &Type::Builtin(BuiltinType::Unit))));
                    } else if current_return_type == Type::Builtin(BuiltinType::Unit) {
                        return Ok(core_annotated::Statement::Expression(Expression { raw, ty }));
                    } else {
                        return Err(TypeError::TypeMismatch(current_return_type, ty.borrow().clone(), 0, 0));
                    }
                }
                
                Ok(core_annotated::Statement::Expression(expr))
            }
            core_lang::Statement::Let { name, ty, value, start, end } => {

                let ty = self.does_type_exist_type(&ty)?;
                let pattern = self.convert_pattern(&name)?;

                let pattern = self.check_pattern_types(pattern, ty.clone())?;
                
                let value = self.convert_expression_type(&value, true)?;

                let value = self.check_expressions_type(value, &ty, true)?;

                Ok(core_annotated::Statement::new_let(pattern, ty, value, *start, *end))
            }
        }
    }

    fn check_pattern_types(&mut self, pattern: core_annotated::Pattern, ty: Type) -> Result<core_annotated::Pattern, TypeError> {
        match pattern {
            core_annotated::Pattern::Variable(name) => {
                self.add_local_type(&vec![&name], Rc::new(RefCell::new(ty.clone())));
                Ok(core_annotated::Pattern::Variable(name))
            }
            core_annotated::Pattern::Tuple(members) => {
                let mut output = Vec::new();

                let Type::Tuple(tuple_types) = ty else {
                    todo!("Report type not being a tuple")
                };

                for (member, ty) in members.into_iter().zip(tuple_types.into_iter()) {
                    output.push(self.check_pattern_types(member, ty)?);
                }

                Ok(core_annotated::Pattern::Tuple(output))
            }
            core_annotated::Pattern::Wildcard => {
                Ok(core_annotated::Pattern::Wildcard)
            }
            core_annotated::Pattern::Constructor { name, fields } => {
                todo!("Lookup constructor via name and match on fields")
            }
            _ => {
                todo!("error on pattern match due to literal")
            }
        }
    }

    fn get_statements_type(&mut self, statement: &core_annotated::Statement) -> Result<Type, TypeError> {
        match statement {
            core_annotated::Statement::Assignment { .. } => {
                Ok(Type::Builtin(BuiltinType::Unit))
            }
            core_annotated::Statement::Expression(expr) => {
                let Expression { raw, .. } = expr;
                self.get_expressions_type(raw.clone(), true)
            }
            core_annotated::Statement::Let { .. } => {
                Ok(Type::Builtin(BuiltinType::Unit))
            }

        }

    }

    fn statements_add_tail_return(&mut self, statements: Vec<core_annotated::Statement>) -> Result<Vec<core_annotated::Statement>, TypeError> {
        let mut output = Vec::new();

        let statements_len = statements.len();

        for (i, statement) in statements.into_iter().enumerate() {
            if i == statements_len - 1 {
                output.push(self.statement_add_tail_return(statement, true)?);

                return Ok(output);
            }
            output.push(self.statement_add_tail_return(statement, false)?);
        }

        Ok(output)
    }

    fn statement_add_tail_return(&mut self, statement: core_annotated::Statement, tail: bool) -> Result<core_annotated::Statement, TypeError> {
        match statement {
            core_annotated::Statement::Expression(expr) => {
                if tail {
                    let Expression { raw, ty } = expr;

                    match &raw {
                        ExpressionRaw::Return(_) => {
                            return Ok(core_annotated::Statement::Expression(Expression { raw, ty }));
                        }
                        _ => {} 
                    }

                    let current_return_type = self.get_return_type().unwrap();

                    if current_return_type == *ty.borrow() && current_return_type != Type::Builtin(BuiltinType::Unit) {
                        let raw = ExpressionRaw::Return(Some(Either::Right(Box::new(Expression { raw, ty }))));
                        return Ok(core_annotated::Statement::Expression(Expression::new(raw, &Type::Builtin(BuiltinType::Unit))));
                    } else if current_return_type == Type::Builtin(BuiltinType::Unit) {
                        return Ok(core_annotated::Statement::Expression(Expression { raw, ty }));
                    } else {
                        return Err(TypeError::TypeMismatch(current_return_type, ty.borrow().clone(), 0, 0));
                    }
                }
                
                Ok(core_annotated::Statement::Expression(expr))
            }
            x => Ok(x)
        }
    }
}
