use petgraph::algo;
use petgraph::graph::UnGraph;
use sb_ast::core_annotated::{self, BuiltinType, Call, CallArg, Enum, Expression, ExpressionRaw, Field, Function, IfExpr, Import, Literal, Param, PathName, TopLevelStatement, Type};
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

pub struct TypeChecker<'a> {
    overloads: HashMap<&'a str, Vec<&'a Vec<String>>>,
    global_types: HashMap<Vec<String>, Rc<RefCell<Type>>>,
    local_types: Vec<HashMap<Vec<String>, Rc<RefCell<Type>>>>,
    seen_files: HashSet<String>,
    current_return_type: Option<Type>,
}

impl <'a> TypeChecker<'a> {
    pub fn new() -> Self {
        Self {
            overloads: HashMap::new(),
            global_types: generate_internal_globals(),
            local_types: Vec::new(),
            seen_files: HashSet::new(),
            current_return_type: None,
        }
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

    pub fn get_type<S>(&self, name: &Vec<S>) -> Option<Rc<RefCell<Type>>>
    where S: AsRef<str> {
        let name: Vec<String> = name.into_iter().map(|x| x.as_ref().to_string()).collect();
        if let Some(scope) = self.local_types.last() {
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

        let pattern = self.convert_pattern(name)?;
        let bound_names = pattern.get_bound_names();
        
        let param = core_annotated::GenericParam::new(pattern, Some(ty.clone()), *start, *end);
        
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
                let tuple = tuple.iter().map(|x| self.convert_expression_type(x)).collect::<Result<Vec<_>, _>>()?;

                let tuple = tuple.into_iter().map(|x| {
                    match x {
                        ExpressionRaw::Type(ty) => Ok(ty),
                        _ => Err(TypeError::NotAType),
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
                let l = l.iter().map(|x| self.convert_expression(x)).collect::<Result<Vec<_>, _>>()?;
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
                let value = self.convert_expression_type(value)?;

                let value = self.check_expressions_type(value, &ty)?;

                self.add_global_type(&segments, Rc::new(RefCell::new(ty.clone())));
                
                Ok(core_annotated::Const::new(visibility, name, ty, value, *start, *end))
            }
            _ => unreachable!(),
        }).collect()
    }

    fn convert_expression_type(&mut self, expr: &core_lang::ExpressionType) -> Result<ExpressionRaw, TypeError> {
        let core_lang::ExpressionType { expression, variadic } = expr;
        let expr = self.convert_expression(expression)?;

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
        expr: &core_lang::Expression
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
                let function = self.convert_expression(name)?;
                let type_args = type_args.iter().map(|x| self.reduce_to_type_expression_type(x)).collect::<Result<Vec<_>, _>>()?;
                let args = args.iter().map(|x| {
                    let core_lang::CallArg { name, value, start, end } = x;
                    let name = name.map(|x| x.to_string());
                    let value = self.convert_expression(value)?;
                    Ok(CallArg::new(name, Either::Left(value), *start, *end))
                }).collect::<Result<Vec<_>, _>>()?;
                Ok(ExpressionRaw::Call(Call::new(Box::new(function), type_args,  args, *start, *end)))
            }
            core_lang::Expression::Return(expr) => {
                let expr = expr.as_ref().map(|x| {
                    match self.convert_expression(&x) {
                        Ok(x) => Ok(Either::Left(Box::new(x))),
                        Err(e) => Err(e),
                    }
                }).transpose()?;
                Ok(ExpressionRaw::Return(expr))
            }
            core_lang::Expression::Closure(closure) => {
                todo!("convert closures")
            }
            core_lang::Expression::Parenthesized(expr) => {
                let expr = self.convert_expression(expr)?;
                Ok(ExpressionRaw::Parenthesized(Either::Left(Box::new(expr))))
            }
            core_lang::Expression::Tuple(exprs) => {
                let exprs = exprs.iter()
                    .map(|x| self.convert_expression_type(&x))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ExpressionRaw::Tuple(Either::Left(exprs)))
            }
            core_lang::Expression::IfExpression(if_) => {
                Ok(ExpressionRaw::IfExpression(self.convert_if_expression(if_)?))
            }
            core_lang::Expression::MatchExpression(match_) => {
                todo!("convert match")
            }
            core_lang::Expression::UnaryOperation { operator, operand, start, end } => {
                todo!("convert unary operation")
            }
            core_lang::Expression::BinaryOperation { operator, left, right, start, end } => {
                todo!("convert binary operation")
            }
            _ => {
                todo!("remove member access from core_lang")
            }
            
        }
    }

    fn convert_if_expression(
        &mut self,
        if_: &core_lang::IfExpr
    ) -> Result<IfExpr, TypeError> {
        let core_lang::IfExpr { condition, then_branch, else_branch, start, end } = if_;

        let condition = self.convert_expression(&condition.as_ref())?;
        let condition = self.check_expressions_type(condition, &Type::Builtin(BuiltinType::Bool))?;
        let condition = Box::new(condition);
        let statements = self.check_statements(then_branch)?;
        match else_branch {
            Some(Either::Left(if_)) => {
                let if_ = self.convert_if_expression(if_)?;
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

                let function = self.get_type(&name.segments).unwrap();
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
                    let value = self.check_expressions_type(value, &params[i])?;
                    Ok(CallArg::new(name, Either::Right(value), start, end))
                }).collect::<Result<Vec<_>, _>>()?;

                let return_type = return_type.clone();
                Ok(*return_type.clone())
            }
            ExpressionRaw::Return(expr) => {
                todo!("implement way to knoww what the return type is")
            }
            ExpressionRaw::Closure(closure) => {
                todo!("implement typechecking for closures")
            }
            ExpressionRaw::Parenthesized(Either::Left(expr)) => {
                let ty = self.get_expressions_type(*expr)?;
                Ok(ty)
            }
            ExpressionRaw::Parenthesized(Either::Right(expr)) => unreachable!("Parenthesized should not be annotated at this time"),
            ExpressionRaw::Tuple(Either::Left(exprs)) => {
                let exprs = exprs.into_iter().map(|x| self.get_expressions_type(x)).collect::<Result<Vec<_>, _>>()?;
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
                todo!("implement typechecking for unary operations")
            }
            ExpressionRaw::BinaryOperation { operator, left, right, start, end } => {
                todo!("implement typechecking for binary operations")
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
        ty: &Type
    ) -> Result<Expression, TypeError> {

        match expr {
            ExpressionRaw::Type(ref tty) => {
                if *ty == Type::Builtin(BuiltinType::Type) {
                    return Ok(Expression::new(expr, ty));
                }
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

                let function = self.get_type(&name.segments).unwrap();
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
                    let value = self.check_expressions_type(value, &params[i])?;
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
                todo!("implement way to knoww what the return type is")
            }
            ExpressionRaw::Closure(closure) => {
                todo!("implement typechecking for closures")
            }
            ExpressionRaw::Parenthesized(Either::Left(expr)) => {
                let expr = self.check_expressions_type(*expr, ty)?;
                Ok(Expression::new(ExpressionRaw::Parenthesized(Either::Right(Box::new(expr))), ty))
            }
            ExpressionRaw::Parenthesized(Either::Right(expr)) => unreachable!("Parenthesized should not be annotated at this time"),
            ExpressionRaw::Tuple(Either::Left(exprs)) => {
                let exprs = exprs.into_iter().map(|x| self.check_expressions_type(x, &Type::Builtin(BuiltinType::Type))).collect::<Result<Vec<_>, _>>()?;
                Ok(Expression::new(ExpressionRaw::Tuple(Either::Right(exprs)), ty))
            }
            ExpressionRaw::Tuple(Either::Right(exprs)) => unreachable!("Tuple should not be annotated at this time"),
            ExpressionRaw::IfExpression(if_) => {
                let (if_, out_ty) = self.check_if_expression_type(if_, ty)?;
                Ok(Expression::new(ExpressionRaw::IfExpression(if_), &out_ty))
            }
            ExpressionRaw::MatchExpression(_) => {
                todo!("implement typechecking for match expressions")
            }
            ExpressionRaw::UnaryOperation { operator, operand, start, end } => {
                todo!("implement typechecking for unary operations")
            }
            ExpressionRaw::BinaryOperation { operator, left, right, start, end } => {
                todo!("implement typechecking for binary operations")
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
            println!("then_type: {:?}, else_type: {:?}", then_type, else_type);
            println!("ty: {:?}", ty);
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
                let Expression { ty: e_ty, raw } = self.check_expressions_type(expr.raw.clone(), ty)?;
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
                self.push_local_scope();

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
        statements.into_iter().map(|x| self.check_statement(x)).collect()
    }

    fn check_statement(&mut self, statement: &core_lang::Statement) -> Result<core_annotated::Statement, TypeError> {
        match statement {
            core_lang::Statement::Assignment { target, value, start, end } => {
                let target = self.convert_expression_type(target)?;
                let value = self.convert_expression_type(value)?;

                let target_type = self.get_expressions_type(target.clone())?;
                let value_type = self.get_expressions_type(value.clone())?;

                let target = self.check_expressions_type(target, &value_type)?;
                let value = self.check_expressions_type(value, &target_type)?;

                Ok(core_annotated::Statement::new_assignment(target, value, *start, *end))
            }
            core_lang::Statement::Expression(expr) => {
                let expr = self.convert_expression_type(&expr)?;
                let expr_type = self.get_expressions_type(expr.clone())?;

                let expr = self.check_expressions_type(expr, &expr_type)?;
                Ok(core_annotated::Statement::Expression(expr))
            }
            core_lang::Statement::Let { name, ty, value, start, end } => {

                let ty = self.does_type_exist_type(&ty)?;
                let value = self.convert_expression_type(&value)?;

                let value = self.check_expressions_type(value, &ty)?;

                let pattern = self.convert_pattern(&name)?;

                // TODO: destructure expression and check that the types match
                let bound_names = pattern.get_bound_names();

                for name in bound_names {
                    self.add_local_type(&vec![name], Rc::new(RefCell::new(ty.clone())));
                }

                Ok(core_annotated::Statement::new_let(pattern, ty, value, *start, *end))
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
                self.get_expressions_type(raw.clone())
            }
            core_annotated::Statement::Let { .. } => {
                Ok(Type::Builtin(BuiltinType::Unit))
            }

        }

    }
}
