use petgraph::adj::NodeIndex;
use petgraph::algo;
use petgraph::graph::UnGraph;
use sb_ast::core_annotated::{self, BuiltinType, Enum, Expression, Import, PathName, TopLevelStatement, Type};
use sb_ast::core_lang;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;


pub enum TypeError {

}

fn generate_internal_globals() -> HashMap<Vec<String>, Rc<RefCell<Type>>> {
    let mut globals = HashMap::new();
    globals.insert(vec!["nat", "Zero"].into_iter().map(|x| x.to_string()).collect(),
                   Rc::new(RefCell::new(Expression::Type(
                           Type::Builtin(BuiltinType::Function {
                               params: vec![],
                               return_type: Box::new(
                                       Expression::Type(Type::Builtin(BuiltinType::Nat))),
                           }),
                       ))));
    globals.insert(vec!["nat", "Succ"].into_iter().map(|x| x.to_string()).collect(),
                   Rc::new(RefCell::new(ExpressionType::new(
                       Expression::Type(
                           Type::Builtin(BuiltinType::Function {
                               params: vec![
                                       Expression::Type(Type::Builtin(BuiltinType::Nat))
                               ],
                               return_type: Box::new(
                                       Expression::Type(Type::Builtin(BuiltinType::Nat)))
                           }),
                       )))));



    globals
}


pub struct TypeChecker<'a> {
    overloads: HashMap<&'a str, Vec<&'a Vec<String>>>,
    global_types: HashMap<Vec<String>, Rc<RefCell<Type>>>,
    local_types: Vec<HashMap<Vec<String>, Rc<RefCell<Type>>>>,
    seen_files: HashSet<String>,
}

impl <'a> TypeChecker<'a> {
    pub fn new() -> Self {
        Self {
            overloads: HashMap::new(),
            global_types: generate_internal_globals(),
            local_types: Vec::new(),
            seen_files: HashSet::new(),
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

    pub fn get_type<S>(&self, name: &Vec<S>) -> Option<Rc<RefCell<Type>>>
    where S: AsRef<str> {
        let name: Vec<String> = name.into_iter().map(|x| x.as_ref().to_string()).collect();
        if let Some(ty) = self.local_types.last().unwrap().get(&name) {
            return Some(ty.clone());
        }
        self.global_types.get(&name).map(|ty| ty.clone())
    }

    fn convert_visibility(visibility: &core_lang::Visibility) -> core_annotated::Visibility {
        match visibility {
            core_lang::Visibility::Public => core_annotated::Visibility::Public,
            core_lang::Visibility::Private => core_annotated::Visibility::Private,
        }
    }

    pub fn check_files(
        &mut self,
        files: &'a Vec<(&'a str, core_lang::File<'a>)>
    ) -> Result<Vec<core_annotated::File>, TypeError> {
        let mut result = Vec::new();
        for (name, file) in files {
            if self.seen_files.contains(*name) {
                continue;
            }
            result.push(self.check_file(name, file)?);
        }
        Ok(result)
    }
    
    fn check_file(
        &mut self,
        name: &'a str,
        file: &'a core_lang::File<'a>
    ) -> Result<core_annotated::File, TypeError> {
        let core_lang::PathName { segments, start, end } = &file.path;
        let path = PathName::new(segments.clone(), *start, *end);

        let mut imports = Vec::new();
        let mut enums = Vec::new();
        let mut consts = Vec::new();
        let mut functions = Vec::new();
        
        
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
            }
        }

        let imports = Self::convert_imports(imports);
        let enums = self.check_enums(enums)?;
        let functions = self.check_functions(functions)?;
        let consts = self.check_consts(consts)?;

        let mut decl = imports;
        decl.extend(enums);
        decl.extend(consts);
        decl.extend(functions);


        let file = core_annotated::File::new(path, decl);

        
        Ok(file)
    }

    fn convert_imports(imports: Vec<&core_lang::TopLevelStatement<'a>>) -> Vec<TopLevelStatement> {
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


    fn check_enums(&mut self, enums: Vec<&core_lang::TopLevelStatement<'a>>) -> Result<Vec<TopLevelStatement>, TypeError> {
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

        let nodes = algo::kosaraju_scc(&graph);

        let nodes = nodes.into_iter().flatten().map(|x| x.index()).collect::<Vec<usize>>();

        let mut enums = enums.into_iter().map(Some).collect::<Vec<Option<&core_lang::TopLevelStatement<'a>>>>();

        for node in nodes {
            let enum_ = enums[node].take().unwrap();

            let core_lang::TopLevelStatement::Enum(enum_) = enum_ else {
                unreachable!("Encountered a non-enum after filtering only enums")
            };
            result.push(self.check_enum(enum_)?);
        }

        Ok(result)
    }

    fn check_enum(&mut self, enum_: &core_lang::Enum<'a>) -> Result<TopLevelStatement, TypeError> {
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
                let (p, (name, ty)) = self.convert_generic_parameter(x)?;
                self.add_local_type(&vec![name], Rc::new(RefCell::new(ty.clone())));
                Ok((p, ty))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut generic_params = Vec::new();
        let mut generic_args = Vec::new();
        for (param, ty) in pairs {
            generic_params.push(param);
            generic_args.push(ty);
        }

        let variants = self.check_variants(variants)?;

        self.pop_local_scope();

        if generic_params.is_empty() {
            self.add_global_type(&vec![name], Rc::new(RefCell::new(
                Type::User(PathName::new(vec![name], *start, *end)),
            )));
        } else {
            let args = generic_args.into_iter().map(|x| {
                let Expression::Type(ty) = x else {
                    unreachable!("Generic arguments should be types")
                };
                ty
            }).collect();

            let ty = Expression::Type(
                    Type::Parameterized(
                        Box::new(Type::User(PathName::new(vec![name], *start, *end))),
                        args,
                    ));
            let ty = Rc::new(RefCell::new(ty));
            self.add_global_type(&vec![name], ty);
        }

        
        Ok(Enum::new(visibility, name, generic_params, variants, *start, *end))
    }

    fn check_variants(&mut self, variants: &Vec<core_lang::Variant<'a>>) -> Result<Vec<core_annotated::Variant>, TypeError> {
        let mut result = Vec::new();
        for variant in variants {
            let core_lang::Variant { name, fields, start, end } = variant;
            let fields = fields.iter().map(|x| self.check_field(x)).collect::<Result<Vec<_>, _>>()?;
            result.push(core_annotated::Variant::new(name, fields, *start, *end));
        }
        Ok(result)
    }

    fn check_field(&mut self, field: &core_lang::Field<'a>) -> Result<core_annotated::Field, TypeError> {
        let core_lang::Field { visibility, name, ty, start, end } = field;
        let visibility = Self::convert_visibility(visibility);
        let ty = self.does_type_exist_type(ty)?;
        Ok(core_annotated::Field::new(visibility, name, ty, *start, *end))
    }

    fn convert_generic_parameter(
        &mut self,
        param: &core_lang::GenericParam<'a>
    ) -> Result<(core_annotated::GenericParam, (String, core_annotated::ExpressionType)), TypeError> {
        let core_lang::GenericParam { name, constraint, start, end } = param;
        let constraint = constraint.map(|x| self.does_type_exist_type(x)).transpose()?;


        let ty = constraint.unwrap_or_else(|| {
            Expression::Type(Type::Builtin(BuiltinType::Type))
        });

        let (pattern, name) = self.does_type_match(name, &ty)?;
        
        let param = core_annotated::GenericParam::new(pattern, constraint, *start, *end);
        

        Ok((param, (name, ty)))
    }

    fn does_type_exist(&self, ty: &core_lang::Type<'a>) -> Result<core_annotated::Type, TypeError> {
        let ty = self.convert_type(ty)?;


        Ok(ty)
    }

    fn does_type_exist_type(&self, ty: &core_lang::ExpressionType<'a>) -> Result<core_annotated::Type, TypeError> {
        let ty = self.convert_type(ty)?;


        Ok(ty)
    }

    fn convert_type(&self, ty: &core_lang::Type<'a>) -> Result<core_annotated::Type, TypeError> {
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
                    
                }
            }
            core_lang::Type::User(path) => {
                let core_lang::PathName { segments, start, end } = path;
                let segments = segments.iter().map(|x| x.to_string()).collect();
                Type::User(PathName::new(segments, *start, *end))
            }
            core_lang::Type::Parameterized(main, exprs) => {
                let main = Box::new(self.convert_type(main)?);
                let exprs = exprs.iter()
                    .map(|x| self.reduce_to_type(x))
                    .collect::<Result<Vec<_>, _>>()?;
                
            }

        }
    }
}
