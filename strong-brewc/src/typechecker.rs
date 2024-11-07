use petgraph::adj::NodeIndex;
use petgraph::algo;
use petgraph::graph::UnGraph;
use sb_ast::core_annotated::{self, BuiltinType, Enum, Expression, ExpressionType, Import, PathName, TopLevelStatement, Type};
use sb_ast::core_lang;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;


pub enum TypeError {

}

fn generate_internal_globals() -> HashMap<Vec<String>, Rc<RefCell<ExpressionType>>> {
    let mut globals = HashMap::new();
    globals.insert(vec!["nat", "Zero"].into_iter().map(|x| x.to_string()).collect(),
                   Rc::new(RefCell::new(ExpressionType::new(
                       Expression::Type(
                           Type::Builtin(BuiltinType::Function {
                               params: vec![],
                               return_type: Box::new(
                                   ExpressionType::new(
                                       Expression::Type(Type::Builtin(BuiltinType::Nat)), false)),
                           }),
                       ), false))));
    globals.insert(vec!["nat", "Succ"].into_iter().map(|x| x.to_string()).collect(),
                   Rc::new(RefCell::new(ExpressionType::new(
                       Expression::Type(
                           Type::Builtin(BuiltinType::Function {
                               params: vec![
                                   ExpressionType::new(
                                       Expression::Type(Type::Builtin(BuiltinType::Nat)), false)
                               ],
                               return_type: Box::new(
                                   ExpressionType::new(
                                       Expression::Type(Type::Builtin(BuiltinType::Nat)), false)),
                           }),
                       ), false))));



    globals
}


pub struct TypeChecker<'a> {
    overloads: HashMap<&'a str, Vec<&'a Vec<String>>>,
    global_types: HashMap<Vec<String>, Rc<RefCell<ExpressionType>>>,
    local_types: Vec<HashMap<Vec<String>, Rc<RefCell<ExpressionType>>>>,
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

    pub fn add_global_type<S>(&mut self, name: &Vec<S>, ty: Rc<RefCell<ExpressionType>>)
    where S: AsRef<str> {
        let name = name.into_iter().map(|x| x.as_ref().to_string()).collect();
        self.global_types.insert(name, ty);
    }

    pub fn add_local_type<S>(&mut self, name: &Vec<S>, ty: Rc<RefCell<ExpressionType>>)
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

    pub fn get_type<S>(&self, name: &Vec<S>) -> Option<Rc<RefCell<ExpressionType>>>
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
        let generic_params = generic_params
            .iter()
            .map(|x| {
                let (p, (name, ty)) = self.convert_generic_parameter(x)?;
                self.add_local_type(&vec![name], Rc::new(RefCell::new(ty)));
                Ok(p)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let variants = self.check_variants(variants)?;

        self.pop_local_scope();

        if generic_params.is_empty() {
            self.add_global_type(&vec![name], Rc::new(RefCell::new(ExpressionType::new(
                Expression::Type(Type::User(PathName::new(vec![name], *start, *end))),
                false,
            ))));
        } else {

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
        let ty = self.does_type_exist(ty)?;
        Ok(core_annotated::Field::new(visibility, name, ty, *start, *end))
    }

    fn convert_generic_parameter(
        &mut self,
        param: &core_lang::GenericParam<'a>
    ) -> Result<(core_annotated::GenericParam, (String, core_annotated::ExpressionType)), TypeError> {
        let core_lang::GenericParam { name, constraint, start, end } = param;
        let constraint = constraint.map(|x| self.does_type_exist(x)).transpose()?;


        let ty = constraint.unwrap_or_else(|| {
            let ty = ExpressionType::new(
                Expression::Type(Type::Builtin(BuiltinType::Type)),
                false,
            );
            ty
        });

        let (pattern, name) = self.does_type_match(name, &ty)?;
        
        let param = core_annotated::GenericParam::new(pattern, constraint, *start, *end);
        

        Ok((param, (name, ty)))
    }
}
