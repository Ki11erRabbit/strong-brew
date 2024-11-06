use sb_ast::core_annotated::{self, BuiltinType, Expression, ExpressionType, Import, PathName, TopLevelStatement, Type};
use sb_ast::core_lang;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::cell::RefCell;


pub enum TypeError {

}

fn generate_internal_globals() -> HashMap<Vec<&'static str>, Rc<RefCell<ExpressionType>>> {
    let mut globals = HashMap::new();
    globals.insert(vec!["nat", "Zero"],
                   Rc::new(RefCell::new(ExpressionType::new(
                       Expression::Type(
                           Type::Builtin(BuiltinType::Function {
                               params: vec![],
                               return_type: Box::new(
                                   ExpressionType::new(
                                       Expression::Type(Type::Builtin(BuiltinType::Nat)), false)),
                           }),
                       ), false))));
    globals.insert(vec!["nat", "Succ"],
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
    overloads: HashMap<&'a str, Vec<&'a Vec<&'a str>>>,
    global_types: HashMap<Vec<&'a str>, Rc<RefCell<ExpressionType>>>,
    local_types: Vec<HashMap<Vec<&'a str>, Rc<RefCell<ExpressionType>>>>,
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

    pub fn add_global_type(&mut self, name: &'a Vec<&'a str>, ty: Rc<RefCell<ExpressionType>>) {
        let name = name.clone();
        self.global_types.insert(name, ty);
    }

    pub fn add_local_type(&mut self, name: &'a Vec<&'a str>, ty: Rc<RefCell<ExpressionType>>) {
        let name = name.clone();
        self.local_types.last_mut().unwrap().insert(name, ty);
    }

    pub fn push_local_scope(&mut self) {
        self.local_types.push(HashMap::new());
    }

    pub fn pop_local_scope(&mut self) {
        self.local_types.pop();
    }

    pub fn get_type(&self, name: &'a Vec<&'a str>) -> Option<Rc<RefCell<ExpressionType>>> {
        if let Some(ty) = self.local_types.last().unwrap().get(name) {
            return Some(ty.clone());
        }
        self.global_types.get(name).map(|ty| ty.clone())
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


        let file = core_annotated::File::new(decl, file.start, file.end);

        
        Ok(result)
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
    
}
