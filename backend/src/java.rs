use std::collections::HashMap;

use sb_ast::core_annotated::{BuiltinType, CallArg, Enum, Expression, ExpressionRaw, File, Function, GenericParam, Import, Literal, Param, PathName, Pattern, Statement, TopLevelStatement, Type, Variant, Visibility};

use either::Either;
use crate::Codegenerator;


pub struct SubClass(pub String, pub HashMap<String, SubClass>);


pub struct JavaCodegenerator {
    sub_classes: HashMap<Vec<String>, String>,
}


impl JavaCodegenerator {
    pub fn new() -> JavaCodegenerator {
        JavaCodegenerator {
            sub_classes: HashMap::new(),
        }
    }

    fn convert_identifier<S>(name: S) -> String where S: AsRef<str> {
        let mut output = String::new();
        let chars = name.as_ref().chars();
        for c in chars {
            match c {
                '-' => {
                    output.push_str("_dash_");
                }
                '\'' => {
                    output.push_str("_prime");
                }
                '+' => {
                    output.push_str("_plus_");
                }
                '*' => {
                    output.push_str("_star_");
                }
                '/' => {
                    output.push_str("_slash_");
                }
                '%' => {
                    output.push_str("_percent_");
                }
                '<' => {
                    output.push_str("_lt_");
                }
                '>' => {
                    output.push_str("_gt_");
                }
                '=' => {
                    output.push_str("_eq_");
                }
                '!' => {
                    output.push_str("_bang_");
                }
                _ => {
                    output.push(c);
                }

            }
        }
        output
    }

    fn add_to_subclass<S>(&mut self, path: Vec<S>, content: String)
    where S: AsRef<str> {
        let path: Vec<String> = path.iter().map(|s| s.as_ref().to_string()).collect();
        match self.sub_classes.entry(path) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                let entry = entry.get_mut();
                entry.push_str(content.as_str());
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(content);
            }
        }
    }

    fn merge_subclasses(&mut self) -> SubClass {
        let mut start = SubClass(String::new(), HashMap::new());
        
        for (path, content) in self.sub_classes.iter() {
            Self::create_subclasses(&mut start, path, content);
        }
        start
    }

    fn create_subclasses(class: &mut SubClass, path: &[String], content: &String) {
        if path.len() == 1 {
            let map = &mut class.1;
            match map.entry(path[0].clone()) {
                std::collections::hash_map::Entry::Occupied(mut entry) => {
                    entry.get_mut().0.push_str(content.as_str());
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(SubClass(content.clone(), HashMap::new()));
                }
            }
        } else {
            let sub_class = class.1.get_mut(path[0].as_str());
            match sub_class {
                Some(sub_class) => {
                    Self::create_subclasses(sub_class, &path[1..], content);
                }
                None => {
                    let mut sub_class = SubClass(String::new(), HashMap::new());
                    Self::create_subclasses(&mut sub_class, &path[1..], content);
                    class.1.insert(path[0].clone(), sub_class);
                }
            }
        }
    }

    fn compile_file(&mut self, file: &sb_ast::core_annotated::File, output: &mut String) {
        let File { path, content } = file;
        let PathName { segments, .. } = path;
        output.push_str(format!("package {};\n", segments.join(".")).as_str());

        let mut imports = Vec::new();
        let mut rest = Vec::new();

        for statement in content {
            match statement {
                TopLevelStatement::Import(_) => {
                    imports.push(statement);
                }
                _ => {
                    rest.push(statement);
                }
            }
        }

        for statement in imports {
            output.push_str(self.compile_content(statement).as_str());
        }

        output.push_str(format!("public class {} {{\n", segments.last().unwrap()).as_str());

        for statement in rest {
            output.push_str(self.compile_content(statement).as_str());
        }

        let sub_class = self.merge_subclasses();

        self.compile_subclass(&sub_class, output);

        output.push_str("}\n");
    }

    fn compile_subclass(&mut self, sub_class: &SubClass, output: &mut String) {
        let SubClass(content, sub_classes) = sub_class;
        output.push_str(content.as_str());
        for (name, sub_class) in sub_classes {
            output.push_str("public static class ");
            output.push_str(name);
            output.push_str(" {\n");
            self.compile_subclass(sub_class, output);
            output.push_str("}\n");
        }
    }

    fn compile_content(&mut self, content: &TopLevelStatement) -> String {
        let mut output = String::new();
        match content {
            TopLevelStatement::Import(import) => {
                let Import { path, .. } = import;
                let PathName { segments, .. } = path;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();
                output.push_str(format!("import {};\n", segments.join(".")).as_str());
            }
            TopLevelStatement::Enum(enum_) => {
                let Enum { visibility, name, generic_params, variants, .. } = enum_;

                match visibility {
                    Visibility::Public => {
                        output.push_str("public ");
                    }
                    Visibility::Private => {
                        output.push_str("private ");
                    }
                }

                output.push_str("abstract class ");
                let name = Self::convert_identifier(name);
                output.push_str(&name);

                if !generic_params.is_empty() {
                    output.push_str("<");
                    output.push_str(self.compile_generic_params(generic_params).as_str());
                    output.push_str(">");
                }
                
                output.push_str(" {\n}\n ");

                let class_name = name;

                for variant in variants {
                    let Variant { name, fields, .. } = variant;
                    let name = Self::convert_identifier(name);

                    output.push_str("public class ");
                    output.push_str(&name);
                    if !generic_params.is_empty() {
                        output.push_str("<");
                        output.push_str(self.compile_generic_params(generic_params).as_str());
                        output.push_str(">");
                    }
                    output.push_str(" extends ");
                    output.push_str(&class_name);
                    output.push_str(" {\n");

                    for field in fields {
                        output.push_str("public ");
                        output.push_str(self.compile_type(&field.ty).as_str());
                        output.push_str(" ");
                        output.push_str(&Self::convert_identifier(&field.name));
                        output.push_str(";\n");
                    }

                    output.push_str("}\n");
                }
            }
            TopLevelStatement::Const(constant) => {
                let sb_ast::core_annotated::Const { visibility, name, ty, value, .. } = constant;
                let PathName { segments, .. } = name;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();

                match visibility {
                    Visibility::Public => {
                        output.push_str("public ");
                    }
                    Visibility::Private => {
                        output.push_str("private ");
                    }
                }
                output.push_str("static final ");
                output.push_str(self.compile_type(ty).as_str());
                output.push_str(" ");
                output.push_str(segments.last().unwrap().as_str());
                output.push_str(" = ");
                output.push_str(self.compile_expression(value).as_str());

                output = if segments.len() > 1 {
                    let mut path = segments.clone();
                    path.pop();
                    self.add_to_subclass(path, output);
                    String::new()
                } else {
                    output
                };
            }
            TopLevelStatement::Function(function) => {
                output.push_str(self.compile_function(function).as_str());
            }
        }
        output
    }

    fn compile_generic_params(&mut self, generic_params: &Vec<GenericParam>) -> String {
        let mut output = String::new();
        let mut params = Vec::new();
        for param in generic_params {
            let GenericParam { name, .. } = param;

            let bound_names = name.get_bound_names();

            for bound_name in bound_names {
                params.push(bound_name);
            }
        }
        output.push_str(params.join(", ").as_str());
        output
    }

    fn compile_type(&mut self, ty: &sb_ast::core_annotated::Type) -> String {
        let mut output = String::new();
        match ty {
            Type::Builtin(builtin) => {
                match builtin {
                    BuiltinType::I8 => {
                        output.push_str("byte");
                    }
                    BuiltinType::I16 => {
                        output.push_str("short");
                    }
                    BuiltinType::I32 => {
                        output.push_str("int");
                    }
                    BuiltinType::I64 => {
                        output.push_str("long");
                    }
                    BuiltinType::Int => {
                        output.push_str("Int");
                    }
                    BuiltinType::Nat => {
                        output.push_str("Nat");
                    }
                    BuiltinType::F32 => {
                        output.push_str("float");
                    }
                    BuiltinType::F64 => {
                        output.push_str("double");
                    }
                    BuiltinType::Bool => {
                        output.push_str("boolean");
                    }
                    BuiltinType::Char => {
                        output.push_str("char");
                    }
                    BuiltinType::Unit => {
                        output.push_str("void");
                    }
                    BuiltinType::Never => {
                        output.push_str("void");
                    }
                    BuiltinType::Type => {
                        output.push_str("Type");
                    }
                    BuiltinType::Function { params, return_type } => {
                        output.push_str("Runnable");

                        output.push_str("<");
                        for param in params {
                            output.push_str(self.compile_type(param).as_str());
                            output.push_str(", ");
                        }

                        output.push_str(self.compile_type(return_type).as_str());
                        output.push_str(">");
                    }
                }
            }
            Type::User(path) => {
                let PathName { segments, .. } = path;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();
                output.push_str(segments.join(".").as_str());
            }
            Type::Parameterized(name, params) => {
                output.push_str(self.compile_type(name).as_str());
                output.push_str("<");
                for param in params {
                    output.push_str(self.compile_type(param).as_str());
                    output.push_str(", ");
                }
            }
            Type::Tuple(types) => {
                let len = types.len();
                if len == 0 {
                    output.push_str("void");
                } else {
                    output.push_str("Tuple");
                    output.push_str(&len.to_string());
                    output.push_str("<");
                    for ty in types {
                        output.push_str(self.compile_type(ty).as_str());
                        output.push_str(", ");
                    }
                    output.push_str(">");
                }
            }
            Type::Expression(_) => {
                todo!("Expression type")
            }
            _ => unreachable!("Possible Type should not be reached"),
        }
        output
    }

    fn compile_function(&mut self, function: &sb_ast::core_annotated::Function) -> String {
        let mut output = String::new();
        match function {
            Function::Regular {
                visibility,
                name,
                generic_params,
                params,
                return_type,
                body,
                ..
            } => {
                match visibility {
                    Visibility::Public => {
                        output.push_str("public ");
                    }
                    Visibility::Private => {
                        output.push_str("private ");
                    }
                }

                output.push_str("static ");

                if !generic_params.is_empty() {
                    output.push_str("<");
                    output.push_str(self.compile_generic_params(generic_params).as_str());
                    output.push_str("> ");
                } 
                
                output.push_str(&self.compile_type(return_type));
                output.push_str(" ");
                // TODO: handle overloaded operators
                let PathName { segments, .. } = name;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();
                output.push_str(segments.last().unwrap().as_str());
                output.push_str("(");
                for (i, param) in params.iter().enumerate() {
                    let Param { name, ty, .. } = param;
                    output.push_str(&self.compile_type(ty));
                    output.push_str(" ");
                    output.push_str(name);
                    if i < params.len() - 1 {
                        output.push_str(", ");
                    }
                }
                output.push_str(") {\n");
                output.push_str(self.compile_statements(body).as_str());
                output.push_str("}\n");

                output = if segments.len() > 1 {
                    let mut path = segments.clone();
                    path.pop();
                    self.add_to_subclass(path, output);
                    String::new()
                } else {
                    output
                };
            }
            Function::Extern {
                visibility,
                name,
                generic_params,
                params,
                return_type,
                body,
                ..
            } => {
                match visibility {
                    Visibility::Public => {
                        output.push_str("public ");
                    }
                    Visibility::Private => {
                        output.push_str("private ");
                    }
                }

                output.push_str("static ");

                if !generic_params.is_empty() {
                    output.push_str("<");
                    output.push_str(self.compile_generic_params(generic_params).as_str());
                    output.push_str("> ");
                } 
                
                output.push_str(&self.compile_type(return_type));
                output.push_str(" ");
                let PathName { segments, .. } = name;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();
                output.push_str(segments.last().unwrap().as_str());
                output.push_str("(");
                for (i, param) in params.iter().enumerate() {
                    let Param { name, ty, .. } = param;
                    output.push_str(&self.compile_type(ty));
                    output.push_str(" ");
                    output.push_str(name);
                    if i < params.len() - 1 {
                        output.push_str(", ");
                    }
                }
                output.push_str(") {\n");
                output.push_str(&body);
                output.push_str("}\n");

                output = if segments.len() > 1 {
                    let mut path = segments.clone();
                    path.pop();
                    self.add_to_subclass(path, output);
                    String::new()
                } else {
                    output
                };
            }
        }
        output
    }

    fn compile_statements(&mut self, statements: &Vec<sb_ast::core_annotated::Statement>) -> String{
        let mut output = String::new();
        for statement in statements {
            match statement {
                Statement::Expression(expression) => {
                    output.push_str(self.compile_expression(expression).as_str());
                    output.push_str(";\n");
                }
                Statement::Let { name, ty, value, start, end } => {
                    output.push_str("final ");
                    output.push_str(self.compile_type(ty).as_str());
                    output.push_str(" ");
                    let name = match name {
                        Pattern::Variable(name) => name,
                        _ => todo!("Implement pattern matching for let statement"),
                    };
                    output.push_str(name);
                    output.push_str(" = ");
                    output.push_str(self.compile_expression(value).as_str());
                    output.push_str(";\n");

                }
                _ => unreachable!("Only expression and let statements are allowed in function body"),
            }
        }
        output
    }

    fn compile_expression(&mut self, expr: &Expression) -> String {
        let mut output = String::new();

        let Expression { raw, ty, .. } = expr;
        
        match raw {
            ExpressionRaw::Type(_) => {
                todo!("Type expression")
            }
            ExpressionRaw::Variable(name) => {
                let PathName { segments, .. } = name;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();
                output.push_str(segments.join(".").as_str());
            }
            ExpressionRaw::Constant(constant) => {
                let PathName { segments, .. } = constant;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();
                output.push_str(segments.join(".").as_str());
            }
            ExpressionRaw::Literal(lit) => {
                match lit {
                    Literal::Int(value) => {
                        output.push_str(&value);
                    }
                    Literal::Float(value) => {
                        output.push_str(&value);
                    }
                    Literal::Bool(value) => {
                        output.push_str(&value.to_string());
                    }
                    Literal::Char(value) => {
                        output.push_str("'");
                        output.push_str(&value.to_string());
                        output.push_str("'");
                    }
                    Literal::String(value) => {
                        output.push_str("\"");
                        output.push_str(&value);
                        output.push_str("\"");
                    }
                    Literal::Unit => {
                        output.push_str("null");
                    }
                    Literal::List(_) => {
                        unreachable!("List literal should be desugared")
                    }
                }
            }
            ExpressionRaw::Call(call) => {
                let ExpressionRaw::Variable(path) = call.name.as_ref() else {
                    unreachable!("Call expression should have a variable name");
                };
                let PathName { segments, .. } = path;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();
                output.push_str(segments.join(".").as_str());
                output.push_str("(");
                for (i, arg) in call.args.iter().enumerate() {
                    output.push_str(self.compile_call_arg(arg).as_str());
                    if i < call.args.len() - 1 {
                        output.push_str(", ");
                    }
                }
                output.push_str(")");
            }
            _ => todo!("Implement other expressions"),
        }
        output
    }

    fn compile_call_arg(&mut self, arg: &CallArg) -> String {
        let mut output = String::new();
        let CallArg { name, value, .. } = arg;
        let Either::Right(value) = value else {
            unreachable!("Call Arg should have been typechecked");
        };
        output.push_str(self.compile_expression(value).as_str());
        output
    }
}

impl Codegenerator<String> for JavaCodegenerator {
    fn generate(&mut self, file: sb_ast::core_annotated::File) -> String {
        let mut output = String::new();
        self.compile_file(&file, &mut output);
        output
    }
}
