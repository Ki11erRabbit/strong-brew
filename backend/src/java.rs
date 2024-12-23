mod match_generator;

use std::collections::HashMap;

use sb_ast::core_annotated::{BuiltinType, Call, CallArg, Closure, Enum, Expression, ExpressionRaw, Field, File, Function, GenericParam, IfExpr, Import, Literal, MatchArm, MatchExpr, Param, PathName, Pattern, Statement, Struct, TopLevelStatement, Type, Variant, Visibility};

use either::Either;
use crate::Codegenerator;


static CORE_IMPORT: &str = "import static strongbrew.core.core.*;\n";
static TUPLE_IMPORT: &str = "import strongbrew.tuples.*;\n";
static CALLABLE_IMPORT: &str = "import strongbrew.callables.*;\n";
static NUMBER_IMPORT: &str = "import strongbrew.numbers.*;\n";
static MUT_IMPORT: &str = "import strongbrew.Mut;\n";

#[derive(Debug, Clone)]
pub enum InLetBinding<'a> {
    /// Not Applicable
    NA,
    Yes(&'a Pattern, Option<char>, &'a Type),
    No(&'a Expression),
}


pub struct SubClass(pub String, pub HashMap<String, SubClass>);


pub struct JavaCodegenerator {
    /// Subclasses are where we put qualified names.
    /// Qualified names are how we handle function overloading.
    sub_classes: HashMap<Vec<String>, String>,
    /// Grouped imports are used to import all of the classes created by a module.
    /// This is due to how nested classes are handled in Java.
    grouped_imports: HashMap<String, String>,
    constructors: HashMap<Vec<String>, Vec<String>>,
    tags_for_enum: HashMap<String, HashMap<String, usize>>,
}


impl JavaCodegenerator {
    pub fn new() -> JavaCodegenerator {
        JavaCodegenerator {
            sub_classes: HashMap::new(),
            grouped_imports: HashMap::new(),
            constructors: HashMap::new(),
            tags_for_enum: HashMap::new(),
        }
    }

    /// Converts a string to a valid Java identifier.
    /// This is done by replacing all invalid characters with valid ones.
    /// This does lead to more verbose names, but it is good enough for a compiler into Java.
    fn convert_identifier<S>(name: S) -> String where S: AsRef<str> {

        match name.as_ref() {
            "while" => return String::from("whilefn"),
            "for" => return String::from("forfn"),
            _ => {}
        }
        
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
                '[' => {
                    output.push_str("_bracket_open");
                }
                ']' => {
                    output.push_str("_bracket_close");
                }
                '&' => {
                    output.push_str("_and_");
                }
                '|' => {
                    output.push_str("_or_");
                }
                _ => {
                    output.push(c);
                }

            }
        }
        output
    }

    fn add_constructor(&mut self, path: Vec<String>, constructor: String) {
        match self.constructors.entry(path) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                let entry = entry.get_mut();
                entry.push(constructor);
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(vec![constructor]);
            }
        }
    }

    fn add_tag_for_enum(&mut self, enum_name: String, tag: String, value: usize) {
        match self.tags_for_enum.entry(enum_name) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                let entry = entry.get_mut();
                entry.insert(tag, value);
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                let mut map = HashMap::new();
                map.insert(tag, value);
                entry.insert(map);
            }
        }
    }

    fn get_tag_for_enum(&mut self, enum_name: &str, tag: &str) -> Option<usize> {
        let map = self.tags_for_enum.get(enum_name);
        match map {
            Some(map) => {
                map.get(tag).cloned()
            }
            None => {
                None
            }
        }
    }

    fn add_new_import(&mut self, path: String, import: String) {
        match self.grouped_imports.entry(path) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                let entry = entry.get_mut();
                entry.push_str(import.as_str());
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(import);
            }
        }
    }

    fn get_import(&mut self, path: &str) -> Option<&String> {
        self.grouped_imports.get(path)
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

    /// This function converts subclasses to a linked list of subclasses.
    /// This is done because there maybe be multiple subclasses with the same base name.
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

    /// The output parameter is a vector of names of classes and the content of the class.
    /// The name should be blank if the class has the same name as the module. This should be the first element in the vector.
    /// A vector index is used to represent an individual class.
    fn compile_file(&mut self, file: &sb_ast::core_annotated::File, output: &mut Vec<(String, String)>) {
        let mut output_string = String::new();
        let File { path, content } = file;
        let PathName { segments, .. } = path;
        let mut module_segments = segments.clone();
        module_segments.pop();
        // We need to exclude the last segment as it is the name of the file. Java doesn't like that.
        if !module_segments.is_empty() {
            if module_segments.len() == 1 {
                output_string.push_str(format!("package {};\n", segments.join(".")).as_str());
            } else {
                output_string.push_str(format!("package {};\n", module_segments.join(".")).as_str());
            }
        }

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
            let content = self.compile_content(statement);
            let string = content[0].1.as_str();
            match self.get_import(string) {
                Some(import) => {
                    output_string.push_str(import);
                }
                None => {
                    output_string.push_str(string);
                }
            }
        }
        output_string.push_str(CORE_IMPORT);
        output_string.push_str(TUPLE_IMPORT);
        output_string.push_str(CALLABLE_IMPORT);
        output_string.push_str(NUMBER_IMPORT);
        output_string.push_str(MUT_IMPORT);


        let mut statement_output = Vec::new();
        for statement in rest {
            statement_output.push(self.compile_content(statement));
        }


        
        let mut pushed_class = false;
        for statement in statement_output {
            for (i, (name, s)) in statement.into_iter().enumerate() {
                // Here we resize the output vector to the current index if needed.
                if i >= output.len() {
                    output.push((name.clone(), output_string.clone()));
                }
                // If we are at the current module and we haven't pushed the class header yet, we push it.
                if i == 0 && !pushed_class {
                    pushed_class = true;
                    output[i].1.push_str(format!("public class {} {{\n", segments.last().unwrap()).as_str());
                }
                // If we are at the current module, we push the content.
                if i == 0 {
                    output[i].1.push_str(s.as_str());
                }
                // If we are not at the current module, we add the content to the subclass.
                if i > 0 {
                    // Here we add the import needed by other classes if there are submodules.
                    let import_key = format!("import static {}.*;\n", segments.join("."));
                    let import_name = format!("import {}.{};", module_segments.join("."), name);
                    self.add_new_import(import_key, import_name);
                    output[i].1.push_str(s.as_str());
                }
            }
        }

        let sub_class = self.merge_subclasses();

        if !pushed_class {
            if 0 >= output.len() {
                output.push((String::new(), output_string));
            }
            output[0].1.push_str(format!("public class {} {{\n", segments.last().unwrap()).as_str());
        }
        
        self.compile_subclass(&sub_class, &mut output[0].1);

        output[0].1.push_str("}\n");
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

    /// The return type is a vector of names of classes and the content of the class.
    /// The name should be blank if the class has the same name as the module. This should be the first element in the vector.
    /// A vector index is used to represent an individual class.
    fn compile_content(&mut self, content: &TopLevelStatement) -> Vec<(String, String)> {
        let mut output = String::new();
        let mut output_vec = Vec::new();
        let mut top_level_output = String::new();
        match content {
            TopLevelStatement::Import(import) => {
                let Import { path, .. } = import;
                let PathName { segments, .. } = path;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();
                top_level_output.push_str(format!("import static {}.*;\n", segments.join(".")).as_str());
            }
            TopLevelStatement::Struct(struct_) => {
                let Struct { visibility, name, generic_params, fields, .. } = struct_;

                match visibility {
                    Visibility::Public => {
                        output.push_str("public ");
                    }
                    Visibility::Private => {
                        output.push_str("private ");
                    }
                }

                output.push_str("class ");
                let name = Self::convert_identifier(name);
                output.push_str(&name);

                if !generic_params.is_empty() {
                    output.push_str("<");
                    output.push_str(self.compile_generic_params(generic_params).as_str());
                    output.push_str(">");
                }

                output.push_str(" {\n");

                let mut constructor = format!("public static {} {}", name, name);
                let mut constructor_body = format!("return new {}", name);
                let mut internal_constructor = format!("public {}", name);
                let mut internal_constructor_body = String::new();
                if !generic_params.is_empty() {
                    constructor.push_str("<");
                    constructor.push_str(self.compile_generic_params(generic_params).as_str());
                    constructor.push_str(">");
                    constructor_body.push_str("<");
                    constructor_body.push_str(">");
                    internal_constructor.push_str("<");
                    internal_constructor.push_str(self.compile_generic_params(generic_params).as_str());
                    internal_constructor.push_str(">");
                }

                constructor.push_str("(");
                internal_constructor.push_str("(");
                constructor_body.push_str("(");

                let class_name = name;

                for (i, field) in fields.iter().enumerate() {
                    let Field { visibility, name, ty, .. } = field;

                    self.add_constructor(vec![class_name.clone()], name.clone());
                    
                    let mut getter = match visibility {
                        Visibility::Public => {
                            output.push_str("public ");
                            String::from("public static ")
                        }
                        Visibility::Private => {
                            //output.push_str("");
                            String::from("private static ")
                        }
                    };

                    getter.push_str(self.compile_type(ty).as_str());
                    getter.push_str(" ");

                    output.push_str(self.compile_type(ty).as_str());
                    output.push_str(" ");
                    output.push_str(Self::convert_identifier(name).as_str());
                    output.push_str(";\n");

                    getter.push_str(Self::convert_identifier(name).as_str());

                    getter.push_str("(");
                    getter.push_str(class_name.as_str());
                    getter.push_str(" self) {\n");

                    getter.push_str("return self.");
                    getter.push_str(Self::convert_identifier(name).as_str());
                    getter.push_str(";\n}\n");

                    constructor.push_str(self.compile_type(ty).as_str());
                    constructor.push_str(" ");
                    constructor.push_str(Self::convert_identifier(name).as_str());

                    constructor_body.push_str(Self::convert_identifier(name).as_str());

                    internal_constructor.push_str(self.compile_type(ty).as_str());
                    internal_constructor.push_str(" ");
                    internal_constructor.push_str(Self::convert_identifier(name).as_str());

                    internal_constructor_body.push_str("this.");
                    internal_constructor_body.push_str(Self::convert_identifier(name).as_str());
                    internal_constructor_body.push_str(" = ");
                    internal_constructor_body.push_str(Self::convert_identifier(name).as_str());
                    internal_constructor_body.push_str(";\n");

                    
                    
                    if i < fields.len() - 1 {
                        constructor.push_str(", ");
                        constructor_body.push_str(", ");
                        internal_constructor.push_str(", ");
                    }
                    output.push_str(getter.as_str());
                    top_level_output.push_str("\n");
                }


                constructor.push_str(") {\n");
                constructor.push_str(constructor_body.as_str());
                constructor.push_str(");\n}\n");

                internal_constructor.push_str(") {\n");
                internal_constructor.push_str(internal_constructor_body.as_str());
                internal_constructor.push_str("\n}\n");

                output.push_str(internal_constructor.as_str());

                output.push_str("}\n");

                output_vec.push((class_name.clone(), output));

                top_level_output.push_str(constructor.as_str());
                top_level_output.push_str("\n");
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

                // Enums are represented as abstract classes in Java with an implementation for each variant.
                // It should also contain constructor static methods to create each variant.
                output.push_str("abstract class ");
                let name = Self::convert_identifier(name);
                output.push_str(&name);

                if !generic_params.is_empty() {
                    output.push_str("<");
                    output.push_str(self.compile_generic_params(generic_params).as_str());
                    output.push_str(">");
                }
                
                output.push_str(" {\n");

                output.push_str("public abstract int tag();\n");

                // We need to backup the class name as we need to use it in the constructor functions.
                let class_name = name;
                for (i, variant) in variants.into_iter().enumerate() {
                    let mut variant_output = String::new();
                    let Variant { name, fields, .. } = variant;
                    let name = Self::convert_identifier(name);

                    let mut constructor = String::from("public static ");

                    variant_output.push_str("public class ");
                    variant_output.push_str(&name);
                    if !generic_params.is_empty() {
                        variant_output.push_str("<");
                        variant_output.push_str(self.compile_generic_params(generic_params).as_str());
                        variant_output.push_str(">");

                        constructor.push_str("<");
                        constructor.push_str(self.compile_generic_params(generic_params).as_str());
                        constructor.push_str(">");
                    }

                    constructor.push_str(&class_name);
                    if !generic_params.is_empty() {
                        constructor.push_str("<");
                        constructor.push_str(self.compile_generic_params(generic_params).as_str());
                        constructor.push_str(">");
                    }
                    constructor.push_str(" ");
                    constructor.push_str(&name);
                    constructor.push_str("(");
                    
                    variant_output.push_str(" extends ");
                    variant_output.push_str(&class_name);

                    if !generic_params.is_empty() {
                        variant_output.push_str("<");
                        variant_output.push_str(self.compile_generic_params(generic_params).as_str());
                        variant_output.push_str(">");
                    }
                    
                    variant_output.push_str(" {\n");

                    variant_output.push_str("@Override\n");
                    variant_output.push_str("public int tag() {\n");
                    variant_output.push_str("return ");
                    variant_output.push_str(&format!("{}", i));
                    variant_output.push_str(";\n}\n");

                    self.add_tag_for_enum(class_name.clone(), name.clone(), i);
                    
                    
                    let mut internal_constructor = String::from("public ");
                    let mut new_call = String::new();
                    if !generic_params.is_empty() {
                        new_call.push_str("<");
                        new_call.push_str(self.compile_generic_params(generic_params).as_str());
                        new_call.push_str(">");

                        /*internal_constructor.push_str("<");
                        internal_constructor.push_str(self.compile_generic_params(generic_params).as_str());
                        internal_constructor.push_str(">");*/
                    }
                    new_call.push_str("(");
                    internal_constructor.push_str(" ");
                    internal_constructor.push_str(&name);
                    internal_constructor.push_str("(");

                    let mut contructor_body = String::new();


                    for (i, field) in fields.into_iter().enumerate() {

                        let getter = {
                            let Field { visibility, name, ty, .. } = field;
                            let mut getter = match visibility {
                                Visibility::Public => {
                                    String::from("public ")
                                }
                                Visibility::Private => {
                                    String::from("private ")
                                }
                            };
                            getter.push_str(self.compile_type(ty).as_str());
                            getter.push_str(" ");
                            getter.push_str(Self::convert_identifier(name).as_str());
                            getter.push_str("() {\n");
                            getter.push_str("return this.");
                            getter.push_str(Self::convert_identifier(name).as_str());
                            getter.push_str(";\n}\n");
                            getter
                        };
                        

                        self.add_constructor(vec![class_name.clone(), name.clone()], field.name.clone());
                        
                        variant_output.push_str("public ");
                        variant_output.push_str(self.compile_type(&field.ty).as_str());
                        variant_output.push_str(" ");
                        variant_output.push_str(&Self::convert_identifier(&field.name));
                        variant_output.push_str(";\n");

                        internal_constructor.push_str(self.compile_type(&field.ty).as_str());
                        internal_constructor.push_str(" ");
                        internal_constructor.push_str(&Self::convert_identifier(&field.name));

                        contructor_body.push_str("this.");
                        contructor_body.push_str(&Self::convert_identifier(&field.name));
                        contructor_body.push_str(" = ");
                        contructor_body.push_str(&Self::convert_identifier(&field.name));
                        contructor_body.push_str(";\n");

                        constructor.push_str(self.compile_type(&field.ty).as_str());
                        constructor.push_str(" ");
                        constructor.push_str(&Self::convert_identifier(&field.name));
                        new_call.push_str(&Self::convert_identifier(&field.name));
                        if i < fields.len() - 1 {
                            constructor.push_str(", ");
                            new_call.push_str(", ");
                            internal_constructor.push_str(", ");
                        }
                        variant_output.push_str(getter.as_str());
                    }
                    internal_constructor.push_str(") {\n");
                    internal_constructor.push_str(contructor_body.as_str());
                    internal_constructor.push_str("}\n");

                    variant_output.push_str(internal_constructor.as_str());

                    variant_output.push_str("}\n");

                    constructor.push_str(") {\n");
                    new_call.push_str(");\n");

                    constructor.push_str("return new ");
                    constructor.push_str(&name);
                    constructor.push_str(new_call.as_str());
                    constructor.push_str("\n}\n");

                    output.push_str(constructor.as_str());

                    output_vec.push((name.clone(), variant_output));
                }


                output.push_str("\n}\n");

                output_vec.insert(0, (class_name, output));
            }
            TopLevelStatement::Const(constant) => {
                let sb_ast::core_annotated::Const { visibility, name, ty, value, .. } = constant;
                let PathName { segments, .. } = name;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();

                match visibility {
                    Visibility::Public => {
                        top_level_output.push_str("public ");
                    }
                    Visibility::Private => {
                        top_level_output.push_str("private ");
                    }
                }
                top_level_output.push_str("static final ");
                top_level_output.push_str(self.compile_type(ty).as_str());
                top_level_output.push_str(" ");
                top_level_output.push_str(segments.last().unwrap().as_str());
                top_level_output.push_str(" = ");
                top_level_output.push_str(self.compile_expression(value, true, InLetBinding::NA).as_str());
                top_level_output.push_str(";\n");

                top_level_output = if segments.len() > 1 {
                    let mut path = segments.clone();
                    path.pop();
                    self.add_to_subclass(path, top_level_output);
                    String::new()
                } else {
                    top_level_output
                };
            }
            TopLevelStatement::Function(function) => {
                top_level_output.push_str(self.compile_function(function).as_str());
            }
            TopLevelStatement::Extern(_, body) => {
                top_level_output.push_str(body);
            }
        }
        output_vec.insert(0 ,(String::new(), top_level_output));
        output_vec
    }

    fn compile_generic_params(&mut self, generic_params: &Vec<GenericParam>) -> String {
        let mut output = String::new();
        let mut params = Vec::new();
        for param in generic_params {
            let GenericParam { name, .. } = param;

            params.push(name.as_str());
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
                        output.push_str("Callable");
                        output.push_str(&format!("{}", params.len()));

                        output.push_str("<");
                        for param in params {
                            let ty = self.compile_type(param);
                            let ty = Self::change_type_for_generic(ty.as_str());
                            output.push_str(ty);
                            output.push_str(", ");
                        }
                        let ty = self.compile_type(return_type);
                        let ty = Self::change_type_for_generic(ty.as_str());

                        output.push_str(ty);
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

                // Here we must handle the special case of the Array type.
                match name.as_ref() {
                    Type::User(path) => {
                        let PathName { segments, .. } = path;
                        if segments.last().unwrap().as_str() == "Array" {
                            for param in params {
                                output.push_str(self.compile_type(param).as_str());
                                output.push_str("[]");
                                return output;
                            }
                            unreachable!("Array type must have a type parameter");
                        }
                    }
                    _ => {}
                }
                
                
                output.push_str(self.compile_type(name).as_str());
                output.push_str("<");
                for (i, param) in params.iter().enumerate() {
                    let ty = self.compile_type(param);
                    let ty = Self::change_type_for_generic(ty.as_str());
                    output.push_str(ty);

                    if i < params.len() - 1 {
                        output.push_str(", ");
                    }
                }
                output.push_str(">");
            }
            Type::Tuple(types) => {
                let len = types.len();
                if len == 0 {
                    output.push_str("void");
                } else {
                    output.push_str("Tuple");
                    output.push_str(&len.to_string());
                    output.push_str("<");
                    for (i, ty) in types.iter().enumerate() {
                        let ty = self.compile_type(ty);
                        let ty = Self::change_type_for_generic(ty.as_str());
                        output.push_str(ty);
                        if i < len - 1 {
                            output.push_str(", ");
                        }
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

    fn change_type_for_generic<'a>(ty: &'a str) -> &'a str {
        match ty {
            "void" => "Void",
            "int" => "Integer",
            "boolean" => "Boolean",
            "char" => "Character",
            "byte" => "Byte",
            "short" => "Short",
            "long" => "Long",
            "float" => "Float",
            "double" => "Double",
            x => x,
        }
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
                output.push_str(self.compile_statements(body, false, InLetBinding::NA).as_str());
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

    fn compile_statements(
        &mut self,
        statements: &Vec<sb_ast::core_annotated::Statement>,
        rhs: bool,
        in_let: InLetBinding

    ) -> String{
        let mut output = String::new();
        for (i, statement) in statements.iter().enumerate() {
            match statement {
                Statement::Expression(expression) => {
                    if i == statements.len() - 1 {
                        match &in_let {
                            InLetBinding::NA => {
                                output.push_str(self.compile_expression(expression, true, InLetBinding::NA).as_str());
                                output.push_str(";\n");
                            }
                            InLetBinding::Yes(name, generic, ty) => {
                                if expression.is_if_or_match() {
                                    output.push_str(self.compile_expression(expression, true, in_let).as_str());
                                    break;
                                }

                                output.push_str(&self.compile_let_pattern(name, &ty, expression, *generic));

                            }
                            InLetBinding::No(expr) => {
                                if expression.is_if_or_match() {
                                    output.push_str(self.compile_expression(expression, true, in_let).as_str());
                                    break;
                                }

                                output.push_str(&self.compile_expression(expr, true, InLetBinding::NA));
                                output.push_str(".set(");

                                output.push_str(&self.compile_expression(expression, true, InLetBinding::NA));
                                output.push_str(");\n");

                            }
                        }
                        break;

                    }
                    output.push_str(self.compile_expression(expression, true, InLetBinding::NA).as_str());
                    output.push_str(";\n");
                    
                }
                Statement::Let { name, ty, value, start, end } => {

                    output.push_str(&self.compile_let_pattern(name, ty, value, None));
                    

                }
                Statement::Assignment { target, value, start, end } => {
                    // Here we must call the Mut method set so that we can updated the mut's value.
                    // Only Mut is allowed for mutable variables.
                    if value.is_if_or_match() {
                        output.push_str(&self.compile_expression(value, true, InLetBinding::No(target)));
                        continue;
                    }
                    output.push_str(&self.compile_expression(target, false, InLetBinding::NA));
                    output.push_str(".set(");
                    output.push_str(&self.compile_expression(value, true, InLetBinding::NA));
                    output.push_str(");\n");
                }
            }
        }
        output
    }

    fn compile_let_pattern(
        &mut self,
        pattern: &Pattern,
        ty: &Type,
        value: &Expression,
        current_tuple: Option<char>,
    ) -> String {
        let mut output = String::new();
        match pattern {
            Pattern::Variable(name) => {
                output.push_str("final ");
                output.push_str(self.compile_type(ty).as_str());
                output.push_str(" ");
                output.push_str(name);

                if value.is_if_or_match() {

                    output.push_str(";\n");

                    output.push_str(&self.compile_expression(value, true, InLetBinding::Yes(pattern, current_tuple, ty)));
                    return output;
                }
                output.push_str(" = ");
                output.push_str(self.compile_expression(value, true, InLetBinding::NA).as_str());
                if let Some(current_tuple) = current_tuple {
                    output.push_str(".");
                    output.push(current_tuple);
                }
            }
            Pattern::Wildcard => {
                output.push_str(self.compile_expression(value, true, InLetBinding::NA).as_str());
            }
            Pattern::Tuple(patterns) => {

                let Type::Tuple(types) = ty else {
                    unreachable!("Tuple pattern must have a tuple type")
                };

                let iter = patterns.iter().zip(types.iter()).zip('a'..='z');
                for ((pattern, ty), access) in iter {
                    output.push_str(self.compile_let_pattern(pattern, ty, value, Some(access)).as_str());
                }
            }
            Pattern::Constructor { name, fields } => {
                let PathName { segments, .. } = name;
                let Type::Tuple(types) = ty else {
                    unreachable!("Constructor pattern must have a tuple type")
                };

                let constructor = self.constructors.get(segments).unwrap().clone();
                let iter = fields.iter().zip(constructor.iter()).zip(types.iter());
                for ((field, constructor), ty) in iter {

                    let mut segments = segments.clone();
                    segments.push(constructor.clone());
                    let expression = Expression::new(ExpressionRaw::Call(Call {
                        name: Box::new(ExpressionRaw::Variable(PathName {
                            segments,
                            start: 0,
                            end: 0,
                        })),
                        args: vec![CallArg::new(None, Either::Right(value.clone()), 0, 0)],
                        type_args: vec![],
                        start: 0,
                        end: 0,

                    }), ty);
                    
                    output.push_str(self.compile_let_pattern(field, ty, &expression, None).as_str());
                }
            }
            _ => todo!("Implement pattern matching for let statement"),
        }
        output.push_str(";\n");
        output
    }

    /// rhs is Right Hand Side. It is to specify that it is on side that provides a value
    /// variable is the value that must be bound to since in Java if is not an if expression
    /// that can take multiple statements.
    fn compile_expression(
        &mut self,
        expr: &Expression,
        rhs: bool,
        variable: InLetBinding
    ) -> String {
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

                // Here, if we are on the left hand side and are a mut type then we
                // mut access the inner value to get the value wrapped by mut.
                if rhs {
                    if ty.borrow().is_mut() {
                        output.push_str(".get()")
                    }
                }

                
            }
            ExpressionRaw::Constant(constant) => {
                let PathName { segments, .. } = constant;
                let segments: Vec<String> = segments.iter().map(|s| Self::convert_identifier(s)).collect();
                output.push_str(segments.join(".").as_str());
            }
            ExpressionRaw::Literal(lit) => {
                // Here we wrap the value in a new Mut if it is a mutable variable.
                if ty.borrow().is_mut() {
                    output.push_str("new Mut<>(");
                }
                match lit {
                    Literal::Int(value) => {
                        match &*ty.borrow() {
                            Type::Builtin(BuiltinType::I8) |
                            Type::Builtin(BuiltinType::I16) |
                            Type::Builtin(BuiltinType::I32) |
                            Type::Builtin(BuiltinType::I64) => {
                                output.push_str(&value);
                            }
                            Type::Builtin(BuiltinType::Int) => {
                                output.push_str("new Int(");
                                output.push_str(&value.to_string());
                                output.push_str(")");
                            }
                            Type::Builtin(BuiltinType::Nat) => {
                                output.push_str("new Nat(");
                                output.push_str(&value.to_string());
                                output.push_str(")");
                            }
                            Type::Builtin(BuiltinType::F32) |
                            Type::Builtin(BuiltinType::F64) => {
                                output.push_str(&value);
                            }
                            x if x == &Type::Builtin(BuiltinType::I8) => {
                                output.push_str(&value);
                            }
                            x if x == &Type::Builtin(BuiltinType::I16) => {
                                output.push_str(&value);
                            }
                            x if x == &Type::Builtin(BuiltinType::I32) => {
                                output.push_str(&value);
                            }
                            x if x == &Type::Builtin(BuiltinType::I64) => {
                                output.push_str(&value);
                            }
                            x if x == &Type::Builtin(BuiltinType::Int) => {
                                output.push_str("new Int(");
                                output.push_str(&value.to_string());
                                output.push_str(")");
                            }
                            x if x == &Type::Builtin(BuiltinType::Nat) => {
                                output.push_str("new Nat(");
                                output.push_str(&value.to_string());
                                output.push_str(")");
                            }
                            x if x == &Type::Builtin(BuiltinType::F32) => {
                                output.push_str(&value);
                            }
                            x if x == &Type::Builtin(BuiltinType::F64) => {
                                output.push_str(&value);
                            }
                            _ => unreachable!("Internal error: Int literal should have a numeric type"),
                        }
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
                if ty.borrow().is_mut() {
                    output.push_str(")");
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
            ExpressionRaw::IfExpression(if_) => {
                output.push_str(&self.compile_if_expr(if_, rhs, variable));
            }
            ExpressionRaw::Closure(closure) => {
                let Closure { params, return_type, body, start, end } = closure;
                output.push_str("new ");
                let param_count = params.len();
                output.push_str(&format!("Callable{}<", param_count));
                for param in params.iter() {
                    let ty = self.compile_type(&param.ty);
                    let ty = Self::change_type_for_generic(&ty);
                    output.push_str(&ty);
                    output.push_str(",");
                }
                let Some(return_type) = return_type else {
                    unreachable!("Return type of closure should be known at this time")
                };
                let ty = self.compile_type(return_type.as_ref());
                let ty = Self::change_type_for_generic(&ty);
                output.push_str(ty);

                output.push_str(">() {\n@Override\npublic ");
                output.push_str(&format!("{} call(", ty));
                for (i, param) in params.into_iter().enumerate() {
                    let ty = self.compile_type(&param.ty);
                    let ty = Self::change_type_for_generic(&ty);
                    output.push_str(ty);
                    output.push_str(&format!(" {}", param.name));
                    if i < params.len() - 1 {
                        output.push_str(", ");
                    }
                }
                output.push_str(") {\n");
                output.push_str(&self.compile_statements(body, false, InLetBinding::NA));
                // When we are "Void" we must handle the the fact that in Java it is an object
                if ty == "Void" {
                    output.push_str("return null;\n");
                }

                output.push_str("}\n}\n");
                
            }
            ExpressionRaw::Return(body) => {
                match body {
                    Some(Either::Right(body)) => {
                        let body = self.compile_expression(body.as_ref(), true, InLetBinding::NA);
                        output.push_str("return ");
                        output.push_str(&body);
                    }
                    None => todo!("Implement empty returns"),
                    _ => unreachable!("Return body should have been converted")
                }

            }
            ExpressionRaw::Tuple(Either::Right(tuple_expr)) => {
                let len = tuple_expr.len();
                output.push_str("new Tuple");
                output.push_str(&len.to_string());
                output.push_str("<>(");
                for (i, expr) in tuple_expr.iter().enumerate() {
                    let expr = self.compile_expression(expr, true, InLetBinding::NA);
                    output.push_str(expr.as_str());
                    if i < len - 1 {
                        output.push_str(", ");
                    }
                }
                output.push_str(")");
            }
            ExpressionRaw::Tuple(Either::Left(_)) => unreachable!("Tuple expression should have been typechecked"),
            ExpressionRaw::MatchExpression(match_) => {
                let MatchExpr { value, arms, .. } = match_;
                output.push_str("switch (");
                let Expression { ty, .. } = &value.as_ref();
                if let Type::User(path) = &*ty.borrow() {
                    let PathName { segments, .. } = path;
                    if segments.last().unwrap().as_str() == "String" {
                        output.push_str(&self.compile_expression(value, true, InLetBinding::NA));
                    } else {
                        output.push_str(&self.compile_expression(value, true, InLetBinding::NA));
                        output.push_str(".tag()");
                    }
                } else {
                    output.push_str(self.compile_expression(value, true, InLetBinding::NA).as_str());
                    output.push_str(".tag()");
                }
                output.push_str(") {\n");
                let match_arg = value;
                for arm in arms {
                    let MatchArm { pattern, value, .. } = arm;
                    match pattern {
                        Pattern::Wildcard => {
                            output.push_str("default:\n");
                            match value {
                                Either::Right(statements) => {
                                    output.push_str(self.compile_statements(statements, rhs, InLetBinding::NA).as_str());
                                }
                                Either::Left(expr) => {
                                    match variable {
                                        InLetBinding::Yes(name, generic, ty) => {
                                            output.push_str(self.compile_let_pattern(name, ty, expr,generic).as_str());
                                        }
                                        _ => {
                                            output.push_str(self.compile_expression(expr, true, InLetBinding::NA).as_str());
                                        }
                                    }
                                }
                            }
                        }
                        Pattern::Literal(lit) => {
                            match lit {
                                Literal::Int(value) => {
                                    output.push_str("case ");
                                    output.push_str(&value);
                                    output.push_str(":\n");
                                }
                                Literal::Float(value) => {
                                    output.push_str("case ");
                                    output.push_str(&value);
                                    output.push_str(":\n");
                                }
                                Literal::Bool(value) => {
                                    output.push_str("case ");
                                    output.push_str(&value.to_string());
                                    output.push_str(":\n");
                                }
                                Literal::Char(value) => {
                                    output.push_str("case '");
                                    output.push_str(&value.to_string());
                                    output.push_str("':\n");
                                }
                                Literal::String(value) => {
                                    output.push_str("case \"");
                                    output.push_str(&value);
                                    output.push_str("\":\n");
                                }
                                Literal::Unit => {
                                    output.push_str("case null:\n");
                                }
                                _ => todo!("Implement other literals"),
                            }
                            match value {
                                Either::Right(statements) => {
                                    output.push_str(self.compile_statements(statements, rhs, InLetBinding::NA).as_str());
                                }
                                Either::Left(expr) => {
                                    match variable {
                                        InLetBinding::Yes(name, generic, ty) => {
                                            output.push_str(self.compile_let_pattern(name, ty, expr, generic).as_str());
                                        }
                                        _ => {
                                            output.push_str(self.compile_expression(expr, true, InLetBinding::NA).as_str());
                                        }
                                    }
                                }
                            }
                        }
                        Pattern::Variable(var) => {
                            output.push_str("default:\n");
                            match value {
                                Either::Right(statements) => {
                                    output.push_str(&self.compile_type(&ty.borrow()));
                                    output.push_str(" ");
                                    output.push_str(var);
                                    output.push_str(" = ");
                                    output.push_str(self.compile_expression(match_arg, true, InLetBinding::NA).as_str());
                                    
                                    output.push_str(self.compile_statements(statements, rhs, InLetBinding::NA).as_str());
                                }
                                Either::Left(expr) => {
                                    match variable {
                                        InLetBinding::Yes(name, generic, ty) => {
                                            output.push_str(self.compile_let_pattern(name, ty, expr, generic).as_str());
                                        }
                                        _ => {
                                            output.push_str(self.compile_expression(expr, true, InLetBinding::NA).as_str());
                                        }
                                    }
                                }
                            }
                        }
                        Pattern::Constructor { name, fields } => {
                            let PathName { segments, .. } = name;
                            let Type::Tuple(types) = &*ty.borrow() else {
                                unreachable!("Constructor pattern must have a tuple type")
                            };
                            let part1 = segments.iter().rev().skip(1).next().unwrap();
                            let part2 = segments.iter().rev().next().unwrap();
                            let Some(tag) = self.get_tag_for_enum(part1, part2) else {
                                unreachable!("Enum tag must be known at this time")
                            };

                            output.push_str("case ");
                            output.push_str(&tag.to_string());
                            output.push_str(":\n");

                            let constructor = self.constructors.get(segments).unwrap().clone();
                            let iter = fields.iter().zip(constructor.iter()).zip(types.iter());
                            for ((field, constructor), ty) in iter {
                                output.push_str(&self.compile_type(ty));
                                output.push_str(" ");
                                match field {
                                    Pattern::Variable(name) => {
                                        output.push_str(name);
                                    }
                                    _ => {
                                        todo!("Implement other patterns for constructor fields")
                                    }
                                }
                                output.push_str(&format!(" = (({})", part2));
                                output.push_str(self.compile_expression(match_arg, true, InLetBinding::NA).as_str());
                                output.push_str(").");
                                output.push_str(constructor);
                                output.push_str("();\n");
                            }

                            match value {
                                Either::Right(statements) => {
                                    output.push_str(self.compile_statements(statements, rhs, InLetBinding::NA).as_str());
                                }
                                Either::Left(expr) => {
                                    match variable {
                                        InLetBinding::Yes(name, generic, ty) => {
                                            output.push_str(self.compile_let_pattern(name, ty, expr, generic).as_str());
                                        }
                                        _ => {
                                            output.push_str(self.compile_expression(expr, true, InLetBinding::NA).as_str());
                                        }
                                    }
                                }
                            }
                        }
                        _ => todo!("Implement other patterns for match expression"),
                    }

                    output.push_str("break;\n");
                }
                output.push_str("}\n");
            }
            _ => todo!("Implement other expressions"),
        }
        output
    }

    fn compile_if_expr(&mut self, expr: &IfExpr, rhs: bool, variable: InLetBinding) -> String {
        let mut output = String::new();

        let IfExpr { condition, then_branch, else_branch, .. } = expr;
        output.push_str("if (");
        output.push_str(&self.compile_expression(&condition.as_ref(), rhs, InLetBinding::NA));
        output.push_str(") {\n");
        output.push_str(&self.compile_statements(then_branch, rhs, variable.clone()));

        match else_branch {
            Some(Either::Left(expr)) => {
                output.push_str("\n} else ");
                output.push_str(&self.compile_if_expr(expr, rhs, variable.clone()));
            }
            Some(Either::Right(statements)) => {
                output.push_str("\n} else {\n");
                output.push_str(&self.compile_statements(statements, rhs, variable.clone()));
            }
            None => {}
        }
        output.push_str("\n}");

        output
    }

    fn compile_call_arg(&mut self, arg: &CallArg) -> String {
        let mut output = String::new();
        let CallArg { name, value, .. } = arg;
        let Either::Right(value) = value else {
            unreachable!("Call Arg should have been typechecked");
        };
        output.push_str(self.compile_expression(value, true, InLetBinding::NA).as_str());
        output
    }
}

impl Codegenerator<(String,String)> for JavaCodegenerator {
    fn generate(&mut self, file: sb_ast::core_annotated::File) -> Vec<(String, String)> {
        let mut output = Vec::new();
        self.compile_file(&file, &mut output);
        self.sub_classes.clear();
        output
    }
}
