mod typechecker;

use backend::JavaCodegenerator;
use backend::Codegenerator;


fn main() {

    let args: Vec<String> = std::env::args().collect();

    let file = std::fs::read_to_string(&args[1]).unwrap();

    println!("Parsing file: {}", &args[1]);
    let result = parser::parse(&args[1], &file);
    if let Err(_) = result {
        std::process::exit(1);
    }
    let file = result.unwrap();
    println!("Restricting Ast");
    let file = sb_ast::convert_outer_to_inner(file);
    if let Err(_) = file {
        todo!("Report error")
    }
    let file = file.unwrap();
    println!("Desugaring Ast");
    let file = sb_ast::convert_inner_to_core(file);
    println!("Typechecking Ast");

    let mut typechecker = typechecker::TypeChecker::new();
    let files = typechecker.check_files(&vec![(&args[1], file)]);
    if let Err(e) = files {
        println!("{:?}", e);
        std::process::exit(1);
    }
    let files = files.unwrap();
    let mut code_generator = JavaCodegenerator::new();
    for file in files {
        let code = code_generator.generate(file);
        println!("\n{}", code);
    }

}
