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
    println!("Printing Ast");
    println!("{:#?}", file);

}
