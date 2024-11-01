
mod parser;
pub mod ast;

fn main() {
    let input = r#"module test;
import std::io;
"#;
    let file = parser::parse("test.txt", input).unwrap();
    println!("{:?}", file);
}
