
mod parser;
pub mod ast;

fn main() {
    let input = r#"module test;
import std::io;
pub fn print() {
    let x: i64 = j;
    return x

}
"#;
    let file = parser::parse("test.txt", input).unwrap();
    println!("{:?}", file);
}
