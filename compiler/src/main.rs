
mod parser;
pub mod ast;

fn main() {
    let input = r#"module test;
import std::io;
pub fn print() {
    let x: i64 = j;
    return x;
    if x == 0 {
        return 0
    }
    x.print
    x.print()
    x.map |x: i64| -> () {
        x.print()
    }

}
"#;
    let file = parser::parse("test.txt", input).unwrap();
    println!("{:?}", file);
}
