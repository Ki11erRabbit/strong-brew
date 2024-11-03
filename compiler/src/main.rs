
mod parser;
pub mod ast;

fn main() {
    let input = r#"module test;
import std::io;
pub fn print() = {
    let x: i64 = j;
    return x;
    if x == 0 {
        return 0
    }
    x.print
    x.print()
    x.map |x: i64| -> () = {
        x.print()
    };

}
pub fn (==)[A](x: A, y: A) = {
    return x == y;
}

pub fn (set[])[A](list: list[A], index: i64, value: A) = {
    list[index] = value;
}

pub fn (-)(x: nat, y: nat) -> if y > x { return int } else { return nat } = {
    return x - y;
}
"#;
    let file = parser::parse("test.txt", input).unwrap();
    println!("{:#?}", file);
}
