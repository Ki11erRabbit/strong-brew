

use parser;
use sb_ast::outer;

#[test]
fn test_module() {
    let input = r#"module test;"#;
    let file = parser::parse("test.sb", input).unwrap();

    assert_eq!(file, outer::File {
        path: outer::PathName::new(vec!["test"], 6, 11),
        content: Vec::new(),
    });
}

#[test]
fn test_module_with_import() {
    let input = r#"module test;
import test2;"#;
    let file = parser::parse("test.sb", input).unwrap();

    assert_eq!(file, outer::File {
        path: outer::PathName::new(vec!["test"], 6, 11),
        content: vec![
            outer::Import::new(outer::PathName::new(vec!["test2"], 19, 25), 13, 25),
            ],
    });
}

#[test]
fn test_module_scope() {
    let input = r#"module test::inner;"#;
    let file = parser::parse("test.sb", input).unwrap();

    assert_eq!(file, outer::File {
        path: outer::PathName::new(vec!["test", "inner"], 7, 18),
        content: Vec::new(),
    });
}

#[test]
fn test_module_with_import_scope() {
    let input = r#"module test::inner;
import test2::inner;"#;
    let file = parser::parse("test.sb", input).unwrap();

    assert_eq!(file, outer::File {
        path: outer::PathName::new(vec!["test", "inner"], 7, 18),
        content: vec![
            outer::Import::new(outer::PathName::new(vec!["test2", "inner"], 27, 39), 20, 39),
            ],
    });
}

#[test]
fn test_function() {
    let input = r#"module test;
pub fn print() = {
    x;
}
"#;
    let file = parser::parse("test.sb", input).unwrap();
    println!("{:#?}", file);
}
/*r#"module test;
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

pub fn (-)(x: nat, y: nat) -> if int > x { return int } else { return nat } = {
    return x - y;
}
"#;
    let file = parser::parse("test.txt", input).unwrap();
    println!("{:#?}", file);
}*/

