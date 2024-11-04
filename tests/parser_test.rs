

use crate::parser::parse;


#[test]
fn test_module() {
    let input = r#"module test;"#;
    let file = parser::parse("test.sb", input).unwrap();

}
