

use parser;
use sb_ast::outer::{self, *};
use either::Either;

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
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "test",
        ],
        start: 6,
        end: 11,
    },
    content: vec![
        TopLevelStatement::Function(
            Function::Regular {
                visibility: Visibility::Public,
                name: PathName {
                    segments: vec![
                        "print",
                    ],
                    start: 19,
                    end: 25,
                },
                generic_params: vec![],
                params: vec![],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::Literal(
                        Literal::Unit,
                    ),
                }),
                body: vec![
                    Statement::Expression(
                        ExpressionType {
                            mutable: false,
                            expression: Expression::Variable(
                                PathName {
                                    segments: vec![
                                        "x",
                                    ],
                                    start: 36,
                                    end: 37,
                                },
                            ),
                        },
                    ),
                ],
                start: 13,
                end: 40,
            },
        ),
    ],
});
}

#[test]
fn test_function_scoped() {
    let input = r#"module test;
pub fn out::print() = {
    x;
}
"#;
    let file = parser::parse("test.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "test",
        ],
        start: 6,
        end: 11,
    },
    content: vec![
        TopLevelStatement::Function(
            Function::Regular {
                visibility: Visibility::Public,
                name: PathName {
                    segments: vec![
                        "out",
                        "print",
                    ],
                    start: 20,
                    end: 30,
                },
                generic_params: vec![],
                params: vec![],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::Literal(
                        Literal::Unit,
                    ),
                }),
                body: vec![
                    Statement::Expression(
                        ExpressionType {
                            mutable: false,
                            expression: Expression::Variable(
                                PathName {
                                    segments: vec![
                                        "x",
                                    ],
                                    start: 41,
                                    end: 42,
                                },
                            ),
                        },
                    ),
                ],
                start: 13,
                end: 45,
            },
        ),
    ],
});
}

#[test]
fn test_operator_function() {
    let input = r#"module test;
pub fn (==)(x: A, y: A) = {
    x;
}
"#;
    let file = parser::parse("test.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "test",
        ],
        start: 6,
        end: 11,
    },
    content: vec![
        TopLevelStatement::Function(
            Function::Regular {
                visibility: Visibility::Public,
                name: PathName {
                    segments: vec![
                        "==",
                    ],
                    start: 19,
                    end: 24,
                },
                generic_params: vec![],
                params: vec![
                    Param {
                        implicit: false,
                        name: "x",
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Variable(
                                PathName {
                                    segments: vec![
                                        "A",
                                    ],
                                    start: 28,
                                    end: 29,
                                },
                            ),
                        },
                        start: 25,
                        end: 29,
                    },
                    Param {
                        implicit: false,
                        name: "y",
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Variable(
                                PathName {
                                    segments: vec![
                                        "A",
                                    ],
                                    start: 34,
                                    end: 35,
                                },
                            ),
                        },
                        start: 31,
                        end: 35,
                    },
                ],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::Literal(
                        Literal::Unit,
                    ),
                }),
                body: vec![
                    Statement::Expression(
                        ExpressionType {
                            mutable: false,
                            expression: Expression::Variable(
                                PathName {
                                    segments: vec![
                                        "x",
                                    ],
                                    start: 45,
                                    end: 46,
                                },
                            ),
                        },
                    ),
                ],
                start: 13,
                end: 49,
            },
        ),
    ],
});
}

#[test]
fn test_dependent_function() {
    let input = r#"module test;
pub fn (-)(x: nat, y: nat) -> if y > x { return int } else { return nat } = {
    x;
}
"#;
    let file = parser::parse("test.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "test",
        ],
        start: 6,
        end: 11,
    },
    content: vec![
        TopLevelStatement::Function(
            Function::Regular {
                visibility: Visibility::Public,
                name: PathName {
                    segments: vec![
                        "-",
                    ],
                    start: 19,
                    end: 23,
                },
                generic_params: vec![],
                params: vec![
                    Param {
                        implicit: false,
                        name: "x",
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Type(
                                BuiltinType::Nat,
                            ),
                        },
                        start: 24,
                        end: 30,
                    },
                    Param {
                        implicit: false,
                        name: "y",
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Type(
                                BuiltinType::Nat,
                            ),
                        },
                        start: 32,
                        end: 38,
                    },
                ],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::IfExpression(
                        IfExpression {
                            condition: Box::new(Expression::BinaryOperation {
                                operator: BinaryOperator::Gt,
                                left: Box::new(Expression::Variable(
                                    PathName {
                                        segments: vec![
                                            "y",
                                        ],
                                        start: 46,
                                        end: 47,
                                    },
                                )),
                                right: Box::new(Expression::Variable(
                                    PathName {
                                        segments: vec![
                                            "x",
                                        ],
                                        start: 50,
                                        end: 51,
                                    },
                                )),
                                start: 46,
                                end: 51,
                            }),
                            then_branch: vec![
                                Statement::Expression(
                                    ExpressionType {
                                        mutable: false,
                                        expression: Expression::Return(
                                            Box::new(Expression::Type(
                                                BuiltinType::Int,
                                            ),
                                        )),
                                    },
                                ),
                            ],
                            else_branch: Some(
                                Either::Right(
                                    vec![
                                        Statement::Expression(
                                            ExpressionType {
                                                mutable: false,
                                                expression: Expression::Return(
                                                    Box::new(Expression::Type(
                                                        BuiltinType::Nat,
                                                    ),
                                                )),
                                            },
                                        ),
                                    ],
                                ),
                            ),
                            start: 43,
                            end: 86,
                        },
                    ),
                }),
                body: vec![
                    Statement::Expression(
                        ExpressionType {
                            mutable: false,
                            expression: Expression::Variable(
                                PathName {
                                    segments: vec![
                                        "x",
                                    ],
                                    start: 95,
                                    end: 96,
                                },
                            ),
                        },
                    ),
                ],
                start: 13,
                end: 99,
            },
        ),
    ],
});
}

#[test]
fn test_higher_order_function() {
    let input = r#"module list;
pub extern "Java" fn map[A, B](l: List[A], f: fn B (A)) -> List[B] = "
List<B> result = new ArrayList<>();
for (A a : l) {
    result.add(f(a));
}
return result;"
"#;
    let file = parser::parse("test.sb", input).unwrap();
    assert_eq!(file,File {
    path: PathName {
        segments: vec![
            "list",
        ],
        start: 6,
        end: 11,
    },
    content: vec![
        TopLevelStatement::Function(
            Function::Extern {
                visibility: Visibility::Public,
                language: "\"Java\"",
                name: PathName {
                    segments: vec![
                        "map",
                    ],
                    start: 33,
                    end: 37,
                },
                generic_params: vec![
                    GenericParam {
                        name: "A",
                        constraints: vec![],
                        start: 38,
                        end: 39,
                    },
                    GenericParam {
                        name: "B",
                        constraints: vec![],
                        start: 41,
                        end: 42,
                    },
                ],
                params: vec![
                    Param {
                        implicit: false,
                        name: "l",
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Bracketed {
                                name: Box::new(Expression::Variable(
                                    PathName {
                                        segments: vec![
                                            "List",
                                        ],
                                        start: 47,
                                        end: 51,
                                    },
                                )),
                                expressions: vec![
                                    Expression::Variable(
                                        PathName {
                                            segments: vec![
                                                "A",
                                            ],
                                            start: 52,
                                            end: 53,
                                        },
                                    ),
                                ],
                                start: 47,
                                end: 54,
                            },
                        },
                        start: 44,
                        end: 54,
                    },
                    Param {
                        implicit: false,
                        name: "f",
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Type(
                                BuiltinType::Function {
                                    params: vec![
                                        ExpressionType {
                                            mutable: false,
                                            expression: Expression::Variable(
                                                PathName {
                                                    segments: vec![
                                                        "A",
                                                    ],
                                                    start: 65,
                                                    end: 66,
                                                },
                                            ),
                                        },
                                    ],
                                    return_type: Box::new(ExpressionType {
                                        mutable: false,
                                        expression: Expression::Variable(
                                            PathName {
                                                segments: vec![
                                                    "B",
                                                ],
                                                start: 62,
                                                end: 63,
                                            },
                                        ),
                                    }),
                                },
                            ),
                        },
                        start: 56,
                        end: 67,
                    },
                ],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::Bracketed {
                        name: Box::new(Expression::Variable(
                            PathName {
                                segments: vec![
                                    "List",
                                ],
                                start: 72,
                                end: 76,
                            },
                        )),
                        expressions: vec![
                            Expression::Variable(
                                PathName {
                                    segments: vec![
                                        "B",
                                    ],
                                    start: 77,
                                    end: 78,
                                },
                            ),
                        ],
                        start: 72,
                        end: 79,
                    },
                }),
                body: "\"\nList<B> result = new ArrayList<>();\nfor (A a : l) {\n    result.add(f(a));\n}\nreturn result;\"",
                start: 13,
                end: 175,
            },
        ),
    ],
});
}

#[test]
fn test_trailing_lambdas() {
    let input = r#"module main;
pub fn main() = {
    x.map |x: i64| -> () = {
        x.print()
    };
}
"#;
    let file = parser::parse("test.sb", input).unwrap();
    println!("{:#?}", file);
    panic!();
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

