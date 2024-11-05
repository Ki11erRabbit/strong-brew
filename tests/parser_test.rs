

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
                    variadic: false,
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
                            variadic: false,
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
                    variadic: false,
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
                            variadic: false,
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
                            variadic: false,
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
                            variadic: false,
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
                    variadic: false,
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
                            variadic: false,
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
                            variadic: false,
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
                            variadic: false,
                        },
                        start: 32,
                        end: 38,
                    },
                ],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    variadic: false,
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
                                        variadic: false,
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
                                                variadic: false,
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
                            variadic: false,
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
                            variadic: false,
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
                            variadic: false,
                            expression: Expression::Type(
                                BuiltinType::Function {
                                    params: vec![
                                        ExpressionType {
                                            variadic: false,
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
                                        variadic: false,
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
                    variadic: false,
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
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "main",
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
                        "main",
                    ],
                    start: 19,
                    end: 24,
                },
                generic_params: vec![],
                params: vec![],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::Literal(
                        Literal::Unit,
                    ),
                    variadic: false,
                }),
                body: vec![
                    Statement::Expression(
                        ExpressionType {
                            mutable: false,
                            expression: Expression::TrailingLambdas(
                                Box::new(Expression::MemberAccess {
                                    object: Box::new(Expression::Variable(
                                        PathName {
                                            segments: vec![
                                                "x",
                                            ],
                                            start: 35,
                                            end: 36,
                                        },
                                    )),
                                    field: "map",
                                    start: 35,
                                    end: 40,
                                }),
                                vec![
                                    Expression::Closure(
                                        Closure {
                                            params: vec![
                                                Param {
                                                    implicit: false,
                                                    name: "x",
                                                    ty: ExpressionType {
                                                        mutable: false,
                                                        expression: Expression::Type(
                                                            BuiltinType::I64,
                                                        ),
                                                        variadic: false,
                                                    },
                                                    start: 42,
                                                    end: 48,
                                                },
                                            ],
                                            return_type: Some(Box::new(
                                                ExpressionType {
                                                    mutable: false,
                                                    expression: Expression::Literal(
                                                        Literal::Unit,
                                                    ),
                                                    variadic: false,
                                                },
                                            )),
                                            body: vec![
                                                Statement::Expression(
                                                    ExpressionType {
                                                        mutable: false,
                                                        expression: Expression::Call(
                                                            Call {
                                                                name: Box::new(Expression::MemberAccess {
                                                                    object: Box::new(Expression::Variable(
                                                                        PathName {
                                                                            segments: vec![
                                                                                "x",
                                                                            ],
                                                                            start: 68,
                                                                            end: 69,
                                                                        },
                                                                    )),
                                                                    field: "print",
                                                                    start: 68,
                                                                    end: 75,
                                                                }),
                                                                type_args: vec![],
                                                                args: vec![],
                                                                start: 68,
                                                                end: 77,
                                                            },
                                                        ),
                                                        variadic: false,
                                                    },
                                                ),
                                            ],
                                            start: 41,
                                            end: 83,
                                        },
                                    ),
                                ],
                                35,
                                83,
                            ),
                            variadic: false,
                        },
                    ),
                ],
                start: 13,
                end: 86,
            },
        ),
    ],
});
}

#[test]
fn test_trailing_lambdas_sugary() {
    let input = r#"module main;
pub fn main() = {
    while { x < 10 } {
        x.print()
    };
}
"#;
    let file = parser::parse("test.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "main",
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
                        "main",
                    ],
                    start: 19,
                    end: 24,
                },
                generic_params: vec![],
                params: vec![],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::Literal(
                        Literal::Unit,
                    ),
                    variadic: false,
                }),
                body: vec![
                    Statement::Expression(
                        ExpressionType {
                            mutable: false,
                            expression: Expression::TrailingLambdas(
                                Box::new(Expression::Variable(
                                    PathName {
                                        segments: vec![
                                            "while",
                                        ],
                                        start: 35,
                                        end: 40,
                                    },
                                )),
                                vec![
                                    Expression::Closure(
                                        Closure {
                                            params: vec![],
                                            return_type: None,
                                            body: vec![
                                                Statement::Expression(
                                                    ExpressionType {
                                                        mutable: false,
                                                        expression: Expression::BinaryOperation {
                                                            operator: BinaryOperator::Lt,
                                                            left: Box::new(Expression::Variable(
                                                                PathName {
                                                                    segments: vec![
                                                                        "x",
                                                                    ],
                                                                    start: 43,
                                                                    end: 44,
                                                                },
                                                            )),
                                                            right: Box::new(Expression::Literal(
                                                                Literal::Int(
                                                                    "10",
                                                                ),
                                                            )),
                                                            start: 43,
                                                            end: 49,
                                                        },
                                                        variadic: false,
                                                    },
                                                ),
                                            ],
                                            start: 41,
                                            end: 51,
                                        },
                                    ),
                                    Expression::Closure(
                                        Closure {
                                            params: vec![],
                                            return_type: None,
                                            body: vec![
                                                Statement::Expression(
                                                    ExpressionType {
                                                        mutable: false,
                                                        expression: Expression::Call(
                                                            Call {
                                                                name: Box::new(Expression::MemberAccess {
                                                                    object: Box::new(Expression::Variable(
                                                                        PathName {
                                                                            segments: vec![
                                                                                "x",
                                                                            ],
                                                                            start: 62,
                                                                            end: 63,
                                                                        },
                                                                    )),
                                                                    field: "print",
                                                                    start: 62,
                                                                    end: 69,
                                                                }),
                                                                type_args: vec![],
                                                                args: vec![],
                                                                start: 62,
                                                                end: 71,
                                                            },
                                                        ),
                                                        variadic: false,
                                                    },
                                                ),
                                            ],
                                            start: 52,
                                            end: 77,
                                        },
                                    ),
                                ],
                                35,
                                77,
                            ),
                            variadic: false,
                        },
                    ),
                ],
                start: 13,
                end: 80,
            },
        ),
    ],
});
}

#[test]
fn test_variadic_function() {
    let input = r#"module arraylist;
pub fn (create[])[A](items: ...A) -> ArrayList[A] = {
    x;
}
"#;
    let file = parser::parse("arraylist.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "arraylist",
        ],
        start: 6,
        end: 16,
    },
    content: vec![
        TopLevelStatement::Function(
            Function::Regular {
                visibility: Visibility::Public,
                name: PathName {
                    segments: vec![
                        "create[]",
                    ],
                    start: 24,
                    end: 35,
                },
                generic_params: vec![
                    GenericParam {
                        name: "A",
                        constraints: vec![],
                        start: 36,
                        end: 37,
                    },
                ],
                params: vec![
                    Param {
                        implicit: false,
                        name: "items",
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Variable(
                                PathName {
                                    segments: vec![
                                        "A",
                                    ],
                                    start: 49,
                                    end: 50,
                                },
                            ),
                            variadic: true,
                        },
                        start: 39,
                        end: 50,
                    },
                ],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::Bracketed {
                        name: Box::new(Expression::Variable(
                            PathName {
                                segments: vec![
                                    "ArrayList",
                                ],
                                start: 55,
                                end: 64,
                            },
                        )),
                        expressions: vec![
                            Expression::Variable(
                                PathName {
                                    segments: vec![
                                        "A",
                                    ],
                                    start: 65,
                                    end: 66,
                                },
                            ),
                        ],
                        start: 55,
                        end: 67,
                    },
                    variadic: false,
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
                                    start: 76,
                                    end: 77,
                                },
                            ),
                            variadic: false,
                        },
                    ),
                ],
                start: 18,
                end: 80,
            },
        ),
    ],
});
}

#[test]
fn test_match() {
    let input = r#"module main;
pub fn main() = {
    match x {
        x -> x,
        0 -> {
            x;
        }
        (x, y) -> x + y,
        Maybe::Some(x) -> x,
        Maybe::None() -> 0,
    }
}
"#;
    let file = parser::parse("main.sb", input).unwrap();
    assert_eq!(file,File {
    path: PathName {
        segments: vec![
            "main",
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
                        "main",
                    ],
                    start: 19,
                    end: 24,
                },
                generic_params: vec![],
                params: vec![],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::Literal(
                        Literal::Unit,
                    ),
                    variadic: false,
                }),
                body: vec![
                    Statement::Expression(
                        ExpressionType {
                            mutable: false,
                            expression: Expression::MatchExpression(
                                MatchExpression {
                                    value: Box::new(Expression::Variable(
                                        PathName {
                                            segments: vec![
                                                "x",
                                            ],
                                            start: 41,
                                            end: 42,
                                        },
                                    )),
                                    arms: vec![
                                        MatchArm {
                                            pattern: Pattern::Variable(
                                                "x",
                                            ),
                                            value: Either::Left(
                                                ExpressionType {
                                                    mutable: false,
                                                    expression: Expression::Variable(
                                                        PathName {
                                                            segments: vec![
                                                                "x",
                                                            ],
                                                            start: 58,
                                                            end: 59,
                                                        },
                                                    ),
                                                    variadic: false,
                                                },
                                            ),
                                            start: 53,
                                            end: 60,
                                        },
                                        MatchArm {
                                            pattern: Pattern::Literal(
                                                Literal::Int(
                                                    "0",
                                                ),
                                            ),
                                            value: Either::Right(
                                                vec![
                                                    Statement::Expression(
                                                        ExpressionType {
                                                            mutable: false,
                                                            expression: Expression::Variable(
                                                                PathName {
                                                                    segments: vec![
                                                                        "x",
                                                                    ],
                                                                    start: 88,
                                                                    end: 89,
                                                                },
                                                            ),
                                                            variadic: false,
                                                        },
                                                    ),
                                                ],
                                            ),
                                            start: 69,
                                            end: 100,
                                        },
                                        MatchArm {
                                            pattern: Pattern::Tuple(
                                                vec![
                                                    Pattern::Variable(
                                                        "x",
                                                    ),
                                                    Pattern::Variable(
                                                        "y",
                                                    ),
                                                ],
                                            ),
                                            value: Either::Left(
                                                ExpressionType {
                                                    mutable: false,
                                                    expression: Expression::BinaryOperation {
                                                        operator: BinaryOperator::Add,
                                                        left: Box::new(Expression::Variable(
                                                            PathName {
                                                                segments: vec![
                                                                    "x",
                                                                ],
                                                                start: 119,
                                                                end: 120,
                                                            },
                                                        )),
                                                        right: Box::new(Expression::Variable(
                                                            PathName {
                                                                segments: vec![
                                                                    "y",
                                                                ],
                                                                start: 123,
                                                                end: 124,
                                                            },
                                                        )),
                                                        start: 119,
                                                        end: 124,
                                                    },
                                                    variadic: false,
                                                },
                                            ),
                                            start: 109,
                                            end: 125,
                                        },
                                        MatchArm {
                                            pattern: Pattern::Constructor {
                                                name: PathName {
                                                    segments: vec![
                                                        "Maybe",
                                                        "Some",
                                                    ],
                                                    start: 134,
                                                    end: 145,
                                                },
                                                fields: vec![
                                                    Pattern::Variable(
                                                        "x",
                                                    ),
                                                ],
                                            },
                                            value: Either::Left(
                                                ExpressionType {
                                                    mutable: false,
                                                    expression: Expression::Variable(
                                                        PathName {
                                                            segments: vec![
                                                                "x",
                                                            ],
                                                            start: 152,
                                                            end: 153,
                                                        },
                                                    ),
                                                    variadic: false,
                                                },
                                            ),
                                            start: 134,
                                            end: 154,
                                        },
                                        MatchArm {
                                            pattern: Pattern::Constructor {
                                                name: PathName {
                                                    segments: vec![
                                                        "Maybe",
                                                        "None",
                                                    ],
                                                    start: 163,
                                                    end: 174,
                                                },
                                                fields: vec![],
                                            },
                                            value: Either::Left(
                                                ExpressionType {
                                                    mutable: false,
                                                    expression: Expression::Literal(
                                                        Literal::Int(
                                                            "0",
                                                        ),
                                                    ),
                                                    variadic: false,
                                                },
                                            ),
                                            start: 163,
                                            end: 182,
                                        },
                                    ],
                                    start: 35,
                                    end: 188,
                                },
                            ),
                            variadic: false,
                        },
                    ),
                ],
                start: 13,
                end: 190,
            },
        ),
    ],
});
}

#[test]
fn test_function_call() {
    let input = r#"module main;
pub fn main() = {
    x(2, 3, j = 4);
}
"#;
    let file = parser::parse("main.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "main",
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
                        "main",
                    ],
                    start: 19,
                    end: 24,
                },
                generic_params: vec![],
                params: vec![],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::Literal(
                        Literal::Unit,
                    ),
                    variadic: false,
                }),
                body: vec![
                    Statement::Expression(
                        ExpressionType {
                            mutable: false,
                            expression: Expression::Call(
                                Call {
                                    name: Box::new(Expression::Variable(
                                        PathName {
                                            segments: vec![
                                                "x",
                                            ],
                                            start: 35,
                                            end: 36,
                                        },
                                    )),
                                    type_args: vec![],
                                    args: vec![
                                        CallArg {
                                            name: None,
                                            value: Expression::Literal(
                                                Literal::Int(
                                                    "2",
                                                ),
                                            ),
                                            start: 37,
                                            end: 38,
                                        },
                                        CallArg {
                                            name: None,
                                            value: Expression::Literal(
                                                Literal::Int(
                                                    "3",
                                                ),
                                            ),
                                            start: 40,
                                            end: 41,
                                        },
                                        CallArg {
                                            name: Some(
                                                "j",
                                            ),
                                            value: Expression::Literal(
                                                Literal::Int(
                                                    "4",
                                                ),
                                            ),
                                            start: 43,
                                            end: 48,
                                        },
                                    ],
                                    start: 35,
                                    end: 49,
                                },
                            ),
                            variadic: false,
                        },
                    ),
                ],
                start: 13,
                end: 52,
            },
        ),
    ],
});
}

#[test]
fn test_struct() {
    let input = r#"module arraylist;
pub struct Arraylist[A] {
    items: List[A],
    size: nat,
}
"#;
    let file = parser::parse("arraylist.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "arraylist",
        ],
        start: 6,
        end: 16,
    },
    content: vec![
        TopLevelStatement::Struct(
            Struct {
                visibility: Visibility::Public,
                name: "Arraylist",
                generic_params: vec![
                    GenericParam {
                        name: "A",
                        constraints: vec![],
                        start: 39,
                        end: 40,
                    },
                ],
                fields: vec![
                    Field {
                        visibility: Visibility::Private,
                        name: "items",
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Bracketed {
                                name: Box::new(Expression::Variable(
                                    PathName {
                                        segments: vec![
                                            "List",
                                        ],
                                        start: 55,
                                        end: 59,
                                    },
                                )),
                                expressions: vec![
                                    Expression::Variable(
                                        PathName {
                                            segments: vec![
                                                "A",
                                            ],
                                            start: 60,
                                            end: 61,
                                        },
                                    ),
                                ],
                                start: 55,
                                end: 62,
                            },
                            variadic: false,
                        },
                        start: 48,
                        end: 62,
                    },
                    Field {
                        visibility: Visibility::Private,
                        name: "size",
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Type(
                                BuiltinType::Nat,
                            ),
                            variadic: false,
                        },
                        start: 68,
                        end: 77,
                    },
                ],
                start: 18,
                end: 80,
            },
        ),
    ],
});
}

#[test]
fn test_enum() {
    let input = r#"module linkedlist;
pub enum Linkedlist[A] {
    Cons(car: A, cdr: Linkedlist[A]),
    Nil,
}
"#;
    let file = parser::parse("arraylist.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "linkedlist",
        ],
        start: 6,
        end: 17,
    },
    content: vec![
        TopLevelStatement::Enum(
            Enum {
                visibility: Visibility::Public,
                name: "Linkedlist",
                generic_params: vec![
                    GenericParam {
                        name: "A",
                        constraints: vec![],
                        start: 39,
                        end: 40,
                    },
                ],
                variants: vec![
                    Variant {
                        name: "Cons",
                        fields: vec![
                            Field {
                                visibility: Visibility::Private,
                                name: "car",
                                ty: ExpressionType {
                                    mutable: false,
                                    expression: Expression::Variable(
                                        PathName {
                                            segments: vec![
                                                "A",
                                            ],
                                            start: 58,
                                            end: 59,
                                        },
                                    ),
                                    variadic: false,
                                },
                                start: 53,
                                end: 59,
                            },
                            Field {
                                visibility: Visibility::Private,
                                name: "cdr",
                                ty: ExpressionType {
                                    mutable: false,
                                    expression: Expression::Bracketed {
                                        name: Box::new(Expression::Variable(
                                            PathName {
                                                segments: vec![
                                                    "Linkedlist",
                                                ],
                                                start: 66,
                                                end: 76,
                                            },
                                        )),
                                        expressions: vec![
                                            Expression::Variable(
                                                PathName {
                                                    segments: vec![
                                                        "A",
                                                    ],
                                                    start: 77,
                                                    end: 78,
                                                },
                                            ),
                                        ],
                                        start: 66,
                                        end: 79,
                                    },
                                    variadic: false,
                                },
                                start: 61,
                                end: 79,
                            },
                        ],
                        start: 48,
                        end: 80,
                    },
                    Variant {
                        name: "Nil",
                        fields: vec![],
                        start: 86,
                        end: 89,
                    },
                ],
                start: 19,
                end: 92,
            },
        ),
    ],
});
}

#[test]
fn test_const() {
    let input = r#"module main;
const x: i64 = 10;
"#;
    let file = parser::parse("main.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "main",
        ],
        start: 6,
        end: 11,
    },
    content: vec![
        TopLevelStatement::Const(
            Const {
                visibility: Visibility::Private,
                name: PathName {
                    segments: vec![
                        "x",
                    ],
                    start: 18,
                    end: 18,
                },
                ty: ExpressionType {
                    mutable: false,
                    expression: Expression::Type(
                        BuiltinType::I64,
                    ),
                    variadic: false,
                },
                value: Expression::Literal(
                    Literal::Int(
                        "10",
                    ),
                ),
                start: 13,
                end: 30,
            },
        ),
    ],
});
}

#[test]
fn test_unary() {
    let input = r#"module main;
const x: i64 = 10?;
"#;
    let file = parser::parse("main.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "main",
        ],
        start: 6,
        end: 11,
    },
    content: vec![
        TopLevelStatement::Const(
            Const {
                visibility: Visibility::Private,
                name: PathName {
                    segments: vec![
                        "x",
                    ],
                    start: 18,
                    end: 18,
                },
                ty: ExpressionType {
                    mutable: false,
                    expression: Expression::Type(
                        BuiltinType::I64,
                    ),
                    variadic: false,
                },
                value: Expression::UnaryOperation {
                    operator: UnaryOperator::Try,
                    operand: Box::new(Expression::Literal(
                        Literal::Int(
                            "10",
                        ),
                    )),
                    start: 28,
                    end: 31,
                },
                start: 13,
                end: 31,
            },
        ),
    ],
});
}

#[test]
fn test_statements() {
    let input = r#"module main;
pub fn main() = {
    let (x, y): (mut i64, i64) = (1, 2);
    const z: i64 = 3;
    x = 4;
}
"#;
    let file = parser::parse("main.sb", input).unwrap();
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "main",
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
                        "main",
                    ],
                    start: 19,
                    end: 24,
                },
                generic_params: vec![],
                params: vec![],
                return_type: Box::new(ExpressionType {
                    mutable: false,
                    expression: Expression::Literal(
                        Literal::Unit,
                    ),
                    variadic: false,
                }),
                body: vec![
                    Statement::Let {
                        name: Pattern::Tuple(
                            vec![
                                Pattern::Variable(
                                    "x",
                                ),
                                Pattern::Variable(
                                    "y",
                                ),
                            ],
                        ),
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Literal(
                                Literal::Tuple(
                                    vec![
                                        ExpressionType {
                                            mutable: true,
                                            expression: Expression::Type(
                                                BuiltinType::I64,
                                            ),
                                            variadic: false,
                                        },
                                        ExpressionType {
                                            mutable: false,
                                            expression: Expression::Type(
                                                BuiltinType::I64,
                                            ),
                                            variadic: false,
                                        },
                                    ],
                                ),
                            ),
                            variadic: false,
                        },
                        value: ExpressionType {
                            mutable: false,
                            expression: Expression::Literal(
                                Literal::Tuple(
                                    vec![
                                        ExpressionType {
                                            mutable: false,
                                            expression: Expression::Literal(
                                                Literal::Int(
                                                    "1",
                                                ),
                                            ),
                                            variadic: false,
                                        },
                                        ExpressionType {
                                            mutable: false,
                                            expression: Expression::Literal(
                                                Literal::Int(
                                                    "2",
                                                ),
                                            ),
                                            variadic: false,
                                        },
                                    ],
                                ),
                            ),
                            variadic: false,
                        },
                        start: 35,
                        end: 70,
                    },
                    Statement::Const {
                        name: "z",
                        ty: ExpressionType {
                            mutable: false,
                            expression: Expression::Type(
                                BuiltinType::I64,
                            ),
                            variadic: false,
                        },
                        value: ExpressionType {
                            mutable: false,
                            expression: Expression::Literal(
                                Literal::Int(
                                    "3",
                                ),
                            ),
                            variadic: false,
                        },
                        start: 76,
                        end: 92,
                    },
                    Statement::Assignment {
                        target: ExpressionType {
                            mutable: false,
                            expression: Expression::Variable(
                                PathName {
                                    segments: vec![
                                        "x",
                                    ],
                                    start: 98,
                                    end: 99,
                                },
                            ),
                            variadic: false,
                        },
                        value: ExpressionType {
                            mutable: false,
                            expression: Expression::Literal(
                                Literal::Int(
                                    "4",
                                ),
                            ),
                            variadic: false,
                        },
                        start: 98,
                        end: 103,
                    },
                ],
                start: 13,
                end: 106,
            },
        ),
    ],
});
}
