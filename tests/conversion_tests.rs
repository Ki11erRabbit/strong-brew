use sb_ast::core_lang::{self, *};






fn string_to_file<'a>(name: &'a str, input: &'a str) -> Result<core_lang::File<'a>, ()> {
    let parse_result = parser::parse(name, input);
    if let Err(_) = parse_result {
        return Err(());
    }

    let file = parse_result.unwrap();
    let file = sb_ast::convert_outer_to_inner(file);
    if let Err(_) = file {
        return Err(());
    }
    let file = file.unwrap();
    let file = sb_ast::convert_inner_to_core(file);
    Ok(file)
}




#[test]
fn test_dotted_functions() {
    let name = "test_dotted_functions.sb";
    let input = r#"module test_dotted_functions
fn main() = {
    x.show.print;
}
"#;
    let file = string_to_file(name, input);
    assert!(file.is_ok());
    let file = file.unwrap();
    println!("{:#?}", file);
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "test_dotted_functions",
        ],
        start: 6,
        end: 28,
    },
    content: vec![
        TopLevelStatement::Function(
            Function::Regular {
                visibility: Visibility::Private,
                name: PathName {
                    segments: vec![
                        "main",
                    ],
                    start: 31,
                    end: 36,
                },
                generic_params: vec![],
                params: vec![],
                return_type: ExpressionType {
                    expression: Expression::Literal(
                        Literal::Unit,
                    ),
                    variadic: false,
                },
                body: vec![
                    Statement::Expression(
                        ExpressionType {
                            expression: Expression::Call(
                                Call {
                                    name: Box::new(Expression::Variable(
                                        PathName {
                                            segments: vec![
                                                "print",
                                            ],
                                            start: 47,
                                            end: 59,
                                        },
                                    )),
                                    type_args: vec![],
                                    args: vec![
                                        CallArg {
                                            name: None,
                                            value: Expression::Call(
                                                Call {
                                                    name: Box::new(Expression::Variable(
                                                        PathName {
                                                            segments: vec![
                                                                "show",
                                                            ],
                                                            start: 47,
                                                            end: 53,
                                                        },
                                                    )),
                                                    type_args: vec![],
                                                    args: vec![
                                                        CallArg {
                                                            name: None,
                                                            value: Expression::Variable(
                                                                PathName {
                                                                    segments: vec![
                                                                        "x",
                                                                    ],
                                                                    start: 47,
                                                                    end: 48,
                                                                },
                                                            ),
                                                            start: 0,
                                                            end: 0,
                                                        },
                                                    ],
                                                    start: 0,
                                                    end: 0,
                                                },
                                            ),
                                            start: 0,
                                            end: 0,
                                        },
                                    ],
                                    start: 0,
                                    end: 0,
                                },
                            ),
                            variadic: false,
                        },
                    ),
                ],
                start: 31,
                end: 36,
            },
        ),
    ],
});
}

#[test]
fn test_trailing_lambdas() {
    let name = "test_trailing_lambdas.sb";
    let input = r#"module test_trailing_lambdas
fn main() = {
    map([1, 2, 3]) |x: i64| -> i64 = { x + 1 };
}
"#;
    let file = string_to_file(name, input);
    assert!(file.is_ok());
    let file = file.unwrap();
    println!("{:#?}", file);
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "test_trailing_lambdas",
        ],
        start: 6,
        end: 28,
    },
    content: vec![
        TopLevelStatement::Function(
            Function::Regular {
                visibility: Visibility::Private,
                name: PathName {
                    segments: vec![
                        "main",
                    ],
                    start: 31,
                    end: 36,
                },
                generic_params: vec![],
                params: vec![],
                return_type: ExpressionType {
                    expression: Expression::Literal(
                        Literal::Unit,
                    ),
                    variadic: false,
                },
                body: vec![
                    Statement::Expression(
                        ExpressionType {
                            expression: Expression::Call(
                                Call {
                                    name: Box::new(Expression::Variable(
                                        PathName {
                                            segments: vec![
                                                "map",
                                            ],
                                            start: 47,
                                            end: 50,
                                        },
                                    )),
                                    type_args: vec![],
                                    args: vec![
                                        CallArg {
                                            name: None,
                                            value: Expression::Literal(
                                                Literal::List(
                                                    vec![
                                                        Expression::Literal(
                                                            Literal::Int(
                                                                "1",
                                                            ),
                                                        ),
                                                        Expression::Literal(
                                                            Literal::Int(
                                                                "2",
                                                            ),
                                                        ),
                                                        Expression::Literal(
                                                            Literal::Int(
                                                                "3",
                                                            ),
                                                        ),
                                                    ],
                                                ),
                                            ),
                                            start: 51,
                                            end: 60,
                                        },
                                        CallArg {
                                            name: None,
                                            value: Expression::Closure(
                                                Closure {
                                                    params: vec![
                                                        Param {
                                                            implicit: false,
                                                            name: "x",
                                                            ty: ExpressionType {
                                                                expression: Expression::Type(
                                                                    Type::Builtin(
                                                                        BuiltinType::I64,
                                                                    ),
                                                                ),
                                                                variadic: false,
                                                            },
                                                            start: 63,
                                                            end: 69,
                                                        },
                                                    ],
                                                    return_type: Some(
                                                        Box::new(ExpressionType {
                                                            expression: Expression::Type(
                                                                Type::Builtin(
                                                                    BuiltinType::I64,
                                                                ),
                                                            ),
                                                            variadic: false,
                                                        }),
                                                    ),
                                                    body: vec![
                                                        Statement::Expression(
                                                            ExpressionType {
                                                                expression: Expression::BinaryOperation {
                                                                    operator: BinaryOperator::Add,
                                                                    left: Box::new(Expression::Variable(
                                                                        PathName {
                                                                            segments: vec![
                                                                                "x",
                                                                            ],
                                                                            start: 82,
                                                                            end: 83,
                                                                        },
                                                                    )),
                                                                    right: Box::new(Expression::Literal(
                                                                        Literal::Int(
                                                                            "1",
                                                                        ),
                                                                    )),
                                                                    start: 82,
                                                                    end: 87,
                                                                },
                                                                variadic: false,
                                                            },
                                                        ),
                                                    ],
                                                    start: 62,
                                                    end: 89,
                                                },
                                            ),
                                            start: 0,
                                            end: 0,
                                        },
                                    ],
                                    start: 47,
                                    end: 61,
                                },
                            ),
                            variadic: false,
                        },
                    ),
                ],
                start: 31,
                end: 36,
            },
        ),
    ],
    });
}

#[test]
fn test_struct_becomes_enum() {
    let name = "test_struct_becomes_enum.sb";
    let input = r#"module test_struct_becomes_enum
pub struct Arraylist[T] {
    data: Array[T],
    pub length: nat,
}
"#;
    let file = string_to_file(name, input);
    assert!(file.is_ok());
    let file = file.unwrap();
    println!("{:#?}", file);
    assert_eq!(file, File {
    path: PathName {
        segments: vec![
            "test_struct_becomes_enum",
        ],
        start: 6,
        end: 31,
    },
    content: vec![
        TopLevelStatement::Enum(
            Enum {
                visibility: Visibility::Public,
                name: "Arraylist",
                generic_params: vec![
                    GenericParam {
                        name: "T",
                        constraints: vec![],
                        start: 53,
                        end: 54,
                    },
                ],
                variants: vec![
                    Variant {
                        name: "Arraylist",
                        fields: vec![
                            Field {
                                visibility: Visibility::Private,
                                name: "data",
                                ty: ExpressionType {
                                    expression: Expression::Type(
                                        Type::Parameterized(
                                            Box::new(Type::User(
                                                PathName {
                                                    segments: vec![
                                                        "Array",
                                                    ],
                                                    start: 68,
                                                    end: 73,
                                                },
                                            )),
                                            vec![
                                                Expression::Constant(
                                                    PathName {
                                                        segments: vec![
                                                            "T",
                                                        ],
                                                        start: 74,
                                                        end: 75,
                                                    },
                                                ),
                                            ],
                                        ),
                                    ),
                                    variadic: false,
                                },
                                start: 62,
                                end: 76,
                            },
                            Field {
                                visibility: Visibility::Public,
                                name: "length",
                                ty: ExpressionType {
                                    expression: Expression::Type(
                                        Type::Builtin(
                                            BuiltinType::Nat,
                                        ),
                                    ),
                                    variadic: false,
                                },
                                start: 82,
                                end: 97,
                            },
                        ],
                        start: 32,
                        end: 100,
                    },
                ],
                start: 32,
                end: 100,
            },
        ),
    ],
});
}
