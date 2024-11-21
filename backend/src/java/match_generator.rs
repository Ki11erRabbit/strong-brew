use either::Either;
use sb_ast::core_annotated::{BuiltinType, Expression, Literal, MatchArm, MatchExpr, Pattern, Statement, Type};



pub struct SwitchTree<'a> {
    internal: SwitchTreeInternal<'a>,
}

impl SwitchTree<'_> {
    pub fn from_match_expr<'a>(expr: &'a MatchExpr) -> SwitchTree<'a> {
        let MatchExpr { value, arms, .. } = expr;
        let Expression { raw, ty } = value.as_ref();
        let root = match &*ty.borrow() {
            Type::Builtin(BuiltinType::Bool) => {
                let (matcher, branches) = Self::convert_arms(arms, *value.clone());
                SwitchTree {
                    internal: SwitchTreeInternal::Root {
                        root: SwitchTreeRoot::RootUntagged(matcher),
                        branches,
                    }
                }
            }
        };
        todo!()
    }

    fn convert_arms<'a>(arms: &'a Vec<MatchArm>, matcher: Expression) -> (Expression, Vec<SwitchBranch<'a>>) {
        let mut output = Vec::new();
        let mut output_matcher = None;
        for arm in arms {
            let MatchArm { pattern, value, .. } = arm;
            let branch = match pattern {
                Pattern::Literal(Literal::Bool(b)) => {
                    SwitchBranch::Literal(b.to_string(), SwitchTree {
                        internal: SwitchTreeInternal::Body {
                            body: value,
                        }
                    })
                }
                Pattern::Literal(Literal::Char(c)) => {
                    let mut string = String::from("'");
                    string.push_str(c);
                    string.push('\'');
                    SwitchBranch::Literal(string, SwitchTree {
                        internal: SwitchTreeInternal::Body {
                            body: value,
                        }
                    })
                }
                Pattern::Literal(Literal::Int(i)) => {
                    SwitchBranch::Literal(i.to_string(), SwitchTree {
                        internal: SwitchTreeInternal::Body {
                            body: value,
                        }
                    })
                }
                Pattern::Literal(Literal::String(s)) => {
                    let mut string = String::from("\"");
                    string.push_str(s);
                    string.push('\"');
                    SwitchBranch::Literal(string, SwitchTree {
                        internal: SwitchTreeInternal::Body {
                            body: value,
                        }
                    })
                }
                Pattern::Wildcard => {
                    SwitchBranch::Default(SwitchTree {
                        internal: SwitchTreeInternal::Body {
                            body: value,
                        }
                    })
                }
                Pattern::Variable(v) => {
                    SwitchBranch::Variable(v.clone(), SwitchTree {
                        internal: SwitchTreeInternal::Body {
                            body: value,
                        }
                    })
                }
                Pattern::Tuple(t) => {
                    let tuple = t.iter().enumerate().collect::<Vec<_>>();
                    let tuple_accessors = ('a'..='z').collect::<Vec<_>>();

                    let (tuple, tuple_accessors) = Self::sort_tuple(tuple, tuple_accessors);

                    for ((_, tuple), accessor) in tuple.into_iter().zip(tuple_accessors) {
                        let (matcher, branches) = Self::convert_arms(arms, *value.clone());

                    }

                }
            }

        }

        (output_matcher.unwrap_or(matcher), output)
    }

    fn sort_tuple(mut tuple: Vec<(usize, &Pattern)>, mut accessors: Vec<char>) -> (Vec<(usize, &Pattern)>, Vec<char>){
        let mut pairs = tuple.into_iter().zip(accessors.into_iter()).collect::<Vec<_>>();
        
        pairs.sort_by(|((_, a), _), ((_, b), _)| {
            usize::cmp(a.get_priority(), b.get_priority())
        });

        pairs.into_iter().unzip()
    }
}

enum SwitchTreeInternal<'a> {
    Root {
        root: SwitchTreeRoot,
        branches: Vec<SwitchBranch<'a>>,
    },
    Body {
        body: &'a Either<Expression, Vec<Statement>>,
    }
}

enum SwitchTreeRoot {
    RootTagged(Expression),
    RootUntagged(Expression),
}

enum SwitchBranch<'a> {
    NatualSuccessor(usize, SwitchTree<'a>),
    NaturalZero(SwitchTree<'a>),
    Literal(String, SwitchTree<'a>),
    Default(SwitchTree<'a>),
    Variable(String, SwitchTree<'a>),
}
