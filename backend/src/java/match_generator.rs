use either::Either;
use sb_ast::core_annotated::{Expression, MatchExpr, Statement};



pub struct SwitchTree {
    internal: SwitchTreeInternal,
}

impl SwitchTree {
    pub fn new_root(root: SwitchTreeRoot, branches: Vec<SwitchBranch>) -> SwitchTree {
        SwitchTree {
            internal: SwitchTreeInternal::Root {
                root,
                branches,
            }
        }
    }

    pub fn new_body(body: Either<Expression, Vec<Statement>>) -> SwitchTree {
        SwitchTree {
            internal: SwitchTreeInternal::Body {
                body,
            }
        }
    }

    pub fn from_match_expr(expr: &MatchExpr) -> SwitchTree {
        let MatchExpr { value, arms, .. } = expr;
        let Expression { raw, ty } = value.as_ref();
        let root = match ty {
        };
        todo!()
    }
}

enum SwitchTreeInternal {
    Root {
        root: SwitchTreeRoot,
        branches: Vec<SwitchBranch>,
    },
    Body {
        body: Either<Expression, Vec<Statement>>,
    }
}

enum SwitchTreeRoot {
    RootTagged(Expression),
    RootUntagged(Expression),
}

enum SwitchBranch {
    NatualSuccessor(usize, SwitchTree),
    NaturalZero(SwitchTree),
    Literal(String, SwitchTree),
    Default(SwitchTree),
}
