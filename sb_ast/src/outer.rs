use either::Either;


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct File<'a> {
    pub path: PathName<'a>,
    pub content: Vec<TopLevelStatement<'a>>,
}

impl File<'_> {
    pub fn new<'a>(path: PathName<'a>, content: Vec<TopLevelStatement<'a>>) -> File<'a> {
        File { path, content }
    }

    pub fn get_imports(&self) -> Vec<&PathName> {
        self.content.iter().filter_map(|stmt| {
            if let TopLevelStatement::Import(import) = stmt {
                Some(&import.path)
            } else {
                None
            }
        }).collect()
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct PathName<'a> {
    pub segments: Vec<&'a str>,
    pub start: usize,
    pub end: usize,
}

impl PathName<'_> {
    pub fn new<'a>(segments: Vec<&'a str>, start: usize, end: usize) -> PathName<'a> {
        PathName { segments, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum TopLevelStatement<'a> {
    Function(Function<'a>),
    Struct(Struct<'a>),
    Enum(Enum<'a>),
    Const(Const<'a>),
    Import(Import<'a>),
    Extern(&'a str, &'a str),
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Function<'a> {
    Regular {
        visibility: Visibility,
        name: PathName<'a>,
        generic_params: Vec<GenericParam<'a>>,
        params: Vec<Param<'a>>,
        return_type: Box<ExpressionType<'a>>,
        body: Vec<Statement<'a>>,
        start: usize,
        end: usize,
    },
    Extern {
        visibility: Visibility,
        language: &'a str,
        name: PathName<'a>,
        generic_params: Vec<GenericParam<'a>>,
        params: Vec<Param<'a>>,
        return_type: Box<ExpressionType<'a>>,
        body: &'a str,
        start: usize,
        end: usize,
    },
}

impl Function<'_> {
    pub fn new<'a>(
        visibility: Visibility,
        name: PathName<'a>,
        generic_params: Vec<GenericParam<'a>>,
        params: Vec<Param<'a>>,
        return_type: Box<ExpressionType<'a>>,
        body: Vec<Statement<'a>>,
        start: usize,
        end: usize,
    ) -> TopLevelStatement<'a> {
        TopLevelStatement::Function(Function::Regular {
            visibility,
            name,
            generic_params,
            params,
            return_type,
            body,
            start,
            end,
        })
    }

    pub fn new_extern<'a>(
        visibility: Visibility,
        language: &'a str,
        name: PathName<'a>,
        generic_params: Vec<GenericParam<'a>>,
        params: Vec<Param<'a>>,
        return_type: Box<ExpressionType<'a>>,
        body: &'a str,
        start: usize,
        end: usize,
    ) -> TopLevelStatement<'a> {
        TopLevelStatement::Function(Function::Extern {
            visibility,
            language,
            name,
            generic_params,
            params,
            return_type,
            body,
            start,
            end,
        })
    }
} 

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Struct<'a> {
    pub visibility: Visibility,
    pub name: &'a str,
    pub generic_params: Vec<GenericParam<'a>>,
    pub fields: Vec<Field<'a>>,
    pub start: usize,
    pub end: usize,
}

impl Struct<'_> {
    pub fn new<'a>(
        visibility: Visibility,
        name: &'a str,
        generic_params: Vec<GenericParam<'a>>,
        fields: Vec<Field<'a>>,
        start: usize,
        end: usize,
    ) -> TopLevelStatement<'a> {
        TopLevelStatement::Struct(Struct {
            visibility,
            name,
            generic_params,
            fields,
            start,
            end,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Field<'a> {
    pub visibility: Visibility,
    pub name: &'a str,
    pub ty: ExpressionType<'a>,
    pub start: usize,
    pub end: usize,
}

impl Field<'_> {
    pub fn new<'a>(visibility: Visibility, name: &'a str, ty: ExpressionType<'a>, start: usize, end: usize) -> Field<'a> {
        Field { visibility, name, ty, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Enum<'a> {
    pub visibility: Visibility,
    pub name: &'a str,
    pub generic_params: Vec<GenericParam<'a>>,
    pub variants: Vec<Variant<'a>>,
    pub start: usize,
    pub end: usize,
}

impl Enum<'_> {
    pub fn new<'a>(
        visibility: Visibility,
        name: &'a str,
        generic_params: Vec<GenericParam<'a>>,
        variants: Vec<Variant<'a>>,
        start: usize,
        end: usize,
    ) -> TopLevelStatement<'a> {
        TopLevelStatement::Enum(Enum {
            visibility,
            name,
            generic_params,
            variants,
            start,
            end,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Variant<'a> {
    pub name: &'a str,
    pub fields: Vec<Field<'a>>,
    pub start: usize,
    pub end: usize,
}

impl Variant<'_> {
    pub fn new<'a>(name: &'a str, fields: Vec<Field<'a>>, start: usize, end: usize) -> Variant<'a> {
        Variant { name, fields, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Const<'a> {
    pub visibility: Visibility,
    pub name: PathName<'a>,
    pub ty: ExpressionType<'a>,
    pub value: ExpressionType<'a>,
    pub start: usize,
    pub end: usize,
}

impl Const<'_> {
    pub fn new<'a>(visibility: Visibility, name: PathName<'a>, ty: ExpressionType<'a>, value: ExpressionType<'a>, start: usize, end: usize) -> TopLevelStatement<'a> {
        TopLevelStatement::Const(Const { visibility, name, ty, value, start, end })
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Import<'a> {
    pub path: PathName<'a>,
    pub start: usize,
    pub end: usize,
}

impl Import<'_> {
    pub fn new<'a>(path: PathName<'a>, start: usize, end: usize) -> TopLevelStatement<'a> {
        TopLevelStatement::Import(Import { path, start, end })
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct GenericParam<'a> {
    pub name: &'a str,
    pub constraint: Option<ExpressionType<'a>>,
    pub start: usize,
    pub end: usize,
}

impl GenericParam<'_> {
    pub fn new<'a>(name: &'a str, constraint: Option<ExpressionType<'a>>, start: usize, end: usize) -> GenericParam<'a> {
        GenericParam { name, constraint, start, end }
    }
}


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Param<'a> {
    pub implicit: bool,
    pub name: &'a str,
    pub ty: ExpressionType<'a>,
    pub start: usize,
    pub end: usize,
}

impl Param<'_> {
    pub fn new<'a>(implicit: bool, name: &'a str, ty: ExpressionType<'a>, start: usize, end: usize) -> Param<'a> {
        Param { implicit, name, ty, start, end }
    }
}


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum BuiltinType<'a> {
    I8,
    I16,
    I32,
    I64,
    Int,
    Nat,
    F32,
    F64,
    Bool,
    Char,
    Unit,
    Never,
    Type,
    Function {
        params: Vec<ExpressionType<'a>>,
        return_type: Box<ExpressionType<'a>>,
    },
}


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Statement<'a> {
    Expression(ExpressionType<'a>),
    Let {
        name: Pattern<'a>,
        ty: ExpressionType<'a>,
        value: ExpressionType<'a>,
        start: usize,
        end: usize,
    },
    Const {
        name: &'a str,
        ty: ExpressionType<'a>,
        value: ExpressionType<'a>,
        start: usize,
        end: usize,
    },
    Assignment {
        target: ExpressionType<'a>,
        value: ExpressionType<'a>,
        start: usize,
        end: usize,
    },
}

impl Statement<'_> {
    pub fn new_let<'a>(name: Pattern<'a>, ty: ExpressionType<'a>, value: ExpressionType<'a>, start: usize, end: usize) -> Statement<'a> {
        Statement::Let { name, ty, value, start, end }
    }

    pub fn new_const<'a>(name: &'a str, ty: ExpressionType<'a>, value: ExpressionType<'a>, start: usize, end: usize) -> Statement<'a> {
        Statement::Const { name, ty, value, start, end }
    }

    pub fn new_assignment<'a>(target: ExpressionType<'a>, value: ExpressionType<'a>, start: usize, end: usize) -> Statement<'a> {
        Statement::Assignment { target, value, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct ExpressionType<'a> {
    pub mutable: bool,
    pub expression: Expression<'a>,
    pub variadic: bool,
}

impl ExpressionType<'_> {
    pub fn new<'a>(mutable: bool, expression: Expression<'a>, variadic: bool) -> ExpressionType<'a> {
        ExpressionType { mutable, expression, variadic }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Expression<'a> {
    Type(BuiltinType<'a>),
    Variable(PathName<'a>),
    Literal(Literal<'a>),
    Call(Call<'a>),
    TrailingLambdas(Box<Expression<'a>>, Vec<Expression<'a>>, usize, usize),
    MemberAccess {
        object: Box<Expression<'a>>,
        field: &'a str,
        start: usize,
        end: usize,
    },
    Return(Option<Box<Expression<'a>>>),
    Closure(Closure<'a>),
    Parenthesized(Box<Expression<'a>>),
    IfExpression(IfExpression<'a>),
    MatchExpression(MatchExpression<'a>),
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression<'a>>,
        start: usize,
        end: usize,
    },
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<Expression<'a>>,
        right: Box<Expression<'a>>,
        start: usize,
        end: usize,
    },
    Bracketed {
        name: Box<Expression<'a>>,
        expressions: Vec<Expression<'a>>,
        start: usize,
        end: usize,
    },
}

impl Expression<'_> {
    pub fn new_binary<'a>(
        operator: BinaryOperator,
        left: Box<Expression<'a>>,
        right: Box<Expression<'a>>,
        start: usize,
        end: usize,
    ) -> Expression<'a> {
        Expression::BinaryOperation { operator, left, right, start, end }
    }

    pub fn new_unary<'a>(
        operator: UnaryOperator,
        operand: Box<Expression<'a>>,
        start: usize,
        end: usize,
    ) -> Expression<'a> {
        Expression::UnaryOperation { operator, operand, start, end }
    }

    pub fn new_member_access<'a>(
        object: Box<Expression<'a>>,
        field: &'a str,
        start: usize,
        end: usize,
    ) -> Expression<'a> {
        Expression::MemberAccess { object, field, start, end }
    }

    pub fn new_bracketed<'a>(name: Box<Expression<'a>>, expressions: Vec<Expression<'a>>, start: usize, end: usize) -> Expression<'a> {
        Expression::Bracketed { name, expressions, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnaryOperator {
    Neg,
    Not,
    Try,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Concat,
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Literal<'a> {
    Int(&'a str),
    Float(&'a str),
    Char(&'a str),
    String(&'a str),
    Bool(bool),
    Unit,
    Tuple(Vec<ExpressionType<'a>>),
    List(Vec<Expression<'a>>),
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Call<'a> {
    pub name: Box<Expression<'a>>,
    pub type_args: Vec<ExpressionType<'a>>,
    pub args: Vec<CallArg<'a>>,
    pub start: usize,
    pub end: usize,
}

impl Call<'_> {
    pub fn new<'a>(
        name: Box<Expression<'a>>,
        type_args: Vec<ExpressionType<'a>>,
        args: Vec<CallArg<'a>>,
        start: usize,
        end: usize,
    ) -> Call<'a> {
        Call { name, type_args, args, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct CallArg<'a> {
    pub name: Option<&'a str>,
    pub value: Expression<'a>,
    pub start: usize,
    pub end: usize,
}

impl CallArg<'_> {
    pub fn new<'a>(name: Option<&'a str>, value: Expression<'a>, start: usize, end: usize) -> CallArg<'a> {
        CallArg { name, value, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Closure<'a> {
    pub params: Vec<Param<'a>>,
    pub return_type: Option<Box<ExpressionType<'a>>>,
    pub body: Vec<Statement<'a>>,
    pub start: usize,
    pub end: usize,
}

impl Closure<'_> {
    pub fn new<'a>(
        params: Vec<Param<'a>>,
        return_type: Option<Box<ExpressionType<'a>>>,
        body: Vec<Statement<'a>>,
        start: usize,
        end: usize,
    ) -> Expression<'a> {
        Expression::Closure(Closure { params, return_type, body, start, end })
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct IfExpression<'a> {
    pub condition: Box<Expression<'a>>,
    pub then_branch: Vec<Statement<'a>>,
    pub else_branch: Option<Either<Box<IfExpression<'a>>, Vec<Statement<'a>>>>,
    pub start: usize,
    pub end: usize,
}

impl IfExpression<'_> {
    pub fn new<'a>(
        condition: Box<Expression<'a>>,
        then_branch: Vec<Statement<'a>>,
        else_branch: Option<Either<Box<IfExpression<'a>>, Vec<Statement<'a>>>>,
        start: usize,
        end: usize,
    ) -> IfExpression<'a> {
        IfExpression { condition, then_branch, else_branch, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct MatchExpression<'a> {
    pub value: Box<Expression<'a>>,
    pub arms: Vec<MatchArm<'a>>,
    pub start: usize,
    pub end: usize,
}

impl MatchExpression<'_> {
    pub fn new<'a>(
        value: Box<Expression<'a>>,
        arms: Vec<MatchArm<'a>>,
        start: usize,
        end: usize,
    ) -> MatchExpression<'a> {
        MatchExpression { value, arms, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct MatchArm<'a> {
    pub pattern: Pattern<'a>,
    pub value: Either<ExpressionType<'a>, Vec<Statement<'a>>>,
    pub start: usize,
    pub end: usize,
}

impl MatchArm<'_> {
    pub fn new<'a>(pattern: Pattern<'a>, value: Either<ExpressionType<'a>, Vec<Statement<'a>>>, start: usize, end: usize) -> MatchArm<'a> {
        MatchArm { pattern, value, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Pattern<'a> {
    Variable(&'a str),
    Literal(Literal<'a>),
    Tuple(Vec<Pattern<'a>>),
    Constructor {
        name: PathName<'a>,
        fields: Vec<Pattern<'a>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Visibility {
    Public,
    Private,
}



