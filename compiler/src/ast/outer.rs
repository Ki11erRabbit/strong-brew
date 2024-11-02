
#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct File<'a> {
    pub path: PathName<'a>,
    pub content: Vec<TopLevelStatement<'a>>,
}

impl File<'_> {
    pub fn new<'a>(path: PathName<'a>, content: Vec<TopLevelStatement<'a>>) -> File<'a> {
        File { path, content }
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
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Function<'a> {
    Regular {
        visibility: Visibility,
        name: PathName<'a>,
        generic_params: Vec<GenericParam<'a>>,
        params: Vec<Param<'a>>,
        return_type: Type<'a>,
        body: Expression<'a>,
        start: usize,
        end: usize,
    },
    Extern {
        visibility: Visibility,
        language: &'a str,
        name: PathName<'a>,
        generic_params: Vec<GenericParam<'a>>,
        params: Vec<Param<'a>>,
        return_type: Type<'a>,
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
        return_type: Type<'a>,
        body: Expression<'a>,
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
        return_type: Type<'a>,
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
    pub ty: Type<'a>,
    pub start: usize,
    pub end: usize,
}

impl Field<'_> {
    pub fn new<'a>(visibility: Visibility, name: &'a str, ty: Type<'a>, start: usize, end: usize) -> Field<'a> {
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
    pub ty: Type<'a>,
    pub value: Expression<'a>,
    pub start: usize,
    pub end: usize,
}

impl Const<'_> {
    pub fn new<'a>(visibility: Visibility, name: PathName<'a>, ty: Type<'a>, value: Expression<'a>, start: usize, end: usize) -> TopLevelStatement<'a> {
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
    pub constraints: Vec<Type<'a>>,
    pub start: usize,
    pub end: usize,
}

impl GenericParam<'_> {
    pub fn new<'a>(name: &'a str, constraints: Vec<Type<'a>>, start: usize, end: usize) -> GenericParam<'a> {
        GenericParam { name, constraints, start, end }
    }
}


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Param<'a> {
    pub implicit: bool,
    pub name: &'a str,
    pub ty: Type<'a>,
    pub start: usize,
    pub end: usize,
}

impl Param<'_> {
    pub fn new<'a>(implicit: bool, name: &'a str, ty: Type<'a>, start: usize, end: usize) -> Param<'a> {
        Param { implicit, name, ty, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Type<'a> {
    pub mutable: bool,
    pub raw: RawType<'a>,
}

impl Type<'_> {
    pub fn new<'a>(mutable: bool, raw: RawType<'a>) -> Type<'a> {
        Type { mutable, raw }
    }
}


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum RawType<'a> {
    Builtin(BuiltinType),
    Simple {
        name: PathName<'a>,
        start: usize,
        end: usize,
    },
    Generic {
        name: PathName<'a>,
        params: Vec<Type<'a>>,
        start: usize,
        end: usize,
    },
    Expression {
        expr: Box<Expression<'a>>,
        start: usize,
        end: usize,
    },
}

impl RawType<'_> {
    pub fn new_builtin<'a>(builtin: BuiltinType) -> RawType<'a> {
        RawType::Builtin(builtin)
    }

    pub fn new_simple<'a>(name: PathName<'a>, start: usize, end: usize) -> RawType<'a> {
        RawType::Simple { name, start, end }
    }

    pub fn new_generic<'a>(name: PathName<'a>, params: Vec<Type<'a>>, start: usize, end: usize) -> RawType<'a> {
        RawType::Generic { name, params, start, end }
    }

    pub fn new_expression<'a>(expr: Box<Expression<'a>>, start: usize, end: usize) -> RawType<'a> {
        RawType::Expression { expr, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum BuiltinType {
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
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Expression<'a> {
    Blank,
    Sequence(Vec<Expression<'a>>),
    Variable(&'a str),
    Type(Type<'a>),
    Literal(Literal<'a>),
    Call(Call<'a>),
    Return(Box<Expression<'a>>),
    Closure(Closure<'a>),
    Parenthesized(Box<Expression<'a>>),
    Tuple(Vec<Expression<'a>>),
    LetExpression {
        name: &'a str,
        ty: Type<'a>,
        value: Box<Expression<'a>>,
        start: usize,
        end: usize,
    },
    ConstExpression {
        name: &'a str,
        ty: Type<'a>,
        value: Box<Expression<'a>>,
        start: usize,
        end: usize,
    },
    Assignment {
        target: Box<Expression<'a>>,
        value: Box<Expression<'a>>,
        start: usize,
        end: usize,
    },
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

    pub fn new_let<'a>(
        name: &'a str,
        ty: Type<'a>,
        value: Box<Expression<'a>>,
        start: usize,
        end: usize,
    ) -> Expression<'a> {
        Expression::LetExpression { name, ty, value, start, end }
    }

    pub fn new_const<'a>(
        name: &'a str,
        ty: Type<'a>,
        value: Box<Expression<'a>>,
        start: usize,
        end: usize,
    ) -> Expression<'a> {
        Expression::ConstExpression { name, ty, value, start, end }
    }

    pub fn new_assignment<'a>(
        target: Box<Expression<'a>>,
        value: Box<Expression<'a>>,
        start: usize,
        end: usize,
    ) -> Expression<'a> {
        Expression::Assignment { target, value, start, end }
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
    Index,
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Literal<'a> {
    Value(&'a str),
    Tuple(Vec<Expression<'a>>),
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Call<'a> {
    Function {
        name: PathName<'a>,
        type_args: Vec<Type<'a>>,
        args: Vec<CallArg<'a>>,
        start: usize,
        end: usize,
    },
    Method {
        object: Box<Expression<'a>>,
        method: &'a str,
        type_args: Vec<Type<'a>>,
        args: Vec<CallArg<'a>>,
        start: usize,
        end: usize,
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct CallArg<'a> {
    pub name: Option<&'a str>,
    pub value: Expression<'a>,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Closure<'a> {
    pub params: Vec<Param<'a>>,
    pub return_type: Option<Type<'a>>,
    pub body: Box<Expression<'a>>,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct IfExpression<'a> {
    pub condition: Box<Expression<'a>>,
    pub then_branch: Box<Expression<'a>>,
    pub else_branch: Option<Box<Expression<'a>>>,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct MatchExpression<'a> {
    pub value: Box<Expression<'a>>,
    pub arms: Vec<MatchArm<'a>>,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct MatchArm<'a> {
    pub pattern: Pattern<'a>,
    pub value: Expression<'a>,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Pattern<'a> {
    Wildcard,
    Variable(&'a str),
    Literal(Literal<'a>),
    Tuple(Vec<Pattern<'a>>),
    Struct {
        name: PathName<'a>,
        fields: Vec<Pattern<'a>>,
    },
    Enum {
        name: PathName<'a>,
        variant: &'a str,
        fields: Vec<Pattern<'a>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Visibility {
    Public,
    Private,
}

