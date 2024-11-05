use either::Either;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct File<'a> {
    pub path: PathName<'a>,
    pub content: Vec<TopLevelStatement<'a>>,
}

impl File<'_> {
    pub fn new<'a>(path: PathName<'a>, content: Vec<TopLevelStatement<'a>>) -> File<'a> {
        File {
            path,
            content,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct PathName<'a> {
    pub segments: Vec<&'a str>,
    pub start: usize,
    pub end: usize,
}

impl PathName<'_> {
    pub fn new(segments: Vec<&str>, start: usize, end: usize) -> PathName {
        PathName {
            segments,
            start,
            end,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum TopLevelStatement<'a> {
    Function(Function<'a>),
    Struct(Struct<'a>),
    Enum(Enum<'a>),
    Const(Const<'a>),
    Import(Import<'a>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
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
    pub fn new<'a>(
        visibility: Visibility,
        name: PathName<'a>,
        ty: ExpressionType<'a>,
        value: ExpressionType<'a>,
        start: usize,
        end: usize,
    ) -> TopLevelStatement<'a> {
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
    pub constraints: Vec<ExpressionType<'a>>,
}

impl GenericParam<'_> {
    pub fn new<'a>(name: &'a str, constraints: Vec<ExpressionType<'a>>) -> GenericParam<'a> {
        GenericParam { name, constraints }
    }
}


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Param<'a> {
    pub implicit: bool,
    pub name: &'a str,
    pub ty: ExpressionType<'a>,
}

impl Param<'_> {
    pub fn new<'a>(implicit: bool, name: &'a str, ty: ExpressionType<'a>) -> Param<'a> {
        Param { implicit, name, ty }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Statement<'a> {
    Expression(ExpressionType<'a>),
    Let {
        name: Pattern<'a>,
        ty: ExpressionType<'a>,
        value: ExpressionType<'a>,
    },
    Const {
        name: &'a str,
        ty: ExpressionType<'a>,
        value: ExpressionType<'a>,
    },
    Assignment {
        target: ExpressionType<'a>,
        value: ExpressionType<'a>,
    },
}

impl Statement<'_> {
    pub fn new_let<'a>(name: Pattern<'a>, ty: ExpressionType<'a>, value: ExpressionType<'a>) -> Statement<'a> {
        Statement::Let { name, ty, value, }
    }

    pub fn new_const<'a>(name: &'a str, ty: ExpressionType<'a>, value: ExpressionType<'a>) -> Statement<'a> {
        Statement::Const { name, ty, value }
    }

    pub fn new_assignment<'a>(target: ExpressionType<'a>, value: ExpressionType<'a>) -> Statement<'a> {
        Statement::Assignment { target, value }
    }
}


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum ExpressionType<'a> {
    Type(Type<'a>),
    Expression(Expression<'a>),
}

impl ExpressionType<'_> {
    pub fn new_type<'a>(ty: Type<'a>) -> ExpressionType<'a> {
        ExpressionType::Type(ty)
    }

    pub fn new_expression<'a>(expr: Expression<'a>) -> ExpressionType<'a> {
        ExpressionType::Expression(expr)
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
pub struct Type<'a> {
    pub mutable: bool,
    pub ty: TypeKind<'a>,
    pub variadic: bool,
}

impl Type<'_> {
    pub fn new<'a>(mutable: bool, ty: TypeKind<'a>, variadic: bool) -> Type<'a> {
        Type { mutable, ty, variadic }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum TypeKind<'a> {
    BuiltIn(BuiltinType<'a>),
    Variable(PathName<'a>),
    Literal(Literal<'a>),
    User(PathName<'a>),
    Call(Call<'a>),
    MemberAccess {
        object: Box<Expression<'a>>,
        field: &'a str,
        start: usize,
        end: usize,
    },
    Parenthesized(Box<Type<'a>>),
    Tuple(Vec<Type<'a>>),
    IfType(IfExpr<'a>),
    MatchType(MatchExpr<'a>),
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<ExpressionType<'a>>,
        start: usize,
        end: usize,
    },
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<ExpressionType<'a>>,
        right: Box<ExpressionType<'a>>,
        start: usize,
        end: usize,
    },
    Generic {
        name: Box<Type<'a>>,
        params: Vec<Type<'a>>,
        start: usize,
        end: usize,
    },
}

impl TypeKind<'_> {
    pub fn new_binary<'a>(
        operator: BinaryOperator,
        left: Box<ExpressionType<'a>>,
        right: Box<ExpressionType<'a>>,
        start: usize,
        end: usize,
    ) -> TypeKind<'a> {
        TypeKind::BinaryOperation { operator, left, right, start, end }
    }

    pub fn new_unary<'a>(
        operator: UnaryOperator,
        operand: Box<ExpressionType<'a>>,
        start: usize,
        end: usize,
    ) -> TypeKind<'a> {
        TypeKind::UnaryOperation { operator, operand, start, end }
    }

    pub fn new_member_access<'a>(
        object: Box<Expression<'a>>,
        field: &'a str,
        start: usize,
        end: usize,
    ) -> TypeKind<'a> {
        TypeKind::MemberAccess { object, field, start, end }
    }
}



#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Expression<'a> {
    Variable(PathName<'a>),
    Literal(Literal<'a>),
    Call(Call<'a>),
    //TrailingLambdas(Box<Expression<'a>>, Vec<Expression<'a>>),
    MemberAccess {
        object: Box<Expression<'a>>,
        field: &'a str,
        start: usize,
        end: usize,
    },
    Return(Box<Expression<'a>>),
    Closure(Closure<'a>),
    Parenthesized(Box<Expression<'a>>),
    Tuple(Vec<Expression<'a>>),
    IfExpression(IfExpr<'a>),
    MatchExpression(MatchExpr<'a>),
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

    pub fn new_member_access<'a>(
        object: Box<Expression<'a>>,
        field: &'a str,
        start: usize,
        end: usize,
    ) -> Expression<'a> {
        Expression::MemberAccess { object, field, start, end }
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
    Index
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Literal<'a> {
    Int(&'a str),
    Float(&'a str),
    Char(&'a str),
    String(&'a str),
    Bool(bool),
    Unit,
    List(Vec<Expression<'a>>),
    Type(Box<Type<'a>>),
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Call<'a> {
    pub name: Box<Expression<'a>>,
    pub type_args: Vec<ExpressionType<'a>>,
    pub args: Vec<CallArg<'a>>,
    pub lambdas: Vec<Closure<'a>>,
    pub start: usize,
    pub end: usize,
}

impl Call<'_> {
    pub fn new<'a>(
        name: Box<Expression<'a>>,
        type_args: Vec<ExpressionType<'a>>,
        args: Vec<CallArg<'a>>,
        lambdas: Vec<Closure<'a>>,
        start: usize,
        end: usize,
    ) -> Call<'a> {
        Call { name, type_args, args, lambdas, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct CallArg<'a> {
    pub name: Option<&'a str>,
    pub value: ExpressionType<'a>,
    pub start: usize,
    pub end: usize,
}

impl CallArg<'_> {
    pub fn new<'a>(name: Option<&'a str>, value: ExpressionType<'a>, start: usize, end: usize) -> CallArg<'a> {
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
pub struct IfExpr<'a> {
    pub condition: Box<Expression<'a>>,
    pub then_branch: Vec<Statement<'a>>,
    pub else_branch: Option<Either<Box<IfExpr<'a>>, Vec<Statement<'a>>>>,
    pub start: usize,
    pub end: usize,
}

impl IfExpr<'_> {
    pub fn new<'a>(
        condition: Box<Expression<'a>>,
        then_branch: Vec<Statement<'a>>,
        else_branch: Option<Either<Box<IfExpr<'a>>, Vec<Statement<'a>>>>,
        start: usize,
        end: usize,
    ) -> IfExpr<'a> {
        IfExpr { condition, then_branch, else_branch, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct MatchExpr<'a> {
    pub value: Box<Expression<'a>>,
    pub arms: Vec<MatchArm<'a>>,
    pub start: usize,
    pub end: usize,
}

impl MatchExpr<'_> {
    pub fn new<'a>(
        value: Box<Expression<'a>>,
        arms: Vec<MatchArm<'a>>,
        start: usize,
        end: usize,
    ) -> MatchExpr<'a> {
        MatchExpr { value, arms, start, end }
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
    pub fn new<'a>(
        pattern: Pattern<'a>,
        value: Either<ExpressionType<'a>, Vec<Statement<'a>>>,
        start: usize,
        end: usize
    ) -> MatchArm<'a> {
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



