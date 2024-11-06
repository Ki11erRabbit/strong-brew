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
        return_type: ExpressionType<'a>,
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
        return_type: ExpressionType<'a>,
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
        return_type: ExpressionType<'a>,
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
        return_type: ExpressionType<'a>,
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
    pub start: usize,
    pub end: usize,
}

impl GenericParam<'_> {
    pub fn new<'a>(name: &'a str, constraints: Vec<ExpressionType<'a>>, start: usize, end: usize) -> GenericParam<'a> {
        GenericParam { name, constraints, start, end }
    }
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
}

impl Statement<'_> {
    pub fn new_let<'a>(name: Pattern<'a>, ty: ExpressionType<'a>, value: ExpressionType<'a>, start: usize, end: usize) -> Statement<'a> {
        Statement::Let { name, ty, value, start, end }
    }
}


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct ExpressionType<'a> {
    pub expression: Expression<'a>,
    pub variadic: bool,
}

impl ExpressionType<'_> {
    pub fn new<'a>(expression: Expression<'a>, variadic: bool) -> ExpressionType<'a> {
        ExpressionType { expression, variadic }
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
pub enum Type<'a> {
    /// User Types always begin with a capital letter
    User(PathName<'a>),
    Builtin(BuiltinType<'a>),
    /// A type that is parameterized by another type
    /// e.g. `Maybe[Int]`
    Parameterized(Box<Type<'a>>, Vec<Expression<'a>>),
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Expression<'a> {
    Type(Type<'a>, Box<ExpressionType<'a>>),
    /// A variable is always begins with a lowercase letter
    Variable(PathName<'a>, Box<ExpressionType<'a>>),
    /// A constant is always completely uppercase
    Constant(PathName<'a>, Box<ExpressionType<'a>>),
    Literal(Literal<'a>, Box<ExpressionType<'a>>),
    Call(Call<'a>, Box<ExpressionType<'a>>),
    Return(Option<Box<Expression<'a>>>, Box<ExpressionType<'a>>),
    Closure(Closure<'a>, Box<ExpressionType<'a>>),
    Parenthesized(Box<Expression<'a>>, Box<ExpressionType<'a>>),
    Tuple(Vec<ExpressionType<'a>>, Box<ExpressionType<'a>>),
    IfExpression(IfExpr<'a>, Box<ExpressionType<'a>>),
    MatchExpression(MatchExpr<'a>, Box<ExpressionType<'a>>),
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
        type_: ExpressionType<'a>,
    ) -> Expression<'a> {
        Expression::Closure(Closure { params, return_type, body, start, end }, Box::new(type_))
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
    Variable(&'a str, Box<ExpressionType<'a>>),
    Literal(Literal<'a>, Box<ExpressionType<'a>>),
    Tuple(Vec<Pattern<'a>>, Box<ExpressionType<'a>>),
    Constructor {
        name: PathName<'a>,
        fields: Vec<Pattern<'a>>,
        ty: Box<ExpressionType<'a>>,
    },
}


