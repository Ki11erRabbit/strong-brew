use either::Either;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct File {
    pub path: PathName,
    pub content: Vec<TopLevelStatement>,
}

impl File {
    pub fn new(path: PathName, content: Vec<TopLevelStatement>) -> File {
        File {
            path,
            content,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct PathName {
    pub segments: Vec<String>,
    pub start: usize,
    pub end: usize,
}

impl PathName {
    pub fn new(segments: Vec<&str>, start: usize, end: usize) -> PathName {
        let segments = segments.iter().map(|s| s.to_string()).collect();
        PathName {
            segments,
            start,
            end,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TopLevelStatement {
    Function(Function),
    Enum(Enum),
    Const(Const),
    Import(Import),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Function {
    Regular {
        visibility: Visibility,
        name: PathName,
        generic_params: Vec<GenericParam>,
        params: Vec<Param>,
        return_type: ExpressionType,
        body: Vec<Statement>,
        start: usize,
        end: usize,
    },
    Extern {
        visibility: Visibility,
        language: String,
        name: PathName,
        generic_params: Vec<GenericParam>,
        params: Vec<Param>,
        return_type: ExpressionType,
        body: String,
        start: usize,
        end: usize,
    },
}

impl Function {
    pub fn new(
        visibility: Visibility,
        name: PathName,
        generic_params: Vec<GenericParam>,
        params: Vec<Param>,
        return_type: ExpressionType,
        body: Vec<Statement>,
        start: usize,
        end: usize,
    ) -> TopLevelStatement {
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

    pub fn new_extern(
        visibility: Visibility,
        language: &str,
        name: PathName,
        generic_params: Vec<GenericParam>,
        params: Vec<Param>,
        return_type: ExpressionType,
        body: &str,
        start: usize,
        end: usize,
    ) -> TopLevelStatement {
        TopLevelStatement::Function(Function::Extern {
            visibility,
            language: language.to_string(),
            name,
            generic_params,
            params,
            return_type,
            body: body.to_string(),
            start,
            end,
        })
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Param {
    pub implicit: bool,
    pub name: String,
    pub ty: ExpressionType,
    pub start: usize,
    pub end: usize,
}

impl Param {
    pub fn new(implicit: bool, name: &str, ty: ExpressionType, start: usize, end: usize) -> Param {
        Param { implicit, name: name.to_string(), ty, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Field {
    pub visibility: Visibility,
    pub name: String,
    pub ty: ExpressionType,
    pub start: usize,
    pub end: usize,
}

impl Field {
    pub fn new(visibility: Visibility, name: &str, ty: ExpressionType, start: usize, end: usize) -> Field {
        Field { visibility, name: name.to_string(), ty, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Enum {
    pub visibility: Visibility,
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub variants: Vec<Variant>,
    pub start: usize,
    pub end: usize,
}

impl Enum {
    pub fn new(
        visibility: Visibility,
        name: &str,
        generic_params: Vec<GenericParam>,
        variants: Vec<Variant>,
        start: usize,
        end: usize,
    ) -> TopLevelStatement {
        TopLevelStatement::Enum(Enum {
            visibility,
            name: name.to_string(),
            generic_params,
            variants,
            start,
            end,
        })
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<Field>,
    pub start: usize,
    pub end: usize,
}

impl Variant {
    pub fn new(name: &str, fields: Vec<Field>, start: usize, end: usize) -> Variant {
        Variant { name: name.to_string(), fields, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Const {
    pub visibility: Visibility,
    pub name: PathName,
    pub ty: ExpressionType,
    pub value: ExpressionType,
    pub start: usize,
    pub end: usize,
}

impl Const {
    pub fn new(
        visibility: Visibility,
        name: PathName,
        ty: ExpressionType,
        value: ExpressionType,
        start: usize,
        end: usize,
    ) -> TopLevelStatement {
        TopLevelStatement::Const(Const { visibility, name, ty, value, start, end })
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct Import {
    pub path: PathName,
    pub start: usize,
    pub end: usize,
}

impl Import {
    pub fn new(path: PathName, start: usize, end: usize) -> TopLevelStatement {
        TopLevelStatement::Import(Import { path, start, end })
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct GenericParam {
    pub name: String,
    pub constraints: Vec<ExpressionType>,
    pub start: usize,
    pub end: usize,
}

impl GenericParam {
    pub fn new(name: &str, constraints: Vec<ExpressionType>, start: usize, end: usize) -> GenericParam {
        GenericParam { name: name.to_string(), constraints, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Statement {
    Expression(ExpressionType),
    Let {
        name: Pattern,
        ty: ExpressionType,
        value: ExpressionType,
        start: usize,
        end: usize,
    },
    Assignment {
        target: ExpressionType,
        value: ExpressionType,
        start: usize,
        end: usize,
    },
}

impl Statement {
    pub fn new_let(name: Pattern, ty: ExpressionType, value: ExpressionType, start: usize, end: usize) -> Statement {
        Statement::Let { name, ty, value, start, end }
    }

    pub fn new_assignment(target: ExpressionType, value: ExpressionType, start: usize, end: usize) -> Statement {
        Statement::Assignment { target, value, start, end }
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ExpressionType {
    pub expression: Expression,
    pub variadic: bool,
}

impl ExpressionType {
    pub fn new(expression: Expression, variadic: bool) -> ExpressionType {
        ExpressionType { expression, variadic }
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
    Type,
    Function {
        params: Vec<ExpressionType>,
        return_type: Box<ExpressionType>,
    },
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Type {
    /// User Types always begin with a capital letter
    User(PathName),
    Builtin(BuiltinType),
    /// A type that is parameterized by another type
    /// e.g. `Maybe[Int]`
    Parameterized(Box<Type>, Vec<Expression>),
    PossibleType(Vec<ExpressionType>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expression {
    Type(Type),
    /// A variable is always begins with a lowercase letter
    Variable(PathName, Rc<RefCell<ExpressionType>>),
    /// A constant is always completely uppercase
    Constant(PathName, Rc<RefCell<ExpressionType>>),
    Literal(Literal, Rc<RefCell<ExpressionType>>),
    Call(Call, Rc<RefCell<ExpressionType>>),
    MemberAccess {
        object: Box<Expression>,
        field: String,
        start: usize,
        end: usize,
        ty: Rc<RefCell<ExpressionType>>,
    },
    Return(Option<Box<Expression>>, Rc<RefCell<ExpressionType>>),
    Closure(Closure, Rc<RefCell<ExpressionType>>),
    Parenthesized(Box<Expression>, Rc<RefCell<ExpressionType>>),
    Tuple(Vec<ExpressionType>, Rc<RefCell<ExpressionType>>),
    IfExpression(IfExpr, Rc<RefCell<ExpressionType>>),
    MatchExpression(MatchExpr, Rc<RefCell<ExpressionType>>),
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression>,
        start: usize,
        end: usize,
        ty: Rc<RefCell<ExpressionType>>,
    },
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
        start: usize,
        end: usize,
        ty: Rc<RefCell<ExpressionType>>,
    },
}

impl Expression {
    pub fn new_binary(
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
        start: usize,
        end: usize,
        ty: Rc<RefCell<ExpressionType>>,
    ) -> Expression {
        Expression::BinaryOperation { operator, left, right, start, end, ty }
    }

    pub fn new_unary<'a>(
        operator: UnaryOperator,
        operand: Box<Expression>,
        start: usize,
        end: usize,
        ty: Rc<RefCell<ExpressionType>>,
    ) -> Expression {
        Expression::UnaryOperation { operator, operand, start, end, ty }
    }

    pub fn new_member_access(
        object: Box<Expression>,
        field: String,
        start: usize,
        end: usize,
        ty: Rc<RefCell<ExpressionType>>,
    ) -> Expression {
        Expression::MemberAccess { object, field, start, end, ty }
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

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Literal {
    Int(String),
    Float(String),
    Char(String),
    String(String),
    Bool(bool),
    Unit,
    List(Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Call {
    pub name: Box<Expression>,
    pub type_args: Vec<ExpressionType>,
    pub args: Vec<CallArg>,
    pub start: usize,
    pub end: usize,
}

impl Call {
    pub fn new(
        name: Box<Expression>,
        type_args: Vec<ExpressionType>,
        args: Vec<CallArg>,
        start: usize,
        end: usize,
    ) -> Call {
        Call { name, type_args, args, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct CallArg {
    pub name: Option<String>,
    pub value: Expression,
    pub start: usize,
    pub end: usize,
}

impl CallArg {
    pub fn new<'a>(name: Option<String>, value: Expression, start: usize, end: usize) -> CallArg {
        CallArg { name, value, start, end }
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Closure {
    pub params: Vec<Param>,
    pub return_type: Option<Box<ExpressionType>>,
    pub body: Vec<Statement>,
    pub start: usize,
    pub end: usize,
}

impl Closure {
    pub fn new(
        params: Vec<Param>,
        return_type: Option<Box<ExpressionType>>,
        body: Vec<Statement>,
        start: usize,
        end: usize,
        ty: Rc<RefCell<ExpressionType>>,
    ) -> Expression {
        Expression::Closure(Closure { params, return_type, body, start, end }, ty)
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct IfExpr {
    pub condition: Box<Expression>,
    pub then_branch: Vec<Statement>,
    pub else_branch: Option<Either<Box<IfExpr>, Vec<Statement>>>,
    pub start: usize,
    pub end: usize,
}

impl IfExpr {
    pub fn new(
        condition: Box<Expression>,
        then_branch: Vec<Statement>,
        else_branch: Option<Either<Box<IfExpr>, Vec<Statement>>>,
        start: usize,
        end: usize,
    ) -> IfExpr {
        IfExpr { condition, then_branch, else_branch, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct MatchExpr {
    pub value: Box<Expression>,
    pub arms: Vec<MatchArm>,
    pub start: usize,
    pub end: usize,
}

impl MatchExpr {
    pub fn new<'a>(
        value: Box<Expression>,
        arms: Vec<MatchArm>,
        start: usize,
        end: usize,
    ) -> MatchExpr {
        MatchExpr { value, arms, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub value: Either<ExpressionType, Vec<Statement>>,
    pub start: usize,
    pub end: usize,
}

impl MatchArm {
    pub fn new(
        pattern: Pattern,
        value: Either<ExpressionType, Vec<Statement>>,
        start: usize,
        end: usize
    ) -> MatchArm {
        MatchArm { pattern, value, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Pattern {
    Variable(String, Rc<RefCell<ExpressionType>>),
    Literal(Literal, Rc<RefCell<ExpressionType>>),
    Tuple(Vec<Pattern>, Rc<RefCell<ExpressionType>>),
    Constructor {
        name: PathName,
        fields: Vec<Pattern>,
        ty: Rc<RefCell<ExpressionType>>,
    },
}
