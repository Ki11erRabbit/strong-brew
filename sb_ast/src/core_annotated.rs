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

#[derive(Debug, Clone, PartialOrd, Hash)]
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

impl PartialEq for PathName {
    fn eq(&self, other: &Self) -> bool {
        self.segments == other.segments
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TopLevelStatement {
    Function(Function),
    Enum(Enum),
    Const(Const),
    Import(Import),
    Extern(String, String),
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
        return_type: Type,
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
        return_type: Type,
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
        return_type: Type,
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
        return_type: Type,
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
    pub ty: Type,
    pub start: usize,
    pub end: usize,
}

impl Param {
    pub fn new(implicit: bool, name: &str, ty: Type, start: usize, end: usize) -> Param {
        Param { implicit, name: name.to_string(), ty, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Field {
    pub visibility: Visibility,
    pub name: String,
    pub ty: Type,
    pub start: usize,
    pub end: usize,
}

impl Field {
    pub fn new(visibility: Visibility, name: &str, ty: Type, start: usize, end: usize) -> Field {
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
    pub ty: Type,
    pub value: Expression,
    pub start: usize,
    pub end: usize,
}

impl Const {
    pub fn new(
        visibility: Visibility,
        name: PathName,
        ty: Type,
        value: Expression,
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
    pub constraint: Option<Type>,
    pub start: usize,
    pub end: usize,
}

impl GenericParam {
    pub fn new(name: &str, constraint: Option<Type>, start: usize, end: usize) -> GenericParam {
        GenericParam { name: name.to_string(), constraint, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Statement {
    Expression(Expression),
    Let {
        name: Pattern,
        ty: Type,
        value: Expression,
        start: usize,
        end: usize,
    },
    Assignment {
        target: Expression,
        value: Expression,
        start: usize,
        end: usize,
    },
}

impl Statement {
    pub fn new_let(name: Pattern, ty: Type, value: Expression, start: usize, end: usize) -> Statement {
        Statement::Let { name, ty, value, start, end }
    }

    pub fn new_assignment(target: Expression, value: Expression, start: usize, end: usize) -> Statement {
        Statement::Assignment { target, value, start, end }
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
        params: Vec<Type>,
        return_type: Box<Type>,
    },
}

#[derive(Debug, Clone, PartialOrd)]
pub enum Type {
    /// User Types always begin with a capital letter
    User(PathName),
    Builtin(BuiltinType),
    /// A type that is parameterized by another type
    /// e.g. `Maybe[Int]`
    Parameterized(Box<Type>, Vec<Type>),
    PossibleType(Vec<Type>),
    Tuple(Vec<Type>),
    Expression(Box<Expression>),
}

impl Type {
    pub fn is_mut(&self) -> bool {
        match self {
            Type::Parameterized(a, b) => {
                let Type::User(a) = a.as_ref() else {
                    unreachable!("Type::Parameterized should always contain a Type::User")
                };
                let PathName { segments, .. } = a;
                segments.len() == 1 && segments[0] == "Mut"
            }
            _ => false,
        }
    }
}


impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::User(a), Type::User(b)) => a == b,
            (Type::Builtin(a), Type::Builtin(b)) => a == b,
            (Type::Parameterized(a, b), Type::Parameterized(c, d)) => a == c && b == d,
            (Type::PossibleType(a), Type::PossibleType(b)) => a == b,
            (Type::Tuple(a), Type::Tuple(b)) => a == b,
            (Type::Expression(a), Type::Expression(b)) => a == b,
            (Type::PossibleType(a), x) => {
                a.iter().any(|y| y == x)
            }
            (x, Type::PossibleType(a)) => {
                a.iter().any(|y| y == x)
            }
            (Type::Parameterized(a, b), x) => {
                let Type::User(a) = a.as_ref() else {
                    unreachable!("Type::Parameterized should always contain a Type::User")
                };
                let PathName { segments, .. } = a;
                if segments.len() == 1 && segments[0] == "Mut" {
                    b[0] == *x
                } else {
                    false
                }
            }
            (x, Type::Parameterized(a, b)) => {
                let Type::User(a) = a.as_ref() else {
                    unreachable!("Type::Parameterized should always contain a Type::User")
                };
                let PathName { segments, .. } = a;
                if segments.len() == 1 && segments[0] == "Mut" {
                    b[0] == *x
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Expression {
    pub raw: ExpressionRaw,
    pub ty: Rc<RefCell<Type>>,
}

impl Expression {
    pub fn new(raw: ExpressionRaw, ty: &Type) -> Expression {
        Expression { raw, ty: Rc::new(RefCell::new(ty.clone())) }
    }

    pub fn is_if_or_match(&self) -> bool {
        match &self.raw {
            ExpressionRaw::IfExpression(_) | ExpressionRaw::MatchExpression(_) => true,
            _ => false,
        }
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ExpressionRaw {
    Type(Type),
    /// A variable is always begins with a lowercase letter
    Variable(PathName),
    /// A constant is always completely uppercase
    Constant(PathName),
    Literal(Literal),
    Call(Call),
    Return(Option<Either<Box<ExpressionRaw>, Box<Expression>>>),
    Closure(Closure),
    Parenthesized(Either<Box<ExpressionRaw>, Box<Expression>>),
    Tuple(Either<Vec<ExpressionRaw>, Vec<Expression>>),
    IfExpression(IfExpr),
    MatchExpression(MatchExpr),
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<ExpressionRaw>,
        start: usize,
        end: usize,
    },
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<ExpressionRaw>,
        right: Box<ExpressionRaw>,
        start: usize,
        end: usize,
    },
}

impl ExpressionRaw {
    pub fn new_binary(
        operator: BinaryOperator,
        left: Box<ExpressionRaw>,
        right: Box<ExpressionRaw>,
        start: usize,
        end: usize,
    ) -> ExpressionRaw {
        ExpressionRaw::BinaryOperation { operator, left, right, start, end }
    }

    pub fn new_unary<'a>(
        operator: UnaryOperator,
        operand: Box<ExpressionRaw>,
        start: usize,
        end: usize,
    ) -> ExpressionRaw {
        ExpressionRaw::UnaryOperation { operator, operand, start, end }
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
    List(Vec<ExpressionRaw>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Call {
    pub name: Box<ExpressionRaw>,
    pub type_args: Vec<Type>,
    pub args: Vec<CallArg>,
    pub start: usize,
    pub end: usize,
}

impl Call {
    pub fn new(
        name: Box<ExpressionRaw>,
        type_args: Vec<Type>,
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
    pub value: Either<ExpressionRaw, Expression>,
    pub start: usize,
    pub end: usize,
}

impl CallArg {
    pub fn new<'a>(name: Option<String>, value: Either<ExpressionRaw, Expression>, start: usize, end: usize) -> CallArg {
        CallArg { name, value, start, end }
    }

    pub fn get_type(&self) -> Type {
        match &self.value {
            Either::Left(_) => {
                panic!("Cannot get type of an expression raw")
            }
            Either::Right(expr) => {
                expr.ty.borrow().clone()
            }
        }
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Closure {
    pub params: Vec<Param>,
    pub return_type: Option<Box<Type>>,
    pub body: Vec<Statement>,
    pub start: usize,
    pub end: usize,
}

impl Closure {
    pub fn new(
        params: Vec<Param>,
        return_type: Option<Box<Type>>,
        body: Vec<Statement>,
        start: usize,
        end: usize,
    ) -> ExpressionRaw {
        ExpressionRaw::Closure(Closure { params, return_type, body, start, end })
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
    pub value: Either<Expression, Vec<Statement>>,
    pub start: usize,
    pub end: usize,
}

impl MatchArm {
    pub fn new(
        pattern: Pattern,
        value: Either<Expression, Vec<Statement>>,
        start: usize,
        end: usize
    ) -> MatchArm {
        MatchArm { pattern, value, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Pattern {
    Wildcard,
    Variable(String),
    Literal(Literal),
    Tuple(Vec<Pattern>),
    Constructor {
        name: PathName,
        fields: Vec<Pattern>,
    },
}

impl Pattern {
    pub fn get_bound_names(&self) -> Vec<String> {
        let mut names = Vec::new();
        self.get_bound_names_helper(&mut names);
        names
    }

    fn get_bound_names_helper(&self, names: &mut Vec<String>) {
        match self {
            Pattern::Wildcard => {}
            Pattern::Variable(name) => {
                names.push(name.clone());
            }
            Pattern::Literal(_) => {}
            Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    pattern.get_bound_names_helper(names);
                }
            }
            Pattern::Constructor { fields, .. } => {
                for field in fields {
                    field.get_bound_names_helper(names);
                }
            }
        }
    }
}
