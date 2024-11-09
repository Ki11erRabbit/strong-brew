use either::Either;
use crate::inner;

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

    pub fn from(path: inner::PathName) -> PathName {
        let inner::PathName { segments, start, end } = path;
        PathName::new(segments, start, end)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum TopLevelStatement<'a> {
    Function(Function<'a>),
    Enum(Enum<'a>),
    Const(Const<'a>),
    Import(Import<'a>),
    Extern(&'a str, &'a str),
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
    pub name: Pattern<'a>,
    pub constraint: Option<ExpressionType<'a>>,
    pub start: usize,
    pub end: usize,
}

impl GenericParam<'_> {
    pub fn new<'a>(name: Pattern<'a>, constraint: Option<ExpressionType<'a>>, start: usize, end: usize) -> GenericParam<'a> {
        GenericParam { name, constraint, start, end }
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

    pub fn new_assignment<'a>(target: ExpressionType<'a>, value: ExpressionType<'a>, start: usize, end: usize) -> Statement<'a> {
        Statement::Assignment { target, value, start, end }
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
    Type(Type<'a>),
    /// A variable is always begins with a lowercase letter
    Variable(PathName<'a>),
    /// A constant is always completely uppercase
    Constant(PathName<'a>),
    Literal(Literal<'a>),
    Call(Call<'a>),
    MemberAccess {
        object: Box<Expression<'a>>,
        field: &'a str,
        start: usize,
        end: usize,
    },
    Return(Option<Box<Expression<'a>>>),
    Closure(Closure<'a>),
    Parenthesized(Box<Expression<'a>>),
    Tuple(Vec<ExpressionType<'a>>),
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




pub fn convert_inner_to_core(file: inner::File) -> File {
    let inner::File { path, content } = file;
    let path = PathName::from(path);
    let mut contents = Vec::new();
    for item in content {
        let item = match item {
            inner::TopLevelStatement::Import(import) => {
                let inner::Import { path, start, end } = import;
                let path = PathName::from(path);
                Import::new(path, start, end)
            }
            inner::TopLevelStatement::Const(constant) => {
                let inner::Const { visibility, name, ty, value, start, end } = constant;
                let visibility = convert_visibility(visibility);
                let name = PathName::from(name);
                let ty = convert_expression_type(ty);
                let value = convert_expression_type(value);
                Const::new(visibility, name, ty, value, start, end)
            }
            inner::TopLevelStatement::Enum(enm) => {
                let inner::Enum { visibility, name, generic_params, variants, start, end } = enm;
                let visibility = convert_visibility(visibility);
                let name = name;
                let generic_params = convert_generic_params(generic_params);
                let variants = convert_variants(variants);
                Enum::new(visibility, name, generic_params, variants, start, end)
            }
            inner::TopLevelStatement::Struct(struct_) => {
                convert_struct_to_enum(struct_)
            }
            inner::TopLevelStatement::Function(function) => {
                convert_function(function)
            }
            inner::TopLevelStatement::Extern(lang, body) => {
                TopLevelStatement::Extern(lang, body)
            }
        };

        contents.push(item);
    }

    File::new(path, contents)
}

fn convert_visibility(visibility: inner::Visibility) -> Visibility {
    match visibility {
        inner::Visibility::Public => Visibility::Public,
        inner::Visibility::Private => Visibility::Private,
    }
}

fn convert_generic_params<'a>(params: Vec<inner::GenericParam<'a>>) -> Vec<GenericParam<'a>> {
    params.into_iter().map(|param| {
        let inner::GenericParam { name, constraint, start, end } = param;
        let name = convert_pattern(name);
        let constraint = constraint.map(convert_expression_type);
        GenericParam::new(name, constraint, start, end)
    }).collect()
}

fn convert_variants<'a>(variants: Vec<inner::Variant<'a>>) -> Vec<Variant<'a>> {
    variants.into_iter().map(|variant| {
        let inner::Variant { name, fields, start, end } = variant;
        let fields = fields.into_iter().map(|field| {
            let inner::Field { visibility, name, ty, start, end } = field;
            let visibility = convert_visibility(visibility);
            let ty = convert_expression_type(ty);
            Field::new(visibility, name, ty, start, end)
        }).collect();
        Variant::new(name, fields, start, end)
    }).collect()
}

fn convert_struct_to_enum<'a>(struct_: inner::Struct<'a>) -> TopLevelStatement<'a> {
    let inner::Struct { visibility, name, generic_params, fields, start, end } = struct_;
    let visibility = convert_visibility(visibility);
    let name = name;
    let generic_params = convert_generic_params(generic_params);
    let fields = fields.into_iter().map(|field| {
        let inner::Field { visibility, name, ty, start, end } = field;
        let visibility = convert_visibility(visibility);
        let ty = convert_expression_type(ty);
        Field::new(visibility, name, ty, start, end)
    }).collect();
    let variants = vec![
        Variant::new(name, fields, start, end)
            ];
    Enum::new(visibility, name, generic_params, variants, start, end)
}

fn convert_function<'a>(function: inner::Function<'a>) -> TopLevelStatement<'a> {
    match function {
        inner::Function::Regular { visibility, name, generic_params, params, return_type, body, start, end } => {
            let visibility = convert_visibility(visibility);
            let name = PathName::from(name);
            let generic_params = convert_generic_params(generic_params);
            let params = params.into_iter().map(|param| {
                let inner::Param { implicit, name, ty, start, end } = param;
                let ty = convert_expression_type(ty);
                Param::new(implicit, name, ty, start, end)
            }).collect();
            let return_type = convert_expression_type(*return_type);
            let body = body.into_iter().map(|stmt| {
                convert_statement(stmt)
            }).collect();
            Function::new(visibility, name, generic_params, params, return_type, body, start, end)
        }
        inner::Function::Extern { visibility, language, name, generic_params, params, return_type, body, start, end } => {
            let visibility = convert_visibility(visibility);
            let name = PathName::from(name);
            let generic_params = convert_generic_params(generic_params);
            let params = params.into_iter().map(|param| {
                let inner::Param { implicit, name, ty, start, end } = param;
                let ty = convert_expression_type(ty);
                Param::new(implicit, name, ty, start, end)
            }).collect();
            let return_type = convert_expression_type(*return_type);
            Function::new_extern(visibility, language, name, generic_params, params, return_type, body, start, end)
        }
    }
}

fn convert_statement<'a>(stmt: inner::Statement<'a>) -> Statement<'a> {
    match stmt {
        inner::Statement::Expression(expr) => {
            Statement::Expression(convert_expression_type_member_to_call(expr))
        }
        inner::Statement::Let { name, ty, value, start, end } => {
            let name = convert_pattern(name);
            let ty = convert_expression_type(ty);
            let value = convert_expression_type(value);
            Statement::new_let(name, ty, value, start, end)
        }
        inner::Statement::Const { name, ty, value, start, end } => {
            let ty = convert_expression_type(ty);
            let value = convert_expression_type(value);
            let name = Pattern::Variable(name);
            Statement::new_let(name, ty, value, start, end)
        }
        inner::Statement::Assignment { target, value, start, end } => {
            let target = convert_expression_type(target);
            let value = convert_expression_type_member_to_call(value);
            Statement::new_assignment(target, value, start, end)
        }
    }
}

fn convert_expression_type<'a>(expr: inner::ExpressionType<'a>) -> ExpressionType<'a> {
    let inner::ExpressionType { mutable, expression, variadic } = expr;
    if mutable {
        let ty = Type::Parameterized(
            Box::new(Type::User(PathName::new(vec!["Mut"], 0, 0))),
            vec![convert_expression(expression)]
        );
        ExpressionType::new(Expression::Type(ty), variadic)
    } else {
        let expression = convert_expression(expression);
        ExpressionType::new(expression, variadic)
    }
}

fn convert_expression_type_member_to_call<'a>(expr: inner::ExpressionType<'a>) -> ExpressionType<'a> {
    let inner::ExpressionType { mutable, expression, variadic } = expr;
    if mutable {
        let ty = Type::Parameterized(
            Box::new(Type::User(PathName::new(vec!["Mut"], 0, 0))),
            vec![convert_expression(expression)]
        );
        ExpressionType::new(Expression::Type(ty), variadic)
    } else {
        let expression = convert_expression(expression);
        let expression = convert_member_access_to_call(expression);
        ExpressionType::new(expression, variadic)
    }
}

fn convert_expression<'a>(expr: inner::Expression<'a>) -> Expression<'a> {
    match expr {
        inner::Expression::Type(ty) => {
            Expression::Type(convert_type(ty))
        }
        inner::Expression::Variable(name) => {
            Expression::Variable(PathName::from(name))
        }
        inner::Expression::Constant(name) => {
            Expression::Constant(PathName::from(name))
        }
        inner::Expression::Literal(literal) => {
            Expression::Literal(convert_literal(literal))
        }
        inner::Expression::Call(call) => {
            Expression::Call(convert_call_expr(call))
        }
        inner::Expression::MemberAccess { object, field, start, end } => {
            let object = Box::new(convert_expression(*object));
            Expression::new_member_access(object, field, start, end)
        }
        inner::Expression::Return(value) => {
            let value = value.map(|value| Box::new(convert_expression(*value)));
            Expression::Return(value)
        }
        inner::Expression::Closure(lambda) => {
            Expression::Closure(convert_lambda(lambda))
        }
        inner::Expression::Parenthesized(expr) => {
            let expr = Box::new(convert_expression(*expr));
            Expression::Parenthesized(expr)
        }
        inner::Expression::Tuple(exprs) => {
            let exprs = exprs.into_iter().map(convert_expression_type).collect();
            Expression::Tuple(exprs)
        }
        inner::Expression::IfExpression(if_expr) => {
            Expression::IfExpression(convert_if_expr(if_expr))
        }
        inner::Expression::MatchExpression(match_expr) => {
            Expression::MatchExpression(convert_match_expr(match_expr))
        }
        inner::Expression::UnaryOperation { operator, operand, start, end } => {
            let operator = match operator {
                inner::UnaryOperator::Neg => UnaryOperator::Neg,
                inner::UnaryOperator::Not => UnaryOperator::Not,
                inner::UnaryOperator::Try => UnaryOperator::Try,
            };
            let operand = Box::new(convert_expression(*operand));
            Expression::new_unary(operator, operand, start, end)
        }
        inner::Expression::BinaryOperation { operator, left, right, start, end } => {
            let operator = match operator {
                inner::BinaryOperator::Add => BinaryOperator::Add,
                inner::BinaryOperator::Sub => BinaryOperator::Sub,
                inner::BinaryOperator::Mul => BinaryOperator::Mul,
                inner::BinaryOperator::Div => BinaryOperator::Div,
                inner::BinaryOperator::Mod => BinaryOperator::Mod,
                inner::BinaryOperator::And => BinaryOperator::And,
                inner::BinaryOperator::Or => BinaryOperator::Or,
                inner::BinaryOperator::Eq => BinaryOperator::Eq,
                inner::BinaryOperator::Ne => BinaryOperator::Ne,
                inner::BinaryOperator::Lt => BinaryOperator::Lt,
                inner::BinaryOperator::Le => BinaryOperator::Le,
                inner::BinaryOperator::Gt => BinaryOperator::Gt,
                inner::BinaryOperator::Ge => BinaryOperator::Ge,
                inner::BinaryOperator::Concat => BinaryOperator::Concat,
                inner::BinaryOperator::Index => BinaryOperator::Index,
            };
            let left = Box::new(convert_expression(*left));
            let right = Box::new(convert_expression(*right));
            Expression::new_binary(operator, left, right, start, end)
        }

    }
}

fn convert_call_expr<'a>(call: inner::Call<'a>) -> Call<'a> {
    let inner::Call { name, type_args, args, lambdas, start, end } = call;
    let name = convert_expression(*name);

    let mut first_arg = Vec::new();
    let name = match name {
        Expression::MemberAccess { object, field, start, end } => {
            let object = convert_member_access_to_call(*object);
            first_arg.push(object);
            Expression::Variable(PathName::new(vec![field], start, end))
        }
        x => x,
    };


    let type_args = type_args.into_iter().map(convert_expression_type).collect();
    let mut args: Vec<CallArg> = args.into_iter().map(|arg| {
        let inner::CallArg { name, value, start, end } = arg;
        let value = convert_expression(value);
        CallArg::new(name, value, start, end)
    }).collect();

    for lambda in lambdas {
        args.push(CallArg::new(None, Expression::Closure(convert_lambda(lambda)), 0, 0));
    }

    Call::new(Box::new(name), type_args, args, start, end)
}

fn convert_member_access_to_call<'a>(expr: Expression<'a>) -> Expression<'a> {
    match expr {
        Expression::MemberAccess { object, field, start, end } => {
            let object = convert_member_access_to_call(*object);
            let name = Expression::Variable(PathName::new(vec![field], start, end));
            Expression::Call(Call::new(Box::new(name), vec![], vec![CallArg::new(None, object, 0, 0)], 0, 0))
        }
        x => x,
    }
}

fn convert_pattern<'a>(pattern: inner::Pattern<'a>) -> Pattern<'a> {
    match pattern {
        inner::Pattern::Variable(name) => Pattern::Variable(name),
        inner::Pattern::Literal(literal) => Pattern::Literal(convert_literal(literal)),
        inner::Pattern::Tuple(patterns) => {
            let patterns = patterns.into_iter().map(convert_pattern).collect();
            Pattern::Tuple(patterns)
        }
        inner::Pattern::Constructor { name, fields } => {
            let name = PathName::from(name);
            let fields = fields.into_iter().map(convert_pattern).collect();
            Pattern::Constructor { name, fields }
        }
    }
}

fn convert_literal<'a>(literal: inner::Literal<'a>) -> Literal<'a> {
    match literal {
        inner::Literal::Int(int) => Literal::Int(int),
        inner::Literal::Float(float) => Literal::Float(float),
        inner::Literal::Char(char) => Literal::Char(char),
        inner::Literal::String(string) => Literal::String(string),
        inner::Literal::Bool(bool) => Literal::Bool(bool),
        inner::Literal::Unit => Literal::Unit,
        inner::Literal::List(list) => {
            let list = list.into_iter().map(convert_expression).collect();
            Literal::List(list)
        }
    }
}

fn convert_lambda<'a>(lambda: inner::Closure<'a>) -> Closure<'a> {
    let inner::Closure { params, return_type, body, start, end } = lambda;
    let params = params.into_iter().map(|param| {
        let inner::Param { implicit, name, ty, start, end } = param;
        let ty = convert_expression_type(ty);
        Param::new(implicit, name, ty, start, end)
    }).collect();
    let return_type = return_type.map(|ty| Box::new(convert_expression_type(*ty)));
    let body = body.into_iter().map(convert_statement).collect();
    Closure {
        params,
        return_type,
        body,
        start,
        end,
    }
}

fn convert_if_expr<'a>(if_expr: inner::IfExpr<'a>) -> IfExpr<'a> {
    let inner::IfExpr { condition, then_branch, else_branch, start, end } = if_expr;
    let condition = Box::new(convert_expression(*condition));
    let then_branch = then_branch.into_iter().map(convert_statement).collect();
    let else_branch = else_branch.map(|branch| {
        match branch {
            Either::Left(if_expr) => {
                Either::Left(Box::new(convert_if_expr(*if_expr)))
            }
            Either::Right(statements) => {
                let statements = statements.into_iter().map(convert_statement).collect();
                Either::Right(statements)
            }
        }
    });
    IfExpr::new(condition, then_branch, else_branch, start, end)
}

fn convert_match_expr<'a>(match_expr: inner::MatchExpr<'a>) -> MatchExpr<'a> {
    let inner::MatchExpr { value, arms, start, end } = match_expr;
    let value = Box::new(convert_expression(*value));
    let arms = arms.into_iter().map(|arm| {
        let inner::MatchArm { pattern, value, start, end } = arm;
        let pattern = convert_pattern(pattern);
        let value = match value {
            Either::Left(expr) => Either::Left(convert_expression_type(expr)),
            Either::Right(statements) => Either::Right(statements.into_iter().map(convert_statement).collect())
        };
        MatchArm::new(pattern, value, start, end)
    }).collect();
    MatchExpr::new(value, arms, start, end)
}

fn convert_type<'a>(ty: inner::Type<'a>) -> Type<'a> {
    match ty {
        inner::Type::User(path) => Type::User(PathName::from(path)),
        inner::Type::Builtin(builtin) => {
            match builtin {
                inner::BuiltinType::I8 => Type::Builtin(BuiltinType::I8),
                inner::BuiltinType::I16 => Type::Builtin(BuiltinType::I16),
                inner::BuiltinType::I32 => Type::Builtin(BuiltinType::I32),
                inner::BuiltinType::I64 => Type::Builtin(BuiltinType::I64),
                inner::BuiltinType::Int => Type::Builtin(BuiltinType::Int),
                inner::BuiltinType::Nat => Type::Builtin(BuiltinType::Nat),
                inner::BuiltinType::F32 => Type::Builtin(BuiltinType::F32),
                inner::BuiltinType::F64 => Type::Builtin(BuiltinType::F64),
                inner::BuiltinType::Bool => Type::Builtin(BuiltinType::Bool),
                inner::BuiltinType::Char => Type::Builtin(BuiltinType::Char),
                inner::BuiltinType::Unit => Type::Builtin(BuiltinType::Unit),
                inner::BuiltinType::Never => Type::Builtin(BuiltinType::Never),
                inner::BuiltinType::Type => Type::Builtin(BuiltinType::Type),
                inner::BuiltinType::Function { params, return_type } => {
                    let params = params.into_iter().map(convert_expression_type).collect();
                    let return_type = Box::new(convert_expression_type(*return_type));
                    Type::Builtin(BuiltinType::Function { params, return_type })
                }
            }
        }
        inner::Type::Parameterized(ty, args) => {
            let ty = Box::new(convert_type(*ty));
            let args = args.into_iter().map(convert_expression).collect();
            Type::Parameterized(ty, args)
        }
    }
}
