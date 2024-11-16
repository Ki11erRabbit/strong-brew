use either::Either;
use crate::inner;

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct PathName {
    pub segments: Vec<String>,
    pub start: usize,
    pub end: usize,
}

impl PathName {
    pub fn new<S: AsRef<str>>(segments: Vec<S>, start: usize, end: usize) -> PathName {
        let segments = segments.into_iter().map(|s| s.as_ref().to_string()).collect();
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
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

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
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

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
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

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
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

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
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

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
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

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct GenericParam {
    pub name: String,
    pub constraint: Option<ExpressionType>,
    pub start: usize,
    pub end: usize,
}

impl GenericParam {
    pub fn new(name: &str, constraint: Option<ExpressionType>, start: usize, end: usize) -> GenericParam {
        GenericParam { name: name.to_string(), constraint, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
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


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct ExpressionType {
    pub expression: Expression,
    pub variadic: bool,
}

impl ExpressionType {
    pub fn new(expression: Expression, variadic: bool) -> ExpressionType {
        ExpressionType { expression, variadic }
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
    Type,
    Function {
        params: Vec<ExpressionType>,
        return_type: Box<ExpressionType>,
    },
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Type {
    /// User Types always begin with a capital letter
    User(PathName),
    Builtin(BuiltinType),
    /// A type that is parameterized by another type
    /// e.g. `Maybe[Int]`
    Parameterized(Box<Type>, Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Expression {
    Type(Type),
    /// A variable is always begins with a lowercase letter
    Variable(PathName),
    /// A constant is always completely uppercase
    Constant(PathName),
    Literal(Literal),
    Call(Call),
    MemberAccess {
        object: Box<Expression>,
        field: String,
        start: usize,
        end: usize,
    },
    Return(Option<Box<Expression>>),
    Closure(Closure),
    Parenthesized(Box<Expression>),
    Tuple(Vec<ExpressionType>),
    IfExpression(IfExpr),
    MatchExpression(MatchExpr),
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression>,
        start: usize,
        end: usize,
    },
    BinaryOperation {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
        start: usize,
        end: usize,
    },
}

impl Expression {
    pub fn new_binary(
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
        start: usize,
        end: usize,
    ) -> Expression {
        Expression::BinaryOperation { operator, left, right, start, end }
    }

    pub fn new_unary(
        operator: UnaryOperator,
        operand: Box<Expression>,
        start: usize,
        end: usize,
    ) -> Expression {
        Expression::UnaryOperation { operator, operand, start, end }
    }

    pub fn new_member_access(
        object: Box<Expression>,
        field: &str,
        start: usize,
        end: usize,
    ) -> Expression {
        Expression::MemberAccess { object, field: field.to_string(), start, end }
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
pub enum Literal {
    Int(String),
    Float(String),
    Char(String),
    String(String),
    Bool(bool),
    Unit,
    List(Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
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

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct CallArg {
    pub name: Option<String>,
    pub value: Expression,
    pub start: usize,
    pub end: usize,
}

impl CallArg {
    pub fn new(name: Option<&str>, value: Expression, start: usize, end: usize) -> CallArg {
        let name = name.map(|x| x.to_string());
        CallArg { name, value, start, end }
    }
}


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
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
    ) -> Expression {
        Expression::Closure(Closure { params, return_type, body, start, end })
    }
}


#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
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

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub struct MatchExpr {
    pub value: Box<Expression>,
    pub arms: Vec<MatchArm>,
    pub start: usize,
    pub end: usize,
}

impl MatchExpr {
    pub fn new(
        value: Box<Expression>,
        arms: Vec<MatchArm>,
        start: usize,
        end: usize,
    ) -> MatchExpr {
        MatchExpr { value, arms, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
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

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd)]
pub enum Pattern {
    Variable(String),
    Literal(Literal),
    Tuple(Vec<Pattern>),
    Constructor {
        name: PathName,
        fields: Vec<Pattern>,
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
                TopLevelStatement::Extern(lang.to_string(), body.to_string())
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

fn convert_generic_params(params: Vec<inner::GenericParam>) -> Vec<GenericParam> {
    params.into_iter().map(|param| {
        let inner::GenericParam { name, constraint, start, end } = param;
        let name = name;
        let constraint = constraint.map(convert_expression_type);
        GenericParam::new(name, constraint, start, end)
    }).collect()
}

fn convert_variants(variants: Vec<inner::Variant>) -> Vec<Variant> {
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

fn convert_struct_to_enum(struct_: inner::Struct) -> TopLevelStatement {
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
    Enum::new(visibility, &format!("struct-{}",name), generic_params, variants, start, end)
}

fn convert_function(function: inner::Function) -> TopLevelStatement {
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

fn convert_statement(stmt: inner::Statement) -> Statement {
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
            let name = Pattern::Variable(name.to_string());
            Statement::new_let(name, ty, value, start, end)
        }
        inner::Statement::Assignment { target, value, start, end } => {
            let target = convert_expression_type_member_to_call(target);
            let value = convert_expression_type_member_to_call(value);
            Statement::new_assignment(target, value, start, end)
        }
    }
}

fn convert_expression_type(expr: inner::ExpressionType) -> ExpressionType {
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

fn convert_expression_type_member_to_call(expr: inner::ExpressionType) -> ExpressionType {
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

fn convert_expression(expr: inner::Expression) -> Expression {
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

fn convert_call_expr(call: inner::Call) -> Call {
    let inner::Call { name, type_args, args, lambdas, start, end } = call;
    let name = convert_expression(*name);

    let mut first_arg = Vec::new();
    let name = match name {
        Expression::MemberAccess { object, field, start, end } => {
            let object = convert_member_access_to_call(*object);
            first_arg.push(CallArg::new(None, object, 0, 0));
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
    first_arg.append(&mut args);
    let mut args = first_arg;

    for lambda in lambdas {
        args.push(CallArg::new(None, Expression::Closure(convert_lambda(lambda)), 0, 0));
    }

    Call::new(Box::new(name), type_args, args, start, end)
}

fn convert_member_access_to_call(expr: Expression) -> Expression {
    match expr {
        Expression::MemberAccess { object, field, start, end } => {
            let object = convert_member_access_to_call(*object);
            let name = Expression::Variable(PathName::new(vec![field], start, end));
            let expr = Expression::Call(Call::new(Box::new(name), vec![], vec![CallArg::new(None, object, 0, 0)], 0, 0));
            expr
        }
        x => x,
    }
}

fn convert_pattern(pattern: inner::Pattern) -> Pattern {
    match pattern {
        inner::Pattern::Variable(name) => Pattern::Variable(name.to_string()),
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

fn convert_literal(literal: inner::Literal) -> Literal {
    match literal {
        inner::Literal::Int(int) => Literal::Int(int.to_string()),
        inner::Literal::Float(float) => Literal::Float(float.to_string()),
        inner::Literal::Char(c) => Literal::Char(c.to_string()),
        inner::Literal::String(string) => Literal::String(string.to_string()),
        inner::Literal::Bool(bool) => Literal::Bool(bool),
        inner::Literal::Unit => Literal::Unit,
        inner::Literal::List(list) => {
            let list = list.into_iter().map(convert_expression).collect();
            Literal::List(list)
        }
    }
}

fn convert_lambda(lambda: inner::Closure) -> Closure {
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

fn convert_if_expr(if_expr: inner::IfExpr) -> IfExpr {
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

fn convert_match_expr(match_expr: inner::MatchExpr) -> MatchExpr {
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

fn convert_type(ty: inner::Type) -> Type {
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
