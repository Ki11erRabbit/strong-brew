use either::Either;

pub mod outer;
pub mod inner;


#[derive(Debug)]
pub enum ConversionError {
    InvalidPath,
    InvalidContents,
    InvalidVariableName(usize, usize),
    NoListType,
    FoundTypeInExpressionState,
    TrailingLambdasOnNonCall(usize, usize),
    MultipleIndex(usize, usize),
    NotAClosure,
}

pub enum ExpressionState {
    LeftHandSide,
    RightHandSide,
    InType,
}


pub fn convert_outer_to_inner(outer: outer::File) -> Result<inner::File, ConversionError> {
    let outer::File { path, content } = outer;
    let outer::PathName { segments, start, end } = path;

    let mut inner_content = Vec::new();

    for item in content {
        match item {
            outer::TopLevelStatement::Import(import) => {
                inner_content.push(convert_import(import)?);
            }
            outer::TopLevelStatement::Const(constant) => {
                inner_content.push(convert_const(constant)?);
            }
            outer::TopLevelStatement::Function(function) => {
                inner_content.push(convert_function(function)?);
            }
            outer::TopLevelStatement::Struct(struct_) => {
                inner_content.push(convert_struct(struct_)?);
            }
            outer::TopLevelStatement::Enum(enum_) => {
                inner_content.push(convert_enum(enum_)?);
            }
        }
    }
    return Ok(inner::File {
        path: inner::PathName {
            segments,
            start,
            end,
        },
        content: inner_content,
    });
}

fn convert_import(import: outer::Import) -> Result<inner::TopLevelStatement, ConversionError> {
    let outer::Import { path, start: istart, end: iend } = import;
    let outer::PathName { segments, start, end } = path;
    Ok(inner::Import::new(inner::PathName { segments, start, end },istart, iend))
}

fn convert_const(constant: outer::Const) -> Result<inner::TopLevelStatement, ConversionError> {
    let outer::Const { visibility, name, ty, value, start: cstart, end: cend } = constant;
    let inner_visibility = convert_visibility(visibility);
    let outer::PathName { segments, start, end } = name;
    let inner_name = inner::PathName { segments, start, end };
    let inner_ty = convert_expression_type(ty)?;
    let inner_value = convert_expression_type(value)?;
    Ok(inner::Const::new(inner_visibility, inner_name, inner_ty, inner_value, cstart, cend))
}

fn convert_visibility(visibility: outer::Visibility) -> inner::Visibility {
    match visibility {
        outer::Visibility::Public => inner::Visibility::Public,
        outer::Visibility::Private => inner::Visibility::Private,
    }
}

fn convert_struct(struct_: outer::Struct) -> Result<inner::TopLevelStatement, ConversionError> {
    let outer::Struct { visibility, name, generic_params, fields, start: sstart, end: send } = struct_;
    let inner_visibility = convert_visibility(visibility);
    let inner_generic_params = convert_generic_params(generic_params);
    let inner_fields = convert_fields(fields)?;

    Ok(inner::Struct::new(inner_visibility, name, inner_generic_params, inner_fields, sstart, send))
}

fn convert_fields(fields: Vec<outer::Field>) -> Result<Vec<inner::Field>, ConversionError> {
    let mut inner_fields = Vec::new();
    for field in fields {
        let outer::Field { visibility, name, ty, start: fstart, end: fend } = field;
        let inner_visibility = convert_visibility(visibility);
        let inner_name = name;
        let inner_ty = convert_expression_type(ty)?;
        inner_fields.push(inner::Field::new(inner_visibility, inner_name, inner_ty, fstart, fend));
    }
    Ok(inner_fields)
}

fn convert_enum(enum_: outer::Enum) -> Result<inner::TopLevelStatement, ConversionError> {
    let outer::Enum { visibility, name, generic_params, variants, start: estart, end: eend } = enum_;
    let inner_visibility = convert_visibility(visibility);
    let inner_generic_params = convert_generic_params(generic_params);
    let inner_variants = convert_variants(variants)?;

    Ok(inner::Enum::new(inner_visibility, name, inner_generic_params, inner_variants, estart, eend))
}

fn convert_variants(variants: Vec<outer::Variant>) -> Result<Vec<inner::Variant>, ConversionError> {
    let mut inner_variants = Vec::new();
    for variant in variants {
        let outer::Variant { name, fields, start: vstart, end: vend } = variant;
        let inner_name = name;
        let inner_fields = convert_fields(fields)?;
        inner_variants.push(inner::Variant::new(inner_name, inner_fields, vstart, vend));
    }
    Ok(inner_variants)
}

fn convert_function(function: outer::Function) -> Result<inner::TopLevelStatement, ConversionError> {
    match function {
        outer::Function::Regular { visibility, name, generic_params, params, return_type, body, start, end } => {
            let inner_visibility = convert_visibility(visibility);
            let outer::PathName { segments, start, end } = name;
            let inner_name = inner::PathName { segments, start, end };
            let inner_generic_params = convert_generic_params(generic_params);
            let inner_params = convert_params(params)?;
            let inner_return_type = Box::new(convert_expression_type(*return_type)?);
            let inner_body = convert_block(body)?;
            Ok(inner::Function::new(inner_visibility, inner_name, inner_generic_params, inner_params, inner_return_type, inner_body, start, end))
        }
        outer::Function::Extern { visibility, language, name, generic_params, params, return_type, body, start, end } => {
            let inner_visibility = convert_visibility(visibility);
            let outer::PathName { segments, start, end } = name;
            let inner_name = inner::PathName { segments, start, end };
            let inner_generic_params = convert_generic_params(generic_params);
            let inner_params = convert_params(params)?;
            let inner_return_type = Box::new(convert_expression_type(*return_type)?);
            Ok(inner::Function::new_extern(inner_visibility, language, inner_name, inner_generic_params, inner_params, inner_return_type, body, start, end))
        }
    }
}

fn convert_params(params: Vec<outer::Param>) -> Result<Vec<inner::Param>, ConversionError> {
    let mut inner_params = Vec::new();
    for param in params {
        let outer::Param { implicit, name, ty, start, end } = param;
        let inner_name = name;
        let inner_ty = convert_expression_type(ty)?;
        inner_params.push(inner::Param::new(implicit, inner_name, inner_ty, start, end));
    }
    Ok(inner_params)
}

fn convert_generic_params(generic_params: Vec<outer::GenericParam>) -> Vec<inner::GenericParam> {
    let mut inner_generic_params = Vec::new();
    for generic_param in generic_params {
        let outer::GenericParam { name, constraints, start, end } = generic_param;
        let inner_name = name;
        let mut inner_constraints = Vec::new();
        for constraint in constraints {
            let inner_constraint = convert_expression_type(constraint).unwrap();
            inner_constraints.push(inner_constraint);
        }
        inner_generic_params.push(inner::GenericParam::new(inner_name, inner_constraints, start, end));
        
    }
    inner_generic_params
}

fn convert_block(block: Vec<outer::Statement>) -> Result<Vec<inner::Statement>, ConversionError> {
    let mut inner_block = Vec::new();
    for statement in block {
        match statement {
            outer::Statement::Let { name, ty, value, start, end } => {
                let inner_name = convert_pattern(name)?;
                let inner_ty = convert_expression_type(ty)?;
                let inner_value = convert_expression_type(value)?;
                inner_block.push(inner::Statement::new_let(inner_name, inner_ty, inner_value, start, end));
            }
            outer::Statement::Const { name, ty, value, start, end } => {
                let inner_name = name;
                let inner_ty = convert_expression_type(ty)?;
                let inner_value = convert_expression_type(value)?;
                inner_block.push(inner::Statement::new_const(inner_name, inner_ty, inner_value, start, end));
            }
            outer::Statement::Assignment { target, value, start, end } => {
                let inner_target = convert_expression_type(target)?;
                let inner_value = convert_expression_type(value)?;
                inner_block.push(inner::Statement::new_assignment(inner_target, inner_value, start, end));
            }
            outer::Statement::Expression(expression) => {
                let inner_expression = convert_expression_type(expression)?;
                inner_block.push(inner::Statement::Expression(inner_expression));
            }
        }
    }
    Ok(inner_block)
}

fn convert_pattern(pattern: outer::Pattern) -> Result<inner::Pattern, ConversionError> {
    match pattern {
        outer::Pattern::Variable(name) => Ok(inner::Pattern::Variable(name)),
        outer::Pattern::Literal(lit) => {
            let literal = convert_literal(lit)?;
            Ok(inner::Pattern::Literal(literal))
        },
        outer::Pattern::Tuple(patterns) => {
            let mut inner_patterns = Vec::new();
            for pattern in patterns {
                inner_patterns.push(convert_pattern(pattern)?);
            }
            Ok(inner::Pattern::Tuple(inner_patterns))
        }
        outer::Pattern::Constructor { name, fields } => {
            let outer::PathName { segments, start, end } = name;
            let inner_name = inner::PathName { segments, start, end };
            let mut inner_fields = Vec::new();
            for field in fields {
                inner_fields.push(convert_pattern(field)?);
            }
            Ok(inner::Pattern::Constructor { name: inner_name, fields: inner_fields })
        }
    }
}

fn convert_expression_type(expression: outer::ExpressionType) -> Result<inner::ExpressionType, ConversionError> {
    let outer::ExpressionType { mutable, expression, variadic } = expression;
    let inner_expression = convert_expression(expression)?;

    Ok(inner::ExpressionType { mutable, expression: inner_expression, variadic })
    
}

fn convert_expression(expression: outer::Expression) -> Result<inner::Expression, ConversionError> {
    match expression {
        outer::Expression::Type(builtin) => {
            let inner_builtin = convert_builtin(builtin)?;
            Ok(inner::Expression::Type(inner::Type::Builtin(inner_builtin)))
        }
        outer::Expression::Variable(name) => {
            let outer::PathName { segments, start, end } = name;
            let inner_name = inner::PathName { segments, start, end };

            let last_segment = inner_name.segments.last().unwrap();
            let mut all_capitals = true;
            for (i, c) in last_segment.chars().enumerate() {
                // Normal variables must start with a lowercase letter
                if i == 0 && !c.is_uppercase() {
                    return Ok(inner::Expression::Variable(inner_name));
                }
                // Type variables must be all uppercase
                if !c.is_uppercase() {
                    all_capitals = false;
                    break;
                }
            }
            if all_capitals {
                // Constants must be all uppercase
                Ok(inner::Expression::Constant(inner_name))
            } else {
                // Types start with an uppercase letter
                Ok(inner::Expression::Type(inner::Type::User(inner_name)))
            }
        }
        outer::Expression::Literal(lit) => {
            match lit {
                outer::Literal::Tuple(lits) => {
                    let mut inner_lits = Vec::new();
                    for lit in lits {
                        inner_lits.push(convert_expression_type(lit)?);
                    }
                    Ok(inner::Expression::Tuple(inner_lits))
                }
                external => Ok(inner::Expression::Literal(convert_literal(external)?)),
            }
        },
        outer::Expression::Call(call) => {
            let outer::Call { name, type_args, args, start, end } = call;
            let inner_name = convert_expression(*name)?;
            let inner_type_args = convert_type_args(type_args)?;
            let inner_args = convert_args(args)?;
            let call = inner::Call{ name: Box::new(inner_name), type_args: inner_type_args, args: inner_args,lambdas: Vec::new(), start, end };
            Ok(inner::Expression::Call(call))
        }
        outer::Expression::TrailingLambdas(source, lambdas, start, end) => {
            let inner_source = convert_expression(*source)?;
            let inner_lambdas = convert_lambdas(lambdas)?;

            match inner_source {
                inner::Expression::Call(call) => {
                    let mut call = call;
                    call.lambdas = inner_lambdas;
                    Ok(inner::Expression::Call(call))
                }
                inner::Expression::MemberAccess { .. } => {

                    let call = inner::Call {
                        name: Box::new(inner_source),
                        type_args: Vec::new(),
                        args: Vec::new(),
                        lambdas: inner_lambdas,
                        start,
                        end,
                    };
                    Ok(inner::Expression::Call(call))
                }
                inner::Expression::Variable(path) => {
                    let call = inner::Call {
                        name: Box::new(inner::Expression::Variable(path)),
                        type_args: Vec::new(),
                        args: Vec::new(),
                        lambdas: inner_lambdas,
                        start,
                        end,
                    };
                    Ok(inner::Expression::Call(call))
                }
                _ => Err(ConversionError::TrailingLambdasOnNonCall(start, end)),
            }
        }
        outer::Expression::MemberAccess { object, field, start, end } => {
            let inner_object = convert_expression(*object)?;
            let inner_field = field;
            Ok(inner::Expression::MemberAccess { object: Box::new(inner_object), field: inner_field, start, end })
        }
        outer::Expression::Return(value) => {
            let inner_value = value.map(|value| convert_expression(*value).map(Box::new)).transpose()?;
            
            Ok(inner::Expression::Return(inner_value))
        }
        outer::Expression::Closure(closure) => {
            let inner_closure = convert_lambda(closure)?;
            Ok(inner::Expression::Closure(inner_closure))
        }
        outer::Expression::Parenthesized(expression) => {
            let inner_expression = convert_expression(*expression)?;
            Ok(inner::Expression::Parenthesized(Box::new(inner_expression)))
        }
        outer::Expression::IfExpression(if_expression) => {
            let inner_if_expression = convert_if_expression(if_expression)?;
            Ok(inner::Expression::IfExpression(inner_if_expression))
        }
        outer::Expression::MatchExpression(match_expression) => {
            let inner_match_expression = convert_match_expression(match_expression)?;
            Ok(inner::Expression::MatchExpression(inner_match_expression))
        }
        outer::Expression::UnaryOperation { operator, operand, start, end } => {
            let inner_operator = convert_unary_operator(operator);
            let inner_operand = convert_expression(*operand)?;
            Ok(inner::Expression::UnaryOperation { operator: inner_operator, operand: Box::new(inner_operand), start, end })
        }
        outer::Expression::BinaryOperation { operator, left, right, start, end } => {
            let inner_operator = convert_binary_operator(operator);
            let inner_left = convert_expression(*left)?;
            let inner_right = convert_expression(*right)?;
            Ok(inner::Expression::BinaryOperation { operator: inner_operator, left: Box::new(inner_left), right: Box::new(inner_right), start, end })
        }
        outer::Expression::Bracketed { name, expressions, start, end } => {
            let inner_name = convert_expression(*name)?;
            let mut inner_expressions = Vec::new();
            for expression in expressions {
                inner_expressions.push(convert_expression(expression)?);
            }

            match inner_name {
                inner::Expression::Type(ty) => {
                    let ty = inner::Type::Parameterized(Box::new(ty), inner_expressions);

                    Ok(inner::Expression::Type(ty))
                }
                _ => {
                    if inner_expressions.len() > 1 {
                        Err(ConversionError::MultipleIndex(start, end))
                    } else {
                        Ok(inner::Expression::new_binary(
                            inner::BinaryOperator::Index,
                            Box::new(inner_name
                            ), Box::new(inner_expressions[0].clone()), start, end))
                    }
                }
            }

            
        }
    }
}

fn convert_builtin(builtin: outer::BuiltinType) -> Result<inner::BuiltinType, ConversionError> {
    let ty = match builtin {
        outer::BuiltinType::Unit => inner::BuiltinType::Unit,
        outer::BuiltinType::Never => inner::BuiltinType::Never,
        outer::BuiltinType::Type => inner::BuiltinType::Type,
        outer::BuiltinType::Char => inner::BuiltinType::Char,
        outer::BuiltinType::Nat => inner::BuiltinType::Nat,
        outer::BuiltinType::Bool => inner::BuiltinType::Bool,
        outer::BuiltinType::Int => inner::BuiltinType::Int,
        outer::BuiltinType::I8 => inner::BuiltinType::I8,
        outer::BuiltinType::I16 => inner::BuiltinType::I16,
        outer::BuiltinType::I32 => inner::BuiltinType::I32,
        outer::BuiltinType::I64 => inner::BuiltinType::I64,
        outer::BuiltinType::F32 => inner::BuiltinType::F32,
        outer::BuiltinType::F64 => inner::BuiltinType::F64,
        outer::BuiltinType::Function { params, return_type } => {
            let mut inner_params = Vec::new();
            for param in params {
                inner_params.push(convert_expression_type(param)?);
            }
            let inner_return_type = convert_expression_type(*return_type)?;
            inner::BuiltinType::Function { params: inner_params, return_type: Box::new(inner_return_type) }
        }
    };
    Ok(ty)
}

fn convert_literal(lit: outer::Literal) -> Result<inner::Literal, ConversionError> {
    let lit = match lit {
        outer::Literal::Int(n) => inner::Literal::Int(n),
        outer::Literal::Bool(b) => inner::Literal::Bool(b),
        outer::Literal::Char(c) => inner::Literal::Char(c),
        outer::Literal::String(s) => inner::Literal::String(s),
        outer::Literal::Float(f) => inner::Literal::Float(f),
        outer::Literal::Unit => inner::Literal::Unit,
        outer::Literal::List(l) => {
            let mut inner_l = Vec::new();
            for lit in l {
                inner_l.push(convert_expression(lit)?);
            }
            inner::Literal::List(inner_l)
        },
        _ => unreachable!("Tuples are evaluated in a different context"),
    };
    Ok(lit)
}

fn convert_type_args(type_args: Vec<outer::ExpressionType>) -> Result<Vec<inner::ExpressionType>, ConversionError> {
    let mut inner_type_args = Vec::new();
    for type_arg in type_args {
        inner_type_args.push(convert_expression_type(type_arg)?);
    }
    Ok(inner_type_args)
}

fn convert_args(args: Vec<outer::CallArg>) -> Result<Vec<inner::CallArg>, ConversionError> {
    let mut inner_args = Vec::new();
    for arg in args {
        let outer::CallArg { name, value, start, end } = arg;
        let inner_name = name;
        let inner_value = convert_expression(value)?;
        inner_args.push(inner::CallArg { name: inner_name, value: inner_value, start, end });
    }
    Ok(inner_args)
}

fn convert_lambdas(exprs: Vec<outer::Expression>) -> Result<Vec<inner::Closure>, ConversionError> {
    let mut inner_exprs = Vec::new();
    for expr in exprs {
        match expr {
            outer::Expression::Closure(closure) => {
                inner_exprs.push(convert_lambda(closure)?);
            }
            _ => return Err(ConversionError::NotAClosure),
        }
    }
    Ok(inner_exprs)
}

fn convert_lambda(lambda: outer::Closure) -> Result<inner::Closure, ConversionError> {
    let outer::Closure { params, return_type, body, start, end } = lambda;
    let inner_params = convert_params(params)?;
    let inner_return_type = return_type.map(|ty| convert_expression_type(*ty).map(Box::new)).transpose()?;
    let inner_body = convert_block(body)?;
    Ok(inner::Closure{ params: inner_params, return_type: inner_return_type, body: inner_body, start, end })
}

fn convert_if_expression(if_expression: outer::IfExpression) -> Result<inner::IfExpr, ConversionError> {
    let outer::IfExpression { condition, then_branch, else_branch, start, end } = if_expression;

    let inner_condition = Box::new(convert_expression(*condition)?);
    let inner_then_branch = convert_block(then_branch)?;
    let inner_else_branch = convert_else_branch(else_branch)?;

    Ok(inner::IfExpr { condition: inner_condition, then_branch: inner_then_branch, else_branch: inner_else_branch, start, end })
}

fn convert_else_branch<'a>(
    else_branch: Option<Either<Box<outer::IfExpression<'a>>, Vec<outer::Statement<'a>>>>
) -> Result<Option<Either<Box<inner::IfExpr<'a>>, Vec<inner::Statement<'a>>>>, ConversionError> {
    match else_branch {
        Some(else_branch) => {
            match else_branch {
                Either::Left(if_expression) => {
                    let inner_if_expression = convert_if_expression(*if_expression)?;
                    Ok(Some(Either::Left(Box::new(inner_if_expression)))
                    )
                }
                Either::Right(block) => {
                    let inner_block = convert_block(block)?;
                    Ok(Some(Either::Right(inner_block)))
                }
            }
        }
        None => Ok(None),
    }
}

fn convert_match_expression(
    match_expression: outer::MatchExpression
) -> Result<inner::MatchExpr, ConversionError> {
    let outer::MatchExpression { value, arms, start, end } = match_expression;
    let inner_value = Box::new(convert_expression(*value)?);
    let inner_arms = convert_match_arms(arms)?;
    Ok(inner::MatchExpr { value: inner_value, arms: inner_arms, start, end })
}

fn convert_match_arms(arms: Vec<outer::MatchArm>) -> Result<Vec<inner::MatchArm>, ConversionError> {
    let mut inner_arms = Vec::new();
    for arm in arms {
        let outer::MatchArm { pattern, value, start, end } = arm;
        let inner_pattern = convert_pattern(pattern)?;
        let inner_body = match value {
            Either::Left(expr) => {
                let inner_expr = convert_expression_type(expr)?;
                Either::Left(inner_expr)
            }
            Either::Right(block) => {
                let inner_block = convert_block(block)?;
                Either::Right(inner_block)
            }
        };
        inner_arms.push(inner::MatchArm { pattern: inner_pattern, value: inner_body, start, end });
    }
    Ok(inner_arms)
}

fn convert_unary_operator(operator: outer::UnaryOperator) -> inner::UnaryOperator {
    match operator {
        outer::UnaryOperator::Neg => inner::UnaryOperator::Neg,
        outer::UnaryOperator::Not => inner::UnaryOperator::Not,
        outer::UnaryOperator::Try => inner::UnaryOperator::Try,
    }
}

fn convert_binary_operator(operator: outer::BinaryOperator) -> inner::BinaryOperator {
    match operator {
        outer::BinaryOperator::Add => inner::BinaryOperator::Add,
        outer::BinaryOperator::Sub => inner::BinaryOperator::Sub,
        outer::BinaryOperator::Mul => inner::BinaryOperator::Mul,
        outer::BinaryOperator::Div => inner::BinaryOperator::Div,
        outer::BinaryOperator::Mod => inner::BinaryOperator::Mod,
        outer::BinaryOperator::And => inner::BinaryOperator::And,
        outer::BinaryOperator::Or => inner::BinaryOperator::Or,
        outer::BinaryOperator::Eq => inner::BinaryOperator::Eq,
        outer::BinaryOperator::Ne => inner::BinaryOperator::Ne,
        outer::BinaryOperator::Lt => inner::BinaryOperator::Lt,
        outer::BinaryOperator::Gt => inner::BinaryOperator::Gt,
        outer::BinaryOperator::Le => inner::BinaryOperator::Le,
        outer::BinaryOperator::Ge => inner::BinaryOperator::Ge,
        outer::BinaryOperator::Concat => inner::BinaryOperator::Concat,
    }
}
