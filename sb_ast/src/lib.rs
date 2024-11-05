
pub mod outer;
pub mod inner;


#[derive(Debug)]
pub enum ConversionError {
    InvalidPath,
    InvalidContents,
    InvalidVariableName(usize, usize),
    NoListType,
    FoundTypeInExpressionState,
}

pub enum ExpressionState {
    InType,
    InExpression,
}


pub fn convert_outer_to_inner(outer: outer::File) -> Result<inner::File, ConversionError> {
    let outer::File { path, content } = outer;
    let outer::PathName { segments, start, end } = path;

    let mut inner_content = Vec::new();

    for item in content {
        match item {
            outer::TopLevelStatement::Import(import) => {
                let outer::Import { path, start: istart, end: iend } = import;
                let outer::PathName { segments, start, end } = path;
                inner_content.push(inner::Import::new(
                    inner::PathName { segments, start, end }, istart, iend));
            }
            outer::TopLevelStatement::Const(constant) => {
                let outer::Const { visibility, name, ty, value, start: cstart, end: cend } = constant;
                let visibility = match visibility {
                    outer::Visibility::Public => inner::Visibility::Public,
                    outer::Visibility::Private => inner::Visibility::Private,
                };
                let outer::PathName { segments, start, end } = name;
                let name = inner::PathName { segments, start, end };
                let ty = convert_expression(ty, ExpressionState::InType)?;
                let value = convert_expression(value, ExpressionState::InExpression)?;
                inner_content.push(inner::Const::new(
                    visibility, name, ty, value, cstart, cend));
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
            segments: segments,
            start: start,
            end: end,
        },
        content: inner_content,
    });
}


fn convert_expression(
    expr: outer::ExpressionType,
    state: ExpressionState
) -> Result<inner::ExpressionType, ConversionError> {
    let outer::ExpressionType { mutable, expression, variadic } = expr;

    match expression {
        outer::Expression::Type(builtin) => {
            let inner_builtin = convert_builtin_type(builtin)?;

            match state {
                ExpressionState::InType => {
                    let kind = inner::TypeKind::BuiltIn(inner_builtin);
                    Ok(inner::ExpressionType::new_type(inner::Type::new(mutable, kind, variadic)))
                }
                ExpressionState::InExpression => {
                    let kind = inner::TypeKind::BuiltIn(inner_builtin);
                    let ty = inner::Type::new(mutable, kind, variadic);

                    Ok(inner::ExpressionType::new_expression(
                        inner::Expression::Literal(inner::Literal::Type(Box::new(ty)))))
                }
            }
        }
        outer::Expression::Variable(path) => {
            let outer::PathName { segments, start, end } = path;
            let path = inner::PathName { segments, start, end };

            match state {
                ExpressionState::InType => {
                    if path.segments.last().unwrap().chars().next().unwrap().is_uppercase() {
                        let kind = inner::TypeKind::User(path);
                        Ok(inner::ExpressionType::new_type(inner::Type::new(mutable, kind, variadic)))
                    } else {
                        let kind = inner::TypeKind::Variable(path);
                        Ok(inner::ExpressionType::new_type(inner::Type::new(mutable, kind, variadic)))
                    }
                }
                ExpressionState::InExpression => {
                    if let Some(segments) = path.segments.last() {
                        let mut all_uppercase = true;
                        for (i, c) in segments.chars().enumerate() {

                            if i == 0 && !c.is_uppercase() {
                                return Ok(inner::ExpressionType::new_expression(
                                    inner::Expression::Variable(path)));
                            }
                            
                            if !c.is_uppercase() {
                                all_uppercase = false;
                                break;
                            }
                        }
                        if all_uppercase {
                            Ok(inner::ExpressionType::new_expression(
                                inner::Expression::Variable(path)))
                            
                        } else {
                            Err(ConversionError::InvalidVariableName(path.end - segments.len(), path.end))
                        }
                    } else {
                        let kind = inner::TypeKind::Variable(path);
                        let ty = inner::Type::new(mutable, kind, variadic);

                        Ok(inner::ExpressionType::new_expression(
                            inner::Expression::Literal(inner::Literal::Type(Box::new(ty)))))
                    }
                }
            }
        }
        outer::Expression::Literal(literal) => {
            let inner_literal = match literal {
                outer::Literal::Bool(b) => inner::Literal::Bool(b),
                outer::Literal::Int(n) => inner::Literal::Int(n),
                outer::Literal::Float(f) => inner::Literal::Float(f),
                outer::Literal::Char(c) => inner::Literal::Char(c),
                outer::Literal::String(s) => inner::Literal::String(s),
                outer::Literal::Unit => {
                    if let ExpressionState::InType = state {
                        let kind = inner::TypeKind::BuiltIn(inner::BuiltinType::Unit);
                        let ty = inner::Type::new(mutable, kind, variadic);
                        return Ok(inner::ExpressionType::new_type(ty));
                    } else {
                        inner::Literal::Unit
                    }
                }
                outer::Literal::Tuple(tuple) => {
                    match state {
                        ExpressionState::InType => {
                            let mut inner_tuple = Vec::new();
                            for expr in tuple {
                                let inner_expr = convert_expression(expr, ExpressionState::InType)?;
                                inner_tuple.push(inner_expr);
                            }
                            let kind = inner::TypeKind::Tuple(inner_tuple);
                            let ty = inner::Type::new(mutable, kind, variadic);
                            return Ok(inner::ExpressionType::new_type(ty));
                        }
                        ExpressionState::InExpression => {
                            let mut inner_tuple = Vec::new();
                            for expr in tuple {
                                let inner_expr = convert_expression(expr, ExpressionState::InExpression)?;
                                match inner_expr {
                                    inner::ExpressionType::Type(_) => {
                                        return Err(ConversionError::FoundTypeInExpressionState);
                                    }
                                    inner::ExpressionType::Expression(expr) => {
                                        inner_tuple.push(expr);
                                    }
                                }
                            }
                            let expr = inner::Expression::Tuple(inner_tuple);
                            return Ok(inner::ExpressionType::new_expression(expr));
                        }
                    }
                }
                outer::Literal::List(list) => {
                    if let ExpressionState::InType = state {
                        return Err(ConversionError::NoListType);
                    }

                    let mut inner_list = Vec::new();
                    for expr in list {
                        let inner_expr = convert_expression(expr, ExpressionState::InExpression)?;
                        inner_list.push(inner_expr);
                    }
                    let expr = inner::Expression::List(inner_list);
                    return Ok(inner::ExpressionType::new_expression(expr));
                }
            };

            Ok(inner::ExpressionType::new_expression(inner::Expression::Literal(inner_literal)))
        }

    }
}

fn convert_builtin_type(builtin: outer::BuiltinType) -> Result<inner::BuiltinType, ConversionError> {
    match builtin {
        outer::BuiltinType::Bool => Ok(inner::BuiltinType::Bool),
        outer::BuiltinType::Nat => Ok(inner::BuiltinType::Nat),
        outer::BuiltinType::Int => Ok(inner::BuiltinType::Int),
        outer::BuiltinType::F32 => Ok(inner::BuiltinType::F32),
        outer::BuiltinType::F64 => Ok(inner::BuiltinType::F64),
        outer::BuiltinType::I8 => Ok(inner::BuiltinType::I8),
        outer::BuiltinType::I16 => Ok(inner::BuiltinType::I16),
        outer::BuiltinType::I32 => Ok(inner::BuiltinType::I32),
        outer::BuiltinType::I64 => Ok(inner::BuiltinType::I64),
        outer::BuiltinType::Char => Ok(inner::BuiltinType::Char),
        outer::BuiltinType::Unit => Ok(inner::BuiltinType::Unit),
        outer::BuiltinType::Never => Ok(inner::BuiltinType::Never),
        outer::BuiltinType::Type => Ok(inner::BuiltinType::Type),
        outer::BuiltinType::Function { params, return_type } => {
            let mut inner_params = Vec::new();
            for param in params {
                let inner_param = convert_expression(param, ExpressionState::InType)?;
                inner_params.push(inner_param);
            }
            let inner_return_type = convert_expression(*return_type, ExpressionState::InType)?;

            return Ok(inner::BuiltinType::Function {
                params: inner_params,
                return_type: Box::new(inner_return_type),
            });
        }
    }
}
