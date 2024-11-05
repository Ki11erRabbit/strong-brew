
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
    let inner_ty = convert_expression(ty)?;
    let inner_value = convert_expression(value)?;
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
        let inner_ty = convert_expression(ty)?;
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
            let inner_return_type = convert_expression(return_type)?;
            let inner_body = convert_block(body)?;
            Ok(inner::Function::new(inner_visibility, inner_name, inner_generic_params, inner_params, inner_return_type, inner_body, start, end))
        }
        outer::Function::Extern { visibility, language, name, generic_params, params, return_type, body, start, end } => {
            let inner_visibility = convert_visibility(visibility);
            let outer::PathName { segments, start, end } = name;
            let inner_name = inner::PathName { segments, start, end };
            let inner_generic_params = convert_generic_params(generic_params);
            let inner_params = convert_params(params)?;
            let inner_return_type = convert_expression(return_type)?;
            Ok(inner::Function::new_extern(inner_visibility, language, inner_name, inner_generic_params, inner_params, inner_return_type, body, start, end))
        }
    }
}

fn convert_params(params: Vec<outer::Param>) -> Result<Vec<inner::Param>, ConversionError> {
    let mut inner_params = Vec::new();
    for param in params {
        let outer::Param { implicit, name, ty, start, end } = param;
        let inner_name = name;
        let inner_ty = convert_expression(ty)?;
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
            let inner_constraint = convert_expression(constraint).unwrap();
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
                let inner_ty = convert_expression(ty)?;
                let inner_value = convert_expression(value)?;
                inner_block.push(inner::Statement::new_let(inner_name, inner_ty, inner_value, start, end));
            }
            outer::Statement::Const { name, ty, value, start, end } => {
                let inner_name = name;
                let inner_ty = convert_expression(ty)?;
                let inner_value = convert_expression(value)?;
                inner_block.push(inner::Statement::new_const(inner_name, inner_ty, inner_value, start, end));
            }
            outer::Statement::Assignment { target, value, start, end } => {
                let inner_target = convert_expression(target)?;
                let inner_value = convert_expression(value)?;
                inner_block.push(inner::Statement::new_assignment(inner_target, inner_value, start, end));
            }
            outer::Statement::Expression(expression) => {
                let inner_expression = convert_expression(expression)?;
                inner_block.push(inner::Statement::new_expression(inner_expression));
            }
        }
    }
    Ok(inner_block)
}

fn convert_pattern(pattern: outer::Pattern) -> Result<inner::Pattern, ConversionError> {
    match pattern {
        outer::Pattern::Variable(name) => Ok(inner::Pattern::Variable(name)),
        outer::Pattern::Literal(lit) => Ok(convert_literal(lit)),
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

fn convert_expression(expression: outer::Expression) -> Result<inner::Expression, ConversionError> {
    match expression {
        outer::Expression::Type(builtin) => {
            let inner_builtin = convert_builtin(builtin);
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
                external => Ok(inner::Expression::Literal(convert_literal(external))),
            }
        },
        outer::Expression::Call(call) => {

        }
    }
}
