use crate::{
    diagnostics::span::{HasSpan, Span},
    id::{
        Id, TaggedAst, TaggedBlock, TaggedBranch, TaggedExpr, TaggedExprInner, TaggedFunc,
        TaggedNode,
    },
    parse::{expr::BinOp, Node},
};

use super::Ty;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum Constraint {
    #[allow(unused)]
    IdToTy {
        id: Id,
        ty: Ty,
    },
    #[allow(unused)]
    IdToId {
        id: Id,
        to: Id,
    },
    TyToTy {
        ty: Ty,
        to: Ty,
    },
}

pub(crate) fn collect(ast: &TaggedAst) -> Result<Vec<Constraint>, ConstraintGatheringError> {
    let mut c = vec![];
    let definitions = gather_function_definitions(ast);
    for node in &ast.nodes {
        c.extend(collect_node(node, &definitions, None)?)
    }
    Ok(c)
}

fn gather_function_definitions<'a, 'collect>(
    ast: &'collect TaggedAst<'a>,
) -> Vec<&'collect TaggedFunc<'a>> {
    ast.nodes
        .iter()
        .filter_map(|node: &TaggedNode| match node {
            Node::Func(f) => Some(f),
            _ => None,
        })
        .collect()
}

fn collect_node(
    node: &TaggedNode,
    definitions: &Vec<&TaggedFunc>,
    current_func: Option<&TaggedFunc>,
) -> Result<Vec<Constraint>, ConstraintGatheringError> {
    match node {
        Node::Expr(expr) => collect_expr(expr, definitions, None),
        Node::For(block) => {
            let mut constraints = vec![];
            constraints.push(Constraint::IdToTy {
                id: block.var.id,
                ty: Ty::Int,
            });
            constraints.push(Constraint::IdToTy {
                id: block.between.start.id,
                ty: Ty::Int,
            });
            constraints.push(Constraint::IdToTy {
                id: block.between.stop.id,
                ty: Ty::Int,
            });
            if let Some(ref step) = block.between.step {
                constraints.push(Constraint::IdToTy {
                    id: step.id,
                    ty: Ty::Int,
                });
            }
            constraints.extend(collect_block(&block.block, definitions, current_func)?);
            Ok(constraints)
        }
        Node::If(stmt) => {
            let mut c = vec![];

            let branch_constraints =
                |branch: &TaggedBranch| -> Result<Vec<Constraint>, ConstraintGatheringError> {
                    let mut c = vec![];
                    c.push(Constraint::IdToTy {
                        id: branch.condition.id,
                        ty: Ty::Bool,
                    });

                    c.extend(collect_block(&stmt.r#if.block, definitions, current_func)?);
                    Ok(c)
                };

            let val = (branch_constraints)(&stmt.r#if)?;
            c.extend(val);

            for each in &stmt.else_ifs {
                let val = (branch_constraints)(&each)?;
                c.extend(val)
            }

            if let Some(ref r#else) = stmt.r#else {
                c.extend(collect_block(r#else, definitions, current_func)?);
            }

            Ok(c)
        }
        Node::While(block) => {
            let mut c = vec![];

            c.push(Constraint::IdToTy {
                id: block.condition.id,
                ty: Ty::Bool,
            });

            c.extend(collect_block(&block.block, definitions, current_func)?);

            Ok(c)
        }
        Node::Return(ret) => {
            let mut c = vec![];
            if let Some(func) = current_func {
                c.push(Constraint::IdToId {
                    id: func.name.id,
                    to: ret.expr.id,
                });
                c.extend(collect_expr(&ret.expr, definitions, None)?);
                Ok(c)
            } else {
                return Err(ConstraintGatheringError::ReturnOutsideFunction {
                    span: ret.expr.span(),
                    explanation: "Return statements can only be used inside functions".to_string(),
                });
            }
        }
        Node::Func(func) => collect_block(&func.block, definitions, Some(func)),
    }
}

fn collect_block(
    block: &TaggedBlock,
    definitions: &Vec<&TaggedFunc>,
    current_func: Option<&TaggedFunc>,
) -> Result<Vec<Constraint>, ConstraintGatheringError> {
    let mut c = vec![];
    for node in &block.inner.nodes {
        c.extend(collect_node(node, definitions, current_func)?);
    }
    Ok(c)
}

fn collect_expr(
    expr: &TaggedExpr,
    definitions: &Vec<&TaggedFunc>,
    ty: Option<Ty>,
) -> Result<Vec<Constraint>, ConstraintGatheringError> {
    let mut constraints = vec![];

    if let Some(ty) = ty {
        constraints.push(Constraint::IdToTy { id: expr.id, ty });
    }

    match &expr.token {
        crate::id::TaggedExprInner::Ident(ident) => constraints.push(Constraint::IdToId {
            id: ident.id,
            to: expr.id,
        }),
        crate::id::TaggedExprInner::Literal(lit) => {
            let ty = match lit.token {
                crate::parse::lit::Literal::String(_) => todo!(),
                crate::parse::lit::Literal::Number(_) => Ty::Int,
                crate::parse::lit::Literal::Bool(_) => Ty::Bool,
            };
            constraints.push(Constraint::IdToTy { id: expr.id, ty });
        }
        crate::id::TaggedExprInner::BinOp(op, left, right) => match (op.token, left, right) {
            (BinOp::Add | BinOp::Divide | BinOp::Multiply | BinOp::Subtract, left, right) => {
                constraints.push(Constraint::IdToId {
                    id: left.id,
                    to: right.id,
                });
                constraints.push(Constraint::IdToId {
                    id: left.id,
                    to: expr.id,
                });
                constraints.extend(collect_expr(&left, definitions, None)?);
                constraints.extend(collect_expr(&right, definitions, None)?);
            }
            (BinOp::SetEquals, left, right) => {
                if let TaggedExprInner::Ident(ref ident) = left.token {
                    constraints.push(Constraint::IdToId {
                        id: ident.id,
                        to: right.id,
                    });
                    constraints.push(Constraint::IdToId {
                        id: left.id,
                        to: right.id,
                    });
                    constraints.push(Constraint::IdToId {
                        id: ident.id,
                        to: left.id,
                    });
                    constraints.push(Constraint::IdToId {
                        id: expr.id,
                        to: left.id,
                    });
                    constraints.push(Constraint::IdToId {
                        id: expr.id,
                        to: right.id,
                    });
                    constraints.extend(collect_expr(&left, definitions, None)?);
                    constraints.extend(collect_expr(&right, definitions, None)?);
                } else {
                    return Err(ConstraintGatheringError::CannotAssignToExpression {
                        span: expr.token.span(),
                        explanation:
                            "Values can only be assigned to variables, not to expressions!"
                                .to_string(),
                    });
                }
            }
        },
        crate::id::TaggedExprInner::UnOp(op, arg) => match op.token {
            crate::parse::expr::UnOp::Positive | crate::parse::expr::UnOp::Negative => constraints
                .push(Constraint::IdToTy {
                    id: arg.id,
                    ty: Ty::Int,
                }),
        },
        crate::id::TaggedExprInner::FunctionCall(func, params) => {
            if *func.token == "print_int" {
                if params.len() != 1 {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: func.span(),
                        explanation: format!(
                            "This function accepts 1
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }

                let param = &params[0];
                constraints.push(Constraint::IdToTy {
                    id: param.id,
                    ty: Ty::Int,
                });
                constraints.push(Constraint::IdToId {
                    id: expr.id,
                    to: param.id,
                });
                constraints.extend(collect_expr(&param, definitions, None)?);
            } else if let Some(function) = definitions
                .iter()
                .find(|function| function.name.token == func.token)
            {
                if function.parameters.len() != params.len() {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: func.span(),
                        explanation: format!(
                            "This function accepts `{}`
                            parameters, but you've called it with `{}` arguments.",
                            function.parameters.len(),
                            params.len()
                        ),
                    });
                }
                for (a, b) in function.parameters.iter().zip(params) {
                    constraints.push(Constraint::IdToId { id: b.id, to: a.id });
                    constraints.extend(collect_expr(b, definitions, None)?);
                }
                constraints.push(Constraint::IdToId {
                    id: func.id,
                    to: expr.id,
                });
            } else {
                dbg!(&*func);
                dbg!(definitions);
                return Err(ConstraintGatheringError::UnresolvableFunction {
                    span: func.span(),
                    explanation: {
                        format!(
                            "A function with name `{}` cannot be found.",
                            func.to_string()
                        )
                    },
                });
            }
        }
    }

    Ok(constraints)
}

#[derive(Debug)]
pub enum ConstraintGatheringError {
    CannotAssignToExpression { span: Span, explanation: String },
    UnresolvableFunction { span: Span, explanation: String },
    MismatchedFunctionCall { span: Span, explanation: String },
    ReturnOutsideFunction { span: Span, explanation: String },
}
