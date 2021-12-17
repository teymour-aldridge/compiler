//! Collects constraints from an AST.

use crate::{
    diagnostics::span::{HasSpan, Span},
    id::{
        Id, TaggedAst, TaggedBranch, TaggedExpr, TaggedExprInner, TaggedFunc, TaggedNode,
        TaggedRecord,
    },
    parse::{
        expr::{BinOp, UnOp},
        lit::Literal,
        Node,
    },
    visitor::Visitor,
};

use super::Ty;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
/// The possible constraints.
pub(crate) enum Constraint {
    #[allow(unused)]
    /// A specific item with the given [crate::id::Id] must have the given base type.
    IdToTy { id: Id, ty: Ty },
    /// A specific item with the given [crate::id::Id] must have the same type as a different item.
    #[allow(unused)]
    IdToId { id: Id, to: Id },
    /// A specific type must be the same as a different item.
    ///
    /// Note that this is used later, while solving the constraints and not while collecting
    /// constraints from the AST.
    TyToTy { ty: Ty, to: Ty },
}

pub(crate) fn collect(ast: &TaggedAst) -> Result<Vec<Constraint>, ConstraintGatheringError> {
    let mut visitor = ConstraintVisitor::new(ast);
    visitor
        .visit_ast(ast)
        .into_iter()
        .collect::<Result<_, _>>()?;
    Ok(visitor.take_constraints())
}

struct ConstraintVisitor<'a, 'ctx> {
    constraints: Vec<Constraint>,
    definitions: Vec<&'a TaggedFunc<'ctx>>,
    records: Vec<&'a TaggedRecord<'ctx>>,
    current_func: Option<&'a TaggedFunc<'ctx>>,
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

/// Collects a reference to every records in a file.
///
/// todo: add a name resolution algorithm
fn gather_record_definitions<'a, 'collect>(
    ast: &'collect TaggedAst<'a>,
) -> Vec<&'collect TaggedRecord<'a>> {
    ast.nodes
        .iter()
        .filter_map(|node: &TaggedNode| match node {
            Node::Record(r) => Some(r),
            _ => None,
        })
        .collect()
}

impl<'a, 'ctx> ConstraintVisitor<'a, 'ctx> {
    fn new(ast: &'a TaggedAst<'ctx>) -> Self {
        Self {
            constraints: vec![],
            definitions: gather_function_definitions(ast),
            records: gather_record_definitions(ast),
            current_func: None,
        }
    }

    fn take_constraints(self) -> Vec<Constraint> {
        self.constraints
    }
}

impl<'a, 'ctx> Visitor<'a, 'ctx> for ConstraintVisitor<'a, 'ctx> {
    type Output = Result<(), ConstraintGatheringError>;

    fn visit_expr(&mut self, expr: &'a TaggedExpr<'ctx>) -> Self::Output {
        self.constraints
            .extend(collect_expr(expr, &self.definitions, &self.records, None)?);
        Ok(())
    }

    fn visit_for(&mut self, stmt: &'a crate::id::TaggedFor<'ctx>) -> Self::Output {
        self.constraints.push(Constraint::IdToTy {
            id: stmt.var.id,
            ty: Ty::Int,
        });
        self.constraints.push(Constraint::IdToTy {
            id: stmt.between.start.id,
            ty: Ty::Int,
        });
        self.constraints.push(Constraint::IdToTy {
            id: stmt.between.stop.id,
            ty: Ty::Int,
        });
        if let Some(ref step) = stmt.between.step {
            self.constraints.push(Constraint::IdToTy {
                id: step.id,
                ty: Ty::Int,
            });
        }
        self.visit_block(&stmt.block)
            .into_iter()
            .collect::<Result<_, _>>()?;
        Ok(())
    }

    fn visit_if(&mut self, stmt: &'a crate::id::TaggedIf<'ctx>) -> Self::Output {
        fn branch_constraints<'a, 'ctx>(
            branch: &'a TaggedBranch<'ctx>,
            visitor: &mut ConstraintVisitor<'a, 'ctx>,
        ) -> Result<(), ConstraintGatheringError> {
            visitor.visit_expr(&branch.condition)?;
            visitor
                .visit_block(&branch.block)
                .into_iter()
                .collect::<Result<_, _>>()?;
            Ok(())
        }

        branch_constraints(&stmt.r#if, self)?;

        for each in &stmt.else_ifs {
            branch_constraints(each, self)?;
        }

        if let Some(ref r#else) = stmt.r#else {
            self.visit_block(r#else)
                .into_iter()
                .collect::<Result<_, _>>()?;
        }

        Ok(())
    }

    fn visit_while(&mut self, stmt: &'a crate::id::TaggedWhile<'ctx>) -> Self::Output {
        self.constraints.push(Constraint::IdToTy {
            id: stmt.condition.id,
            ty: Ty::Bool,
        });

        self.visit_block(&stmt.block)
            .into_iter()
            .collect::<Result<_, _>>()?;

        Ok(())
    }

    fn visit_ret(&mut self, ret: &'a crate::id::TaggedReturn<'ctx>) -> Self::Output {
        if let Some(func) = self.current_func {
            self.constraints.push(Constraint::IdToId {
                id: func.name.id,
                to: ret.expr.id,
            });
            self.visit_expr(&ret.expr)
        } else {
            Err(ConstraintGatheringError::ReturnOutsideFunction {
                span: ret.expr.span(),
                explanation: "Return statements can only be used inside functions".to_string(),
            })
        }
    }

    fn visit_func(&mut self, func: &'a TaggedFunc<'ctx>) -> Self::Output {
        let prev = self.current_func;
        self.current_func = Some(func);
        self.visit_block(&func.block)
            .into_iter()
            .collect::<Result<_, _>>()?;
        self.current_func = prev;
        Ok(())
    }

    /// Doesn't do anything.
    fn visit_ident(&mut self, _: &crate::id::TaggedIdent) -> Self::Output {
        Ok(())
    }

    fn visit_rec(&mut self, rec: &'a crate::id::TaggedRecord<'ctx>) -> Self::Output {
        for field in &rec.fields {
            self.constraints.push(Constraint::IdToTy {
                id: field.name.id,
                ty: field.ty,
            })
        }
        Ok(())
    }
}

/// Collects constraints from a given expression.
///
/// The type that this expression should conform to. The function will insert constraints as
/// needed.
fn collect_expr<'a, 'ctx>(
    expr: &'a TaggedExpr<'ctx>,
    definitions: &Vec<&'a TaggedFunc<'ctx>>,
    record_definitions: &Vec<&'a TaggedRecord<'ctx>>,
    ty: Option<Ty>,
) -> Result<Vec<Constraint>, ConstraintGatheringError> {
    let mut constraints = vec![];

    if let Some(ty) = ty {
        constraints.push(Constraint::IdToTy { id: expr.id, ty });
    }

    match &expr.token {
        TaggedExprInner::Ident(ident) => constraints.push(Constraint::IdToId {
            id: ident.id,
            to: expr.id,
        }),
        TaggedExprInner::Literal(lit) => {
            let ty = match lit.token {
                Literal::String(_) => Ty::String,
                Literal::Number(_) => Ty::Int,
                Literal::Bool(_) => Ty::Bool,
            };
            constraints.push(Constraint::IdToTy { id: expr.id, ty });
        }
        TaggedExprInner::BinOp(op, left, right) => match (op.token, left, right) {
            (BinOp::Add | BinOp::Divide | BinOp::Multiply | BinOp::Subtract, left, right) => {
                constraints.push(Constraint::IdToId {
                    id: left.id,
                    to: right.id,
                });
                constraints.push(Constraint::IdToId {
                    id: left.id,
                    to: expr.id,
                });
                constraints.extend(collect_expr(left, definitions, record_definitions, None)?);
                constraints.extend(collect_expr(right, definitions, record_definitions, None)?);
            }
            (BinOp::Dot, _left, right) => match right.token {
                TaggedExprInner::Ident(ref ident) => {
                    constraints.push(Constraint::IdToId {
                        id: ident.id,
                        to: right.id,
                    });
                    constraints.push(Constraint::IdToId {
                        id: ident.id,
                        to: expr.id,
                    });
                    constraints.push(Constraint::IdToId {
                        id: right.id,
                        to: expr.id,
                    });
                }
                // todo: methods
                _ => todo!(),
            },
            // todo: add necessary additional type constraints
            (BinOp::IsEqual, left, right) => {
                constraints.push(Constraint::IdToId {
                    id: left.id,
                    to: right.id,
                });
                constraints.push(Constraint::IdToTy {
                    id: expr.id,
                    ty: Ty::Bool,
                });
                constraints.extend(collect_expr(left, definitions, record_definitions, None)?);
                constraints.extend(collect_expr(right, definitions, record_definitions, None)?);
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
                    constraints.extend(collect_expr(left, definitions, record_definitions, None)?);
                    constraints.extend(collect_expr(right, definitions, record_definitions, None)?);
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
        TaggedExprInner::UnOp(op, arg) => match op.token {
            UnOp::Positive | UnOp::Negative => constraints.push(Constraint::IdToTy {
                id: arg.id,
                ty: Ty::Int,
            }),
        },
        TaggedExprInner::Constructor(rec) => {
            let definition = record_definitions
                .iter()
                .find(|candidate| rec.name.token == candidate.name.token)
                .unwrap();

            // todo: proper checks for matching fields
            for (struct_field, (constructor_name, constructor_expr)) in
                definition.fields.iter().zip(rec.fields.iter())
            {
                constraints.push(Constraint::IdToId {
                    id: struct_field.name.id,
                    to: constructor_name.id,
                });
                constraints.push(Constraint::IdToId {
                    id: struct_field.name.id,
                    to: constructor_expr.id,
                });
                constraints.push(Constraint::IdToId {
                    id: constructor_name.id,
                    to: constructor_expr.id,
                });
                constraints.push(Constraint::IdToId {
                    id: constructor_name.id,
                    to: expr.id,
                });
                constraints.push(Constraint::IdToId {
                    id: struct_field.name.id,
                    to: expr.id,
                });
                constraints.push(Constraint::IdToTy {
                    id: expr.id,
                    ty: struct_field.ty,
                });
            }
        }
        TaggedExprInner::FunctionCall(func, params) => {
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
                constraints.extend(collect_expr(
                    param,
                    definitions,
                    record_definitions,
                    Some(Ty::Int),
                )?);
            } else if *func.token == "print" {
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
                    ty: Ty::String,
                });
                constraints.push(Constraint::IdToId {
                    id: expr.id,
                    to: param.id,
                });
                constraints.extend(collect_expr(
                    param,
                    definitions,
                    record_definitions,
                    Some(Ty::String),
                )?);
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
                    constraints.extend(collect_expr(b, definitions, record_definitions, None)?);
                }
                constraints.push(Constraint::IdToId {
                    id: func.id,
                    to: expr.id,
                });
            } else {
                return Err(ConstraintGatheringError::UnresolvableFunction {
                    span: func.span(),
                    explanation: { format!("A function with name `{}` cannot be found.", **func) },
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
