//! Collects constraints from an AST.

use crate::{
    diagnostics::span::{HasSpan, Spanned},
    parse::{
        expr::{BinOp, Expr, UnOp},
        func::{Func, FuncRef, Return},
        ident::Ident,
        lit::Literal,
        r#for::ForLoop,
        r#if::{Branch, If},
        r#while::While,
        record::{Record, RecordRef},
        table::{Id, ParseTable, WithId},
    },
    visitor::IdVisitor,
};

use super::{error::ConstraintGatheringError, track::ConstraintId, PrimitiveType, Ty};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
/// The possible constraints.
pub(crate) enum ConstraintInner {
    #[allow(unused)]
    /// A specific item with the given [crate::id::Id] must have the given base type.
    IdToTy { id: Spanned<Id>, ty: Spanned<Ty> },
    /// A specific item with the given [crate::id::Id] must have the same type as a different item.
    #[allow(unused)]
    IdToId { id: Spanned<Id>, to: Spanned<Id> },
    /// A specific type must be the same as a different item. This is never
    /// produced during the constraint-gathering phase - it is used when we
    /// eliminate the variable ids.
    TyToTy { ty: Spanned<Ty>, to: Spanned<Ty> },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct Constraint {
    pub(crate) id: ConstraintId,
    pub(crate) inner: ConstraintInner,
}

impl Constraint {
    pub(crate) fn new(id: ConstraintId, inner: ConstraintInner) -> Self {
        Self { id, inner }
    }
}

pub(crate) fn collect<'i>(
    ast: &'i ParseTable<'i>,
) -> Result<Vec<Constraint>, ConstraintGatheringError> {
    let mut visitor = ConstraintVisitor::new();
    visitor
        .visit_table(ast)
        .into_iter()
        .collect::<Result<_, _>>()?;
    Ok(visitor.take_constraints())
}

struct ConstraintVisitor {
    constraints: Vec<Constraint>,
    current_func: Option<FuncRef>,
    id: ConstraintId,
}

impl ConstraintVisitor {
    fn new_id(&mut self) -> ConstraintId {
        let res = self.id;
        self.id.inner += 1;
        res
    }

    fn add_constraint(&mut self, inner: ConstraintInner) {
        let id = self.new_id();
        self.constraints.push(Constraint::new(id, inner))
    }

    fn extend_constraints(&mut self, inner: impl IntoIterator<Item = ConstraintInner>) {
        // todo: can this allocation be removed?
        // does it even matter?
        let res: Vec<Constraint> = inner
            .into_iter()
            .map(|inner| Constraint::new(self.new_id(), inner))
            .collect();
        self.constraints.extend(res)
    }
}

impl ConstraintVisitor {
    fn new() -> Self {
        Self {
            constraints: vec![],
            current_func: None,
            id: ConstraintId::default(),
        }
    }

    fn take_constraints(self) -> Vec<Constraint> {
        self.constraints
    }
}

impl<'i> IdVisitor<'i> for ConstraintVisitor {
    type Output = Result<(), ConstraintGatheringError>;

    fn visit_rec(&mut self, rec: WithId<&'i Record>, table: &'i ParseTable<'i>) -> Self::Output {
        for field in &rec.inner().fields {
            self.add_constraint(ConstraintInner::IdToTy {
                id: Spanned::new(table.get_ident(field.name).span(table), field.name.id),
                ty: field.ty.map(Ty::PrimitiveType),
            })
        }
        Ok(())
    }

    fn visit_expr(
        &mut self,
        expr: WithId<&'i Expr<'i>>,
        table: &'i ParseTable<'i>,
    ) -> Self::Output {
        self.extend_constraints(collect_expr(expr, table, None)?);
        Ok(())
    }

    fn visit_for(&mut self, stmt: WithId<&'i ForLoop>, table: &'i ParseTable<'i>) -> Self::Output {
        self.add_constraint(ConstraintInner::IdToTy {
            id: Spanned::new(
                table.get_ident(stmt.inner().var).span(table),
                table.get_ident_with_id(stmt.inner().var).id(),
            ),
            // todo: better span here?
            ty: Spanned::new(
                table.get_ident(stmt.inner().var).span(table),
                Ty::PrimitiveType(PrimitiveType::Int),
            ),
        });
        self.add_constraint(ConstraintInner::IdToTy {
            id: Spanned::new(
                table.get_expr(&stmt.inner().between.start).span(table),
                stmt.inner().between.start.id,
            ),
            ty: Spanned::new(
                table.get_expr(&stmt.inner().between.start).span(table),
                Ty::PrimitiveType(PrimitiveType::Int),
            ),
        });
        self.add_constraint(ConstraintInner::IdToTy {
            id: Spanned::new(
                table.get_expr(&stmt.inner().between.stop).span(table),
                stmt.inner().between.stop.id,
            ),
            ty: Spanned::new(
                table.get_expr(&stmt.inner().between.stop).span(table),
                Ty::PrimitiveType(PrimitiveType::Int),
            ),
        });
        if let Some(ref step) = stmt.inner().between.step {
            self.add_constraint(ConstraintInner::IdToTy {
                id: Spanned::new(table.get_expr(step).span(table), step.id),
                ty: Spanned::new(
                    table.get_expr(step).span(table),
                    Ty::PrimitiveType(PrimitiveType::Int),
                ),
            });
        }
        self.visit_block(table.get_block_with_id(stmt.inner().block), table)
            .into_iter()
            .collect::<Result<_, _>>()?;
        Ok(())
    }

    fn visit_if(&mut self, stmt: WithId<&'i If>, table: &'i ParseTable<'i>) -> Self::Output {
        fn branch_constraints<'i>(
            branch: &'i Branch,
            table: &'i ParseTable<'i>,
            visitor: &mut ConstraintVisitor,
        ) -> Result<(), ConstraintGatheringError> {
            visitor.visit_expr(table.get_expr_with_id(branch.condition), table)?;
            visitor
                .visit_block(table.get_block_with_id(branch.block), table)
                .into_iter()
                .collect::<Result<_, _>>()?;
            Ok(())
        }

        branch_constraints(&stmt.inner().r#if, table, self)?;

        for each in &stmt.inner().else_ifs {
            branch_constraints(each, table, self)?;
        }

        if let Some(ref r#else) = stmt.inner().r#else {
            self.visit_block(table.get_block_with_id(*r#else), table)
                .into_iter()
                .collect::<Result<_, _>>()?;
        }

        Ok(())
    }

    fn visit_while(&mut self, stmt: WithId<&'i While>, table: &'i ParseTable<'i>) -> Self::Output {
        self.add_constraint(ConstraintInner::IdToTy {
            id: Spanned::new(
                table.get_expr(&stmt.inner().condition).span(table),
                stmt.inner().condition.id,
            ),
            // todo: better span?
            ty: Spanned::new(
                table.get_expr(&stmt.inner().condition).span(table),
                Ty::PrimitiveType(PrimitiveType::Bool),
            ),
        });

        self.visit_block(table.get_block_with_id(stmt.inner().block), table)
            .into_iter()
            .collect::<Result<_, _>>()?;

        Ok(())
    }

    fn visit_ret(&mut self, ret: WithId<&'i Return>, table: &'i ParseTable<'i>) -> Self::Output {
        if let Some(func) = self.current_func {
            self.add_constraint(ConstraintInner::IdToId {
                id: Spanned::new(
                    table.get_ident(table.get_func(func).name).span(table),
                    table.get_func(func).name.id,
                ),
                to: Spanned::new(
                    table.get_expr(&ret.inner().expr).span(table),
                    table.get_expr_with_id(ret.inner().expr).id(),
                ),
            });
            self.visit_expr(table.get_expr_with_id(ret.inner().expr), table)
        } else {
            Err(ConstraintGatheringError::ReturnOutsideFunction {
                span: table.get_expr(&ret.inner().expr).span(table).into(),
                explanation: "Return statements can only be used inside functions".to_string(),
            })
        }
    }

    fn visit_func(&mut self, func: WithId<&'i Func>, table: &'i ParseTable<'i>) -> Self::Output {
        let prev = self.current_func;
        self.current_func = Some(FuncRef { id: func.id() });
        self.visit_block(table.get_block_with_id(func.inner().block), table)
            .into_iter()
            .collect::<Result<_, _>>()?;
        self.current_func = prev;
        Ok(())
    }

    /// Doesn't do anything.
    fn visit_ident(&mut self, _: WithId<&Ident<'i>>, _: &'i ParseTable<'i>) -> Self::Output {
        Ok(())
    }
}

/// Collects constraints from a given expression.
///
/// The type that this expression should conform to. The function will insert constraints as
/// needed.
fn collect_expr<'i>(
    expr: WithId<&'i Expr<'i>>,
    table: &'i ParseTable<'i>,
    ty: Option<Ty>,
) -> Result<Vec<ConstraintInner>, ConstraintGatheringError> {
    let mut constraints = vec![];

    if let Some(ty) = ty {
        constraints.push(ConstraintInner::IdToTy {
            id: Spanned::new(expr.inner().span(table), expr.id()),
            ty: Spanned::new(expr.inner().span(table), ty),
        });
    }

    match &expr.inner() {
        Expr::Ident(ident) => constraints.push(ConstraintInner::IdToId {
            id: Spanned::new(table.get_ident(*ident).span(table), ident.id),
            to: Spanned::new(expr.inner().span(table), expr.id()),
        }),
        Expr::Literal(lit) => {
            let ty = match lit.token {
                Literal::String(_) => Ty::PrimitiveType(PrimitiveType::StrSlice),
                Literal::Number(_) => Ty::PrimitiveType(PrimitiveType::Int),
                Literal::Bool(_) => Ty::PrimitiveType(PrimitiveType::Bool),
            };
            constraints.push(ConstraintInner::IdToTy {
                id: Spanned::new(expr.inner().span(table), expr.id()),
                ty: Spanned::new(expr.inner().span(table), ty),
            });
        }
        Expr::BinOp(op, left, right) => match (op.token, left, right) {
            (BinOp::Add | BinOp::Divide | BinOp::Multiply | BinOp::Subtract, left, right) => {
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(table.get_expr(left).span(table), left.id),
                    to: Spanned::new(table.get_expr(right).span(table), right.id),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(table.get_expr(left).span(table), left.id),
                    to: Spanned::new(expr.inner().span(table), expr.id()),
                });
                constraints.extend(collect_expr(table.get_expr_with_id(*left), table, None)?);
                constraints.extend(collect_expr(table.get_expr_with_id(*right), table, None)?);
            }
            (BinOp::Dot, _left, right) => match table.get_expr(right) {
                Expr::Ident(ref ident) => {
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(table.get_ident(*ident).span(table), ident.id),
                        to: Spanned::new(table.get_expr(right).span(table), right.id),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(
                            table.get_ident(*ident).span(table),
                            table.get_ident_with_id(*ident).id(),
                        ),
                        to: Spanned::new(expr.inner().span(table), expr.id()),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(table.get_expr(right).span(table), right.id),
                        to: Spanned::new(expr.inner().span(table), expr.id()),
                    });
                }
                Expr::FunctionCall(_, _) => {
                    todo!("methods have not yet been implemented")
                }
                // todo: methods
                e => {
                    return Err(ConstraintGatheringError::LiteralForFieldOrMethodAccess {
                        span: e.span(table).index_only(),
                        explanation:
                            "The dot (`.`) operator calls fields, or accesses methods on structs. Only \
                            function calls or identifiers (variable names) are valid in this position."
                                .to_owned(),
                    })
                }
            },
            // todo: add necessary additional type constraints
            (BinOp::IsEqual | BinOp::IsNotEqual, left, right) => {
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(
                        table.get_expr(left).span(table),
                        table.get_expr_with_id(*left).id(),
                    ),
                    to: Spanned::new(
                        table.get_expr(right).span(table),
                        table.get_expr_with_id(*right).id(),
                    ),
                });
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(expr.inner().span(table), expr.id()),
                    ty: Spanned::new(
                        expr.inner().span(table),
                        Ty::PrimitiveType(PrimitiveType::Bool),
                    ),
                });
                constraints.extend(collect_expr(table.get_expr_with_id(*left), table, None)?);
                constraints.extend(collect_expr(table.get_expr_with_id(*right), table, None)?);
            }
            (BinOp::SetEquals, left, right) => match table.get_expr(left) {
                Expr::Ident(ref ident) => {
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(table.get_ident(*ident).span(table), ident.id),
                        to: Spanned::new(table.get_expr(right).span(table), right.id),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(
                            table.get_expr(left).span(table),
                            table.get_expr_with_id(*left).id(),
                        ),
                        to: Spanned::new(
                            table.get_expr(right).span(table),
                            table.get_expr_with_id(*right).id(),
                        ),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(
                            table.get_ident(*ident).span(table),
                            table.get_ident_with_id(*ident).id(),
                        ),
                        to: Spanned::new(
                            table.get_expr(left).span(table),
                            table.get_expr_with_id(*left).id(),
                        ),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(expr.inner().span(table), expr.id()),
                        to: Spanned::new(
                            table.get_expr(left).span(table),
                            table.get_expr_with_id(*left).id(),
                        ),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(expr.inner().span(table), expr.id()),
                        to: Spanned::new(
                            table.get_expr(right).span(table),
                            table.get_expr_with_id(*right).id(),
                        ),
                    });
                    constraints.extend(collect_expr(table.get_expr_with_id(*left), table, None)?);
                    constraints.extend(collect_expr(table.get_expr_with_id(*right), table, None)?);
                }
                Expr::UnOp(op, ref pointer) if op.token.is_deref() => {
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(expr.inner().span(table), expr.id()),
                        // todo: use the span of the operator
                        to: Spanned::new(
                            table.get_expr(pointer).span(table),
                            table.get_expr_with_id(*pointer).id(),
                        ),
                    });
                    constraints.push(ConstraintInner::IdToTy {
                        id: Spanned::new(
                            table.get_expr(pointer).span(table),
                            table.get_expr_with_id(*pointer).id(),
                        ),
                        // todo: use the span of the operator
                        ty: Spanned::new(
                            table.get_expr(pointer).span(table),
                            Ty::PrimitiveType(PrimitiveType::Pointer),
                        ),
                    });
                    constraints.extend(collect_expr(table.get_expr_with_id(*pointer), table, None)?)
                }
                _ => {
                    return Err(ConstraintGatheringError::CannotAssignToExpression {
                        span: expr.inner().span(table).into(),
                        explanation:
                            "Values can only be assigned to variables, not to expressions!"
                                .to_string(),
                    });
                }
            },
            // todo: sort this out (memory safety)
            (BinOp::Index, left, right) => {
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(expr.inner().span(table), expr.id()),
                    ty: Spanned::new(
                        expr.inner().span(table),
                        Ty::PrimitiveType(PrimitiveType::Pointer),
                    ),
                });
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(table.get_expr(left).span(table), left.id),
                    // todo: use the span of the operator
                    ty: Spanned::new(
                        table.get_expr(left).span(table),
                        Ty::PrimitiveType(PrimitiveType::Pointer),
                    ),
                });
                constraints.extend(collect_expr(table.get_expr_with_id(*left), table, None)?);
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(
                        table.get_expr(right).span(table),
                        table.get_expr_with_id(*right).id(),
                    ),
                    ty: Spanned::new(
                        table.get_expr(right).span(table),
                        Ty::PrimitiveType(PrimitiveType::Int),
                    ),
                });
                constraints.extend(collect_expr(table.get_expr_with_id(*right), table, None)?);
            }
        },
        Expr::UnOp(op, arg) => match op.token {
            UnOp::Positive | UnOp::Negative => constraints.push(ConstraintInner::IdToTy {
                id: Spanned::new(table.get_expr(arg).span(table), arg.id),
                ty: Spanned::new(
                    table.get_expr(arg).span(table),
                    Ty::PrimitiveType(PrimitiveType::Int),
                ),
            }),
            UnOp::Deref => constraints.push(ConstraintInner::IdToTy {
                id: Spanned::new(table.get_expr(arg).span(table), arg.id),
                ty: Spanned::new(
                    table.get_expr(arg).span(table),
                    Ty::PrimitiveType(PrimitiveType::Pointer),
                ),
            }),
        },
        Expr::Constructor(rec) => {
            let definition = table
                .record_
                .iter()
                .find(|(_, record)| table.get_ident(record.name) == table.get_ident(rec.name))
                .unwrap();

            constraints.push(ConstraintInner::IdToTy {
                id: Spanned::new(expr.inner().span(table), expr.id()),
                ty: Spanned::new(
                    definition.1.span(table),
                    Ty::Record {
                        ref_: RecordRef { id: *definition.0 },
                    },
                ),
            });

            // todo: proper checks for matching fields
            for (struct_field, (constructor_name, constructor_expr)) in
                definition.1.fields.iter().zip(rec.fields.iter())
            {
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(
                        table.get_ident(struct_field.name).span(table),
                        struct_field.name.id,
                    ),
                    to: Spanned::new(
                        table.get_ident(*constructor_name).span(table),
                        constructor_name.id,
                    ),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(
                        table.get_ident(struct_field.name).span(table),
                        struct_field.name.id,
                    ),
                    to: Spanned::new(
                        table.get_expr(constructor_expr).span(table),
                        constructor_expr.id,
                    ),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(
                        table.get_ident(*constructor_name).span(table),
                        constructor_name.id,
                    ),
                    to: Spanned::new(
                        table.get_expr(constructor_expr).span(table),
                        table.get_expr_with_id(*constructor_expr).id(),
                    ),
                });
            }
        }
        Expr::FunctionCall(func, params) => {
            if table.get_ident(*func).inner == "print_int" {
                if params.len() != 1 {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: table.get_ident(*func).span(table).into(),
                        explanation: format!(
                            "This function accepts 1
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }
                let param = &params[0];
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(table.get_expr(param).span(table), param.id),
                    ty: Spanned::new(
                        table.get_expr(param).span(table),
                        Ty::PrimitiveType(PrimitiveType::Int),
                    ),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(expr.inner().span(table), expr.id()),
                    to: Spanned::new(table.get_expr(param).span(table), param.id),
                });
                constraints.extend(collect_expr(
                    table.get_expr_with_id(*param),
                    table,
                    Some(Ty::PrimitiveType(PrimitiveType::Int)),
                )?);
            } else if table.get_ident(*func).inner == "print" {
                if params.len() != 1 {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: table.get_ident(*func).span(table).into(),
                        explanation: format!(
                            "This function accepts 1
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }
                let param = &params[0];
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(table.get_expr(param).span(table), param.id),
                    ty: Spanned::new(
                        table.get_expr(param).span(table),
                        Ty::PrimitiveType(PrimitiveType::StrSlice),
                    ),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(expr.inner().span(table), expr.id()),
                    to: Spanned::new(
                        table.get_expr(param).span(table),
                        table.get_expr_with_id(*param).id(),
                    ),
                });
                constraints.extend(collect_expr(
                    table.get_expr_with_id(*param),
                    table,
                    Some(Ty::PrimitiveType(PrimitiveType::StrSlice)),
                )?);
            // todo: make memory allocation functions standard-library only!
            } else if table.get_ident(*func).inner == "malloc" {
                if params.len() != 1 {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: table.get_ident(*func).span(table).into(),
                        explanation: format!(
                            "This function accepts 1
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }
                let param = &params[0];
                constraints.extend(collect_expr(table.get_expr_with_id(*param), table, None)?);
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(
                        table.get_expr(param).span(table),
                        table.get_expr_with_id(*param).id(),
                    ),
                    ty: Spanned::new(
                        table.get_expr(param).span(table),
                        Ty::PrimitiveType(PrimitiveType::Int),
                    ),
                });
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(
                        table.get_ident(*func).span(table),
                        table.get_ident_with_id(*func).id(),
                    ),
                    ty: Spanned::new(
                        table.get_ident(*func).span(table),
                        Ty::PrimitiveType(PrimitiveType::Pointer),
                    ),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(
                        table.get_ident(*func).span(table),
                        table.get_ident_with_id(*func).id(),
                    ),
                    to: Spanned::new(expr.inner().span(table), expr.id()),
                });
            } else if table.get_ident(*func).inner == "free" {
                if params.len() != 1 {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: table.get_ident(*func).span(table).into(),
                        explanation: format!(
                            "This function accepts 1
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }
                let param = &params[0];
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(table.get_expr(param).span(table), param.id),
                    ty: Spanned::new(
                        table.get_expr(param).span(table),
                        Ty::PrimitiveType(PrimitiveType::Pointer),
                    ),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(expr.inner().span(table), expr.id()),
                    to: Spanned::new(
                        table.get_expr(param).span(table),
                        table.get_expr_with_id(*param).id(),
                    ),
                });
                constraints.extend(collect_expr(table.get_expr_with_id(*param), table, None)?);
            } else if table.get_ident(*func).inner() == "realloc" {
                if params.len() != 2 {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: table.get_ident(*func).span(table).into(),
                        explanation: format!(
                            "This function accepts 2
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }
                let pointer = &params[0];
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(table.get_expr(pointer).span(table), pointer.id),
                    ty: Spanned::new(
                        table.get_expr(pointer).span(table),
                        Ty::PrimitiveType(PrimitiveType::Pointer),
                    ),
                });
                constraints.extend(collect_expr(
                    table.get_expr_with_id(*pointer),
                    table,
                    Some(Ty::PrimitiveType(PrimitiveType::Pointer)),
                )?);
                let new_size = &params[1];
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(
                        table.get_expr(new_size).span(table),
                        table.get_expr_with_id(*new_size).id(),
                    ),
                    ty: Spanned::new(
                        table.get_expr(new_size).span(table),
                        Ty::PrimitiveType(PrimitiveType::Int),
                    ),
                });
                constraints.extend(collect_expr(
                    table.get_expr_with_id(*new_size),
                    table,
                    Some(Ty::PrimitiveType(PrimitiveType::Int)),
                )?);
            } else if let Some(function) = table.func.iter().find(|function| {
                table.get_ident(function.1.name).inner == table.get_ident(*func).inner
            }) {
                if function.1.parameters.len() != params.len() {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: table.get_ident(*func).span(table).into(),
                        explanation: format!(
                            "This function accepts `{}`
                            parameters, but you've called it with `{}` arguments.",
                            function.1.parameters.len(),
                            params.len()
                        ),
                    });
                }
                for (parameter, argument_expression) in function.1.parameters.iter().zip(params) {
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(
                            table.get_expr(argument_expression).span(table),
                            argument_expression.id,
                        ),
                        to: Spanned::new(table.get_ident(*parameter).span(table), parameter.id),
                    });
                    constraints.extend(collect_expr(
                        table.get_expr_with_id(*argument_expression),
                        table,
                        None,
                    )?);
                }
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(
                        table.get_ident(*func).span(table),
                        table.get_ident_with_id(*func).id(),
                    ),
                    to: Spanned::new(expr.inner().span(table), expr.id()),
                });
            } else {
                return Err(ConstraintGatheringError::UnresolvableFunction {
                    span: table.get_ident(*func).span(table).into(),
                    explanation: {
                        format!(
                            "A function with name `{}` cannot be found.",
                            table.get_ident(*func).inner
                        )
                    },
                });
            }
        }
    }

    Ok(constraints)
}
