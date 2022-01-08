//! Collects constraints from an AST.

use crate::{
    diagnostics::span::{HasSpan, Spanned},
    id::{
        TaggedAst, TaggedBranch, TaggedExpr, TaggedExprInner, TaggedFunc, TaggedNode, TaggedRecord,
        UniversalId,
    },
    parse::{
        expr::{BinOp, UnOp},
        lit::Literal,
        Node,
    },
    visitor::Visitor,
};

use super::{error::ConstraintGatheringError, track::ConstraintId, Ty};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
/// The possible constraints.
pub(crate) enum ConstraintInner<'ctx> {
    #[allow(unused)]
    /// A specific item with the given [crate::id::Id] must have the given base type.
    IdToTy {
        id: Spanned<UniversalId<'ctx>>,
        ty: Spanned<Ty<'ctx>>,
    },
    /// A specific item with the given [crate::id::Id] must have the same type as a different item.
    #[allow(unused)]
    IdToId {
        id: Spanned<UniversalId<'ctx>>,
        to: Spanned<UniversalId<'ctx>>,
    },
    /// A specific type must be the same as a different item.
    ///
    /// Note that this is used later, while solving the constraints and not while collecting
    /// constraints from the AST.
    TyToTy {
        ty: Spanned<Ty<'ctx>>,
        to: Spanned<Ty<'ctx>>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct Constraint<'ctx> {
    pub(crate) id: ConstraintId,
    pub(crate) inner: ConstraintInner<'ctx>,
}

impl<'ctx> Constraint<'ctx> {
    pub(crate) fn new(id: ConstraintId, inner: ConstraintInner<'ctx>) -> Self {
        Self { id, inner }
    }
}

pub(crate) fn collect<'ctx>(
    ast: &'ctx TaggedAst,
) -> Result<Vec<Constraint<'ctx>>, ConstraintGatheringError> {
    let mut visitor = ConstraintVisitor::new(ast);
    visitor
        .visit_ast(ast)
        .into_iter()
        .collect::<Result<_, _>>()?;
    Ok(visitor.take_constraints())
}

struct ConstraintVisitor<'ctx> {
    constraints: Vec<Constraint<'ctx>>,
    definitions: Vec<&'ctx TaggedFunc<'ctx>>,
    records: Vec<&'ctx TaggedRecord<'ctx>>,
    current_func: Option<&'ctx TaggedFunc<'ctx>>,
    id: ConstraintId,
}

impl<'ctx> ConstraintVisitor<'ctx> {
    fn new_id(&mut self) -> ConstraintId {
        let res = self.id;
        self.id.inner += 1;
        res
    }

    fn add_constraint(&mut self, inner: ConstraintInner<'ctx>) {
        let id = self.new_id();
        self.constraints.push(Constraint::new(id, inner))
    }

    fn extend_constraints(&mut self, inner: impl IntoIterator<Item = ConstraintInner<'ctx>>) {
        // todo: can this allocation be removed?
        // does it even matter?
        let res: Vec<Constraint> = inner
            .into_iter()
            .map(|inner| Constraint::new(self.new_id(), inner))
            .collect();
        self.constraints.extend(res)
    }
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
fn gather_record_definitions<'collect>(
    ast: &'collect TaggedAst<'collect>,
) -> Vec<&'collect TaggedRecord<'collect>> {
    ast.nodes
        .iter()
        .filter_map(|node: &TaggedNode| match node {
            Node::Record(r) => Some(r),
            _ => None,
        })
        .collect()
}

impl<'ctx> ConstraintVisitor<'ctx> {
    fn new(ast: &'ctx TaggedAst<'ctx>) -> Self {
        Self {
            constraints: vec![],
            definitions: gather_function_definitions(ast),
            records: gather_record_definitions(ast),
            current_func: None,
            id: ConstraintId::default(),
        }
    }

    fn take_constraints(self) -> Vec<Constraint<'ctx>> {
        self.constraints
    }
}

impl<'ctx> Visitor<'ctx> for ConstraintVisitor<'ctx> {
    type Output = Result<(), ConstraintGatheringError>;

    fn visit_rec(&mut self, rec: &'ctx crate::id::TaggedRecord<'ctx>) -> Self::Output {
        for field in &rec.fields {
            self.add_constraint(ConstraintInner::IdToTy {
                id: Spanned::new(field.name.span(), field.name.id.into()),
                ty: field.ty.clone(),
            })
        }
        Ok(())
    }

    fn visit_expr(&mut self, expr: &'ctx TaggedExpr<'ctx>) -> Self::Output {
        self.extend_constraints(collect_expr(expr, &self.definitions, &self.records, None)?);
        Ok(())
    }

    fn visit_for(&mut self, stmt: &'ctx crate::id::TaggedFor<'ctx>) -> Self::Output {
        self.add_constraint(ConstraintInner::IdToTy {
            id: Spanned::new(stmt.var.span(), stmt.var.id.into()),
            // todo: better span here?
            ty: Spanned::new(stmt.var.span(), Ty::Int),
        });
        self.add_constraint(ConstraintInner::IdToTy {
            id: Spanned::new(stmt.between.start.span(), stmt.between.start.id.into()),
            ty: Spanned::new(stmt.between.start.span(), Ty::Int),
        });
        self.add_constraint(ConstraintInner::IdToTy {
            id: Spanned::new(stmt.between.stop.span(), stmt.between.stop.id.into()),
            ty: Spanned::new(stmt.between.stop.span(), Ty::Int),
        });
        if let Some(ref step) = stmt.between.step {
            self.add_constraint(ConstraintInner::IdToTy {
                id: Spanned::new(step.span(), step.id.into()),
                ty: Spanned::new(step.span(), Ty::Int),
            });
        }
        self.visit_block(&stmt.block)
            .into_iter()
            .collect::<Result<_, _>>()?;
        Ok(())
    }

    fn visit_if(&mut self, stmt: &'ctx crate::id::TaggedIf<'ctx>) -> Self::Output {
        fn branch_constraints<'ctx>(
            branch: &'ctx TaggedBranch<'ctx>,
            visitor: &mut ConstraintVisitor<'ctx>,
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

    fn visit_while(&mut self, stmt: &'ctx crate::id::TaggedWhile<'ctx>) -> Self::Output {
        self.add_constraint(ConstraintInner::IdToTy {
            id: Spanned::new(stmt.condition.span(), stmt.condition.id.into()),
            // todo: better span?
            ty: Spanned::new(stmt.condition.span(), Ty::Bool),
        });

        self.visit_block(&stmt.block)
            .into_iter()
            .collect::<Result<_, _>>()?;

        Ok(())
    }

    fn visit_ret(&mut self, ret: &'ctx crate::id::TaggedReturn<'ctx>) -> Self::Output {
        if let Some(func) = self.current_func {
            self.add_constraint(ConstraintInner::IdToId {
                id: Spanned::new(func.name.span(), func.name.id.into()),
                to: Spanned::new(ret.expr.span(), ret.expr.id.into()),
            });
            self.visit_expr(&ret.expr)
        } else {
            Err(ConstraintGatheringError::ReturnOutsideFunction {
                span: ret.expr.span().into(),
                explanation: "Return statements can only be used inside functions".to_string(),
            })
        }
    }

    fn visit_func(&mut self, func: &'ctx TaggedFunc<'ctx>) -> Self::Output {
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
}

/// Collects constraints from a given expression.
///
/// The type that this expression should conform to. The function will insert constraints as
/// needed.
fn collect_expr<'ctx>(
    expr: &'ctx TaggedExpr<'ctx>,
    definitions: &[&'ctx TaggedFunc<'ctx>],
    record_definitions: &[&'ctx TaggedRecord<'ctx>],
    ty: Option<Ty<'ctx>>,
) -> Result<Vec<ConstraintInner<'ctx>>, ConstraintGatheringError> {
    let mut constraints = vec![];

    if let Some(ty) = ty {
        constraints.push(ConstraintInner::IdToTy {
            id: Spanned::new(expr.span(), expr.id.into()),
            ty: Spanned::new(expr.span(), ty),
        });
    }

    match &expr.token {
        TaggedExprInner::Ident(ident) => constraints.push(ConstraintInner::IdToId {
            id: Spanned::new(ident.span(), ident.id.into()),
            to: Spanned::new(expr.span(), expr.id.into()),
        }),
        TaggedExprInner::Literal(lit) => {
            let ty = match lit.token {
                Literal::String(_) => Ty::String,
                Literal::Number(_) => Ty::Int,
                Literal::Bool(_) => Ty::Bool,
            };
            constraints.push(ConstraintInner::IdToTy {
                id: Spanned::new(expr.span(), expr.id.into()),
                ty: Spanned::new(expr.span(), ty),
            });
        }
        TaggedExprInner::BinOp(op, left, right) => match (op.token, left, right) {
            (BinOp::Add | BinOp::Divide | BinOp::Multiply | BinOp::Subtract, left, right) => {
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(left.span(), left.id.into()),
                    to: Spanned::new(right.span(), right.id.into()),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(left.span(), left.id.into()),
                    to: Spanned::new(expr.span(), expr.id.into()),
                });
                constraints.extend(collect_expr(left, definitions, record_definitions, None)?);
                constraints.extend(collect_expr(right, definitions, record_definitions, None)?);
            }
            (BinOp::Dot, _left, right) => match right.token {
                TaggedExprInner::Ident(ref ident) => {
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(ident.span(), ident.id.into()),
                        to: Spanned::new(right.span(), right.id.into()),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(ident.span(), ident.id.into()),
                        to: Spanned::new(expr.span(), expr.id.into()),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(right.span(), right.id.into()),
                        to: Spanned::new(expr.span(), expr.id.into()),
                    });
                }
                // todo: methods
                _ => todo!(),
            },
            // todo: add necessary additional type constraints
            (BinOp::IsEqual, left, right) => {
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(left.span(), left.id.into()),
                    to: Spanned::new(right.span(), right.id.into()),
                });
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(expr.span(), expr.id.into()),
                    ty: Spanned::new(expr.span(), Ty::Bool),
                });
                constraints.extend(collect_expr(left, definitions, record_definitions, None)?);
                constraints.extend(collect_expr(right, definitions, record_definitions, None)?);
            }
            (BinOp::SetEquals, left, right) => match left.token {
                TaggedExprInner::Ident(ref ident) => {
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(ident.span(), ident.id.into()),
                        to: Spanned::new(right.span(), right.id.into()),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(left.span(), left.id.into()),
                        to: Spanned::new(right.span(), right.id.into()),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(ident.span(), ident.id.into()),
                        to: Spanned::new(left.span(), left.id.into()),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(expr.span(), expr.id.into()),
                        to: Spanned::new(left.span(), left.id.into()),
                    });
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(expr.span(), expr.id.into()),
                        to: Spanned::new(right.span(), right.id.into()),
                    });
                    constraints.extend(collect_expr(left, definitions, record_definitions, None)?);
                    constraints.extend(collect_expr(right, definitions, record_definitions, None)?);
                }
                TaggedExprInner::UnOp(op, ref pointer) if op.token.is_deref() => {
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(expr.span(), expr.id.into()),
                        // todo: use the span of the operator
                        to: Spanned::new(pointer.span(), pointer.id.into()),
                    });
                    constraints.push(ConstraintInner::IdToTy {
                        id: Spanned::new(pointer.span(), pointer.id.into()),
                        // todo: use the span of the operator
                        ty: Spanned::new(pointer.span(), Ty::Pointer),
                    });
                    constraints.extend(collect_expr(
                        pointer,
                        definitions,
                        record_definitions,
                        None,
                    )?)
                }
                _ => {
                    return Err(ConstraintGatheringError::CannotAssignToExpression {
                        span: expr.token.span().into(),
                        explanation:
                            "Values can only be assigned to variables, not to expressions!"
                                .to_string(),
                    });
                }
            },
            // todo: sort this out (memory safety)
            (BinOp::Index, left, right) => {
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(expr.span(), expr.id.into()),
                    ty: Spanned::new(expr.span(), Ty::Pointer),
                });
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(left.span(), left.id.into()),
                    // todo: use the span of the operator
                    ty: Spanned::new(left.span(), Ty::Pointer),
                });
                constraints.extend(collect_expr(left, definitions, record_definitions, None)?);
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(right.span(), right.id.into()),
                    ty: Spanned::new(right.span(), Ty::Int),
                });
                constraints.extend(collect_expr(right, definitions, record_definitions, None)?);
            }
        },
        TaggedExprInner::UnOp(op, arg) => match op.token {
            UnOp::Positive | UnOp::Negative => constraints.push(ConstraintInner::IdToTy {
                id: Spanned::new(arg.span(), arg.id.into()),
                ty: Spanned::new(arg.span(), Ty::Int),
            }),
            UnOp::Deref => constraints.push(ConstraintInner::IdToTy {
                id: Spanned::new(arg.span(), arg.id.into()),
                ty: Spanned::new(arg.span(), Ty::Pointer),
            }),
        },
        TaggedExprInner::Constructor(rec) => {
            let definition = record_definitions
                .iter()
                .find(|candidate| rec.name.token == candidate.name.token)
                .unwrap();

            constraints.push(ConstraintInner::IdToTy {
                id: Spanned::new(expr.span(), expr.id.into()),
                ty: Spanned::new(
                    definition.span(),
                    Ty::Record(
                        definition
                            .fields
                            .iter()
                            .map(|field| (field.name.token.inner(), field.ty.token.clone()))
                            .collect(),
                    ),
                ),
            });

            // todo: proper checks for matching fields
            for (struct_field, (constructor_name, constructor_expr)) in
                definition.fields.iter().zip(rec.fields.iter())
            {
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(struct_field.name.span(), struct_field.name.id.into()),
                    to: Spanned::new(constructor_name.span(), constructor_name.id.into()),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(struct_field.name.span(), struct_field.name.id.into()),
                    to: Spanned::new(constructor_expr.span(), constructor_expr.id.into()),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(constructor_name.span(), constructor_name.id.into()),
                    to: Spanned::new(constructor_expr.span(), constructor_expr.id.into()),
                });
            }
        }
        TaggedExprInner::FunctionCall(func, params) => {
            if *func.token == "print_int" {
                if params.len() != 1 {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: func.span().into(),
                        explanation: format!(
                            "This function accepts 1
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }
                let param = &params[0];
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(param.span(), param.id.into()),
                    ty: Spanned::new(param.span(), Ty::Int),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(expr.span(), expr.id.into()),
                    to: Spanned::new(param.span(), param.id.into()),
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
                        span: func.span().into(),
                        explanation: format!(
                            "This function accepts 1
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }
                let param = &params[0];
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(param.span(), param.id.into()),
                    ty: Spanned::new(param.span(), Ty::String),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(expr.span(), expr.id.into()),
                    to: Spanned::new(param.span(), param.id.into()),
                });
                constraints.extend(collect_expr(
                    param,
                    definitions,
                    record_definitions,
                    Some(Ty::String),
                )?);
            // todo: make memory allocation functions standard-library only!
            } else if *func.token == "malloc" {
                if params.len() != 1 {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: func.span().into(),
                        explanation: format!(
                            "This function accepts 1
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }
                let param = &params[0];
                constraints.extend(collect_expr(param, definitions, record_definitions, None)?);
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(param.span(), param.id.into()),
                    ty: Spanned::new(param.span(), Ty::Int),
                });
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(func.span(), func.id.into()),
                    ty: Spanned::new(func.span(), Ty::Pointer),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(func.span(), func.id.into()),
                    to: Spanned::new(expr.span(), expr.id.into()),
                });
            } else if *func.token == "free" {
                if params.len() != 1 {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: func.span().into(),
                        explanation: format!(
                            "This function accepts 1
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }
                let param = &params[0];
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(param.span(), param.id.into()),
                    ty: Spanned::new(param.span(), Ty::Pointer),
                });
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(expr.span(), expr.id.into()),
                    to: Spanned::new(param.span(), param.id.into()),
                });
                constraints.extend(collect_expr(param, definitions, record_definitions, None)?);
            } else if *func.token == "realloc" {
                if params.len() != 2 {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: func.span().into(),
                        explanation: format!(
                            "This function accepts 2
                            parameter, but you've called it with `{}` arguments.",
                            params.len()
                        ),
                    });
                }
                let pointer = &params[0];
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(pointer.span(), pointer.id.into()),
                    ty: Spanned::new(pointer.span(), Ty::Pointer),
                });
                constraints.extend(collect_expr(
                    pointer,
                    definitions,
                    record_definitions,
                    Some(Ty::Pointer),
                )?);
                let new_size = &params[1];
                constraints.push(ConstraintInner::IdToTy {
                    id: Spanned::new(new_size.span(), new_size.id.into()),
                    ty: Spanned::new(new_size.span(), Ty::Int),
                });
                constraints.extend(collect_expr(
                    new_size,
                    definitions,
                    record_definitions,
                    Some(Ty::Int),
                )?);
            } else if let Some(function) = definitions
                .iter()
                .find(|function| function.name.token == func.token)
            {
                if function.parameters.len() != params.len() {
                    return Err(ConstraintGatheringError::MismatchedFunctionCall {
                        span: func.span().into(),
                        explanation: format!(
                            "This function accepts `{}`
                            parameters, but you've called it with `{}` arguments.",
                            function.parameters.len(),
                            params.len()
                        ),
                    });
                }
                for (a, b) in function.parameters.iter().zip(params) {
                    constraints.push(ConstraintInner::IdToId {
                        id: Spanned::new(b.span(), b.id.into()),
                        to: Spanned::new(a.span(), a.id.into()),
                    });
                    constraints.extend(collect_expr(b, definitions, record_definitions, None)?);
                }
                constraints.push(ConstraintInner::IdToId {
                    id: Spanned::new(func.span(), func.id.into()),
                    to: Spanned::new(expr.span(), expr.id.into()),
                });
            } else {
                return Err(ConstraintGatheringError::UnresolvableFunction {
                    span: func.span().into(),
                    explanation: { format!("A function with name `{}` cannot be found.", **func) },
                });
            }
        }
    }

    Ok(constraints)
}
