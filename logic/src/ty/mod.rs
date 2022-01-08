//! Type checking.

use std::{fmt, hash::Hash};

use rustc_hash::{FxHashMap, FxHashSet};

#[cfg(all(test, feature = "fuzzcheck"))]
mod fuzz;
#[cfg(test)]
mod fuzz2;
#[cfg(test)]
mod test;
#[cfg(test)]
mod ui;

pub mod error;
mod track;

use crate::{
    diagnostics::span::Spanned,
    id::{TaggedAst, UniversalId},
    ty::constraints::collect,
};

use self::{
    constraints::{Constraint, ConstraintInner},
    error::TyCheckError,
    track::{ConstraintPosition, ErrorReporter, TraceTable, UnificationOperation},
};

mod constraints;

/// An atomic type - all other types are expressed in terms of these. Types are an important part
/// of the compilation process, and are used by later stages in the compiler to reason about how to
/// translate code.
#[derive(Clone, Eq, Debug)]
// when using fuzzcheck it is necessary to implement some additional traits
#[cfg_attr(test, derive(serde::Serialize))]
pub enum Ty<'ctx> {
    Int,
    Bool,
    String,
    Record(FxHashMap<&'ctx str, Ty<'ctx>>),
    /// note: this will only be useable from the standard library
    #[allow(unused)]
    Pointer,
}

/// todo: test these better to make them line up properly
impl Hash for Ty<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        if let Ty::Record(rec) = self {
            for (key, value) in rec {
                key.hash(state);
                value.hash(state);
            }
        }
    }
}

impl PartialEq for Ty<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Record(l0), Self::Record(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl fmt::Display for Ty<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Ty::Int => "Int",
            Ty::Bool => "Bool",
            Ty::String => "String",
            Ty::Record(_) => {
                unimplemented!()
            }
            Ty::Pointer => "Pointer",
        })
    }
}

pub fn type_check<'ctx>(ast: &'ctx TaggedAst<'ctx>) -> Result<TyEnv<'ctx>, TyCheckError> {
    let constraints: FxHashSet<Constraint> = collect(ast)?.into_iter().collect();

    let mut trace_table = TraceTable::default();

    unify(constraints, TyEnv::new(), &mut trace_table)
        .map_err(|errored_on| TyCheckError::Reportable(ErrorReporter::new(trace_table, errored_on)))
}

#[derive(Clone, Debug, Hash)]
/// A substitution to be made as part of type inference.
enum SubstitutionInner<'ctx> {
    /// Substitute the first id `X`, for the second id `Y`.
    XforY(Spanned<UniversalId<'ctx>>, Spanned<UniversalId<'ctx>>),
    /// Substitute a concrete type for the given id.
    ConcreteForX(Spanned<Ty<'ctx>>, Spanned<UniversalId<'ctx>>),
}

#[derive(Hash, Clone, Debug)]
/// The value of this type (which may have been inferred).
pub enum TyInfo<'ctx> {
    EqId(UniversalId<'ctx>),
    EqTy(Ty<'ctx>),
}

#[derive(Hash, Clone, Debug)]
/// Information corresponding to an [crate::id::Id].
pub struct Info<'ctx> {
    ty: TyInfo<'ctx>,
}

#[derive(Clone, Debug)]
pub struct TyEnv<'ctx> {
    map: FxHashMap<UniversalId<'ctx>, Info<'ctx>>,
}

impl<'ctx> TyEnv<'ctx> {
    /// Obtain a reference to the underlying hash map.
    #[allow(unused)]
    pub(crate) fn map(&'ctx self) -> &'ctx FxHashMap<UniversalId<'ctx>, Info<'ctx>> {
        &self.map
    }

    #[allow(unused)]
    fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    #[allow(unused)]
    fn len(&self) -> usize {
        self.map.len()
    }

    /// Obtain the type of the item in question (if it exists).
    #[allow(unused)]
    pub(crate) fn ty_of(&self, id: UniversalId) -> Option<Ty> {
        self.map.get(&id).and_then(|info| match info.ty.clone() {
            TyInfo::EqId(equal_to) => self.ty_of(equal_to),
            TyInfo::EqTy(ty) => Some(ty),
        })
    }

    fn new() -> Self {
        Self {
            map: Default::default(),
        }
    }

    fn feed_substitution(&mut self, u: SubstitutionInner<'ctx>) {
        match u {
            // we remove y from the system by equating it to x
            SubstitutionInner::XforY(x, y) => self.map.insert(
                *y,
                Info {
                    ty: TyInfo::EqId(*x),
                },
            ),
            // we remove x from the system by equating it to ty
            SubstitutionInner::ConcreteForX(ty, x) => self.map.insert(
                *x,
                Info {
                    ty: TyInfo::EqTy((*ty).clone()),
                },
            ),
        };
    }
}

/// Unify a set of constraints (i.e. solve them, if that is possible).
///
/// note: for details on error reporting, please see [track].
fn unify<'ctx>(
    set: FxHashSet<Constraint<'ctx>>,
    mut solved: TyEnv<'ctx>,
    trace_table: &mut TraceTable<'ctx>,
) -> Result<TyEnv<'ctx>, Constraint<'ctx>> {
    if set.is_empty() {
        return Ok(solved);
    }

    let mut iter = set.into_iter();

    let next = if let Some(next) = iter.next() {
        next
    } else {
        return Ok(solved);
    };

    let u = match next.inner {
        ConstraintInner::IdToTy { id, ty } => Some(SubstitutionInner::ConcreteForX(ty, id)),
        ConstraintInner::IdToId { id, to } => {
            if id != to {
                Some(SubstitutionInner::XforY(to, id))
            } else {
                None
            }
        }
        ConstraintInner::TyToTy { ref ty, ref to } => {
            // here we need to work backwards, and work out where this constraint came from!
            // maybe we need a way of "undoing" the algorithm?
            // –> each constraint should have a "trace" which lists how we got here
            // -> if there is an error, we can use this to produce useful diagnostic information
            // we may need to produce quite complex systems
            if ty != to {
                return Err(next.clone());
            } else {
                None
            }
        }
    };

    if let Some(u) = u {
        // add the substitution to the list of substitutions
        solved.feed_substitution(u.clone());

        // apply the newly generated substitution to the rest of the set
        let new_set: FxHashSet<Constraint> = iter
            .map(|constraint| match (u.clone(), constraint.inner.clone()) {
                // wherever we see y, replace with x
                (SubstitutionInner::XforY(x, y), ConstraintInner::IdToTy { id, ty }) => {
                    Constraint::new(
                        constraint.id,
                        ConstraintInner::IdToTy {
                            id: if id == y {
                                trace_table.log_operation(
                                    constraint.id,
                                    UnificationOperation::Swap {
                                        preexisting: y,
                                        new: x,
                                        pos: ConstraintPosition::One,
                                    },
                                );
                                x
                            } else {
                                id
                            },
                            ty,
                        },
                    )
                }
                // wherever we see y, replace with x
                (SubstitutionInner::XforY(x, y), ConstraintInner::IdToId { id, to }) => {
                    Constraint::new(
                        constraint.id,
                        ConstraintInner::IdToId {
                            id: if id == y {
                                trace_table.log_operation(
                                    constraint.id,
                                    UnificationOperation::Swap {
                                        preexisting: y,
                                        new: x,
                                        pos: ConstraintPosition::One,
                                    },
                                );
                                x
                            } else {
                                id
                            },
                            to: if to == y {
                                trace_table.log_operation(
                                    constraint.id,
                                    UnificationOperation::Swap {
                                        preexisting: y,
                                        new: x,
                                        pos: ConstraintPosition::Two,
                                    },
                                );
                                x
                            } else {
                                to
                            },
                        },
                    )
                }
                (
                    SubstitutionInner::ConcreteForX(sub_with, x),
                    ConstraintInner::IdToTy { id, ty },
                ) => {
                    if id == x {
                        trace_table.log_operation(
                            constraint.id,
                            UnificationOperation::Concretise {
                                preexisting: id,
                                with: sub_with.clone(),
                                pos: ConstraintPosition::One,
                            },
                        );
                        Constraint::new(
                            constraint.id,
                            ConstraintInner::TyToTy {
                                ty: sub_with,
                                to: ty,
                            },
                        )
                    } else {
                        Constraint::new(constraint.id, ConstraintInner::IdToTy { id, ty })
                    }
                }
                (
                    SubstitutionInner::ConcreteForX(sub_with, x),
                    ConstraintInner::IdToId { id, to },
                ) => {
                    if id == x {
                        trace_table.log_operation(
                            constraint.id,
                            UnificationOperation::Concretise {
                                preexisting: id,
                                with: sub_with.clone(),
                                pos: ConstraintPosition::One,
                            },
                        );
                        Constraint::new(
                            constraint.id,
                            ConstraintInner::IdToTy {
                                id: to,
                                ty: sub_with,
                            },
                        )
                    } else if to == x {
                        trace_table.log_operation(
                            constraint.id,
                            UnificationOperation::Concretise {
                                preexisting: to,
                                with: sub_with.clone(),
                                pos: ConstraintPosition::One,
                            },
                        );
                        Constraint::new(constraint.id, ConstraintInner::IdToTy { id, ty: sub_with })
                    } else {
                        Constraint::new(constraint.id, ConstraintInner::IdToId { id, to })
                    }
                }
                // these substitutions cannot be applied to the constraint set
                (SubstitutionInner::XforY(_, _), _)
                | (SubstitutionInner::ConcreteForX(_, _), _) => constraint.clone(),
            })
            .collect();

        unify(new_set, solved, trace_table)
    } else {
        unify(iter.collect(), solved, trace_table)
    }
}
