//! Type checking.

use std::{collections::BTreeMap, hash::Hash};

use rustc_hash::FxHashSet;

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
    parse::{
        record::RecordRef,
        table::{Id, ParseTable},
    },
    ty::constraints::collect,
};

use self::{
    constraints::{Constraint, ConstraintInner},
    error::TyCheckError,
    track::{ConstraintPosition, ErrorReporter, TraceTable, UnificationOperation},
};

mod constraints;

/// A primitive type. All other types are built out of these (using the
/// language constructs we have for building composite types - currently just
/// records, but in future we may introduce abstract data types).
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub enum PrimitiveType {
    /// A integer. These are 64-bit (and signed) by default.
    Int,
    /// A boolean. We follow many other programming languages by using a full
    /// byte (rather than a single bit) to store them..
    Bool,
    /// Much like Rust's `&str`, although we don't expose this to users (we are
    /// trying to provide a _much_ higher-level interface than Rust). This is
    /// used to give a type to user string literals. Note: we will define a
    /// `String` type in the standard library.
    StrSlice,
    /// A pointer. This is an integer which is one word long and can be used as
    /// part of `load` and `store` operations. Note that this is only available
    /// for the standard library (to prevent unsafety, all other programs are
    /// prohibited from using it directly). Once we introduce garbage collection
    /// it will become possible to do so, however, we will never use the word
    /// "pointer" in an external interface (i.e. language user-facing - i.e. no
    /// mentions in the docs, etc).
    Pointer,
}

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub enum Ty {
    /// Contains a reference to the [`crate::parse::record::Record`], from which
    /// the fields can be found (and then the types, from the [`TyEnv`])
    Record { ref_: RecordRef },
    /// A primitive type (see item documentation for more information).
    PrimitiveType(PrimitiveType),
}

pub fn type_check<'i>(ast: &'i ParseTable<'i>) -> Result<TyEnv, TyCheckError> {
    let constraints: FxHashSet<Constraint> = collect(ast)?.into_iter().collect();

    let mut trace_table = TraceTable::default();

    unify(constraints, TyEnv::new(), &mut trace_table)
        .map_err(|errored_on| TyCheckError::Reportable(ErrorReporter::new(trace_table, errored_on)))
}

#[derive(Clone, Debug, Hash)]
/// A substitution to be made as part of type inference.
enum SubstitutionInner {
    /// Substitute the first id `X`, for the second id `Y`.
    XforY(Spanned<Id>, Spanned<Id>),
    /// Substitute a concrete type for the given id.
    ConcreteForX(Spanned<Ty>, Spanned<Id>),
}

#[derive(Hash, Clone, Debug)]
/// The value of this type (which may have been inferred).
pub enum TyInfo {
    EqId(Id),
    EqTy(Ty),
}

#[derive(Hash, Clone, Debug)]
/// Information corresponding to an [crate::id::Id].
pub struct Info {
    ty: TyInfo,
}

#[derive(Clone, Debug)]
pub struct TyEnv {
    map: BTreeMap<Id, Info>,
}

impl TyEnv {
    /// Pretty-prints the inferred type information. Useful primarily for debugging.
    pub fn pretty_print(&self, table: &ParseTable) {
        println!(
            "{0: <15} | {1: <15} | {2: <15}",
            "variable name", "variable id", "inferred type"
        );
        for _ in 0..15 * 3 {
            print!("-")
        }
        println!();
        for (id, ident) in table.ident.iter() {
            let ty = self.ty_of(*id);
            println!(
                "{0: <15} | {1: <15} | {2: <15?}",
                ident.inner(),
                id.as_u32(),
                ty
            )
        }
    }

    /// Obtain a reference to the underlying hash map.
    #[allow(unused)]
    pub(crate) fn map(&self) -> &BTreeMap<Id, Info> {
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
    pub(crate) fn ty_of(&self, id: Id) -> Option<Ty> {
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

    fn feed_substitution(&mut self, u: SubstitutionInner) {
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
                    ty: TyInfo::EqTy(*ty),
                },
            ),
        };
    }
}

/// Unify a set of constraints (i.e. solve them, if that is possible).
///
/// note: for details on error reporting, please see [track].
fn unify(
    set: FxHashSet<Constraint>,
    mut solved: TyEnv,
    trace_table: &mut TraceTable,
) -> Result<TyEnv, Constraint> {
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
                                with: sub_with,
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
                                with: sub_with,
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
                                with: sub_with,
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
                | (SubstitutionInner::ConcreteForX(_, _), _) => constraint,
            })
            .collect();

        unify(new_set, solved, trace_table)
    } else {
        unify(iter.collect(), solved, trace_table)
    }
}
