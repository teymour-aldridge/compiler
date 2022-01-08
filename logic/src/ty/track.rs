//! Tracks constraints for error generation.
//!
//! note: scribbles on error reporting below
//! -> first, we try to solve the set
//!     -> if we can solve the set, then no error reporting is necessary (yay!) so we finish
//! -> once we have an error, we then attempt to create a diagnosis
//! -> the only situation in which this function fails is if we try to unify two types that are
//!    incompatible
//! -> we want to see where the original source is, for example in this program
//! ```ignore
//! function f(x)
//!   if True then
//!     return "yes"
//!   else
//!     return False
//!   endif
//! endfunction
//! ```
//!
//! We would ideally see an error a bit like:
//! ```ignore
//! Your program contains a type error!
//! if True then
//!     return "yes"
//!     ^^^^^^^^^^^^ here the return type is string
//! Functions may only return one type of value!
//!
//! else
//!    return False
//!    ^^^^^^^^^^^^ here the return type is a boolean
//! endif
//! help: to fix this, you should only return one type of value from a function
//! note: if you want to return more than one type, you may wish to use an enumeration
//! ```
//!
//! note: the compiler does not yet support enumerations.
//!
//! If we consider the above program a bit, we can see roughly what the constraints would look like:
//! ```ignore
//! (1) return_value = string
//! (2) return_value = bool
//! ```
//!
//! We would then take constraint (1) and use it to substitute `string` for `return_value`, leaving
//! us with something a bit like:
//!
//! ```ignore
//! (2) bool = string
//! ```
//!
//! Where the type checker will clearly fail and report the bug!
//!
//! The question, now is how best to generate a diagnostic.
//!
//! We are going for the naïve approach; we give every constraint a unique id (a machine-word sized
//! integer) and track changes to the constraints. We can then use this to backpropagate and produce
//! useful information about errors. I was vaguely inspired by [rr](https://rr-project.org) when
//! designing this.
//!
//! We have to attach span information to all the constraints.

use rustc_hash::FxHashMap;

use crate::{diagnostics::span::Span, id::UniversalId};

use super::{
    constraints::{Constraint, ConstraintInner},
    Ty,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ConstraintId {
    inner: usize,
}

/// OCR would be proud (they love having students draw out "trace tables" - writing down the values
/// of variables at different points of execution).
#[derive(Debug, Clone, Default)]
pub(crate) struct TraceTable<'ctx> {
    records: FxHashMap<ConstraintId, TraceData<'ctx>>,
    spans: FxHashMap<UniversalId<'ctx>, Span>,
}

impl<'ctx> TraceTable<'ctx> {
    pub fn new() -> TraceTable<'ctx> {
        Default::default()
    }

    pub fn add_span(&mut self, id: impl Into<UniversalId<'ctx>>, span: Span) {
        self.spans.insert(id.into(), span);
    }

    /// Records a unification operation.
    pub fn log_operation(&mut self, con: &Constraint, operation: UnificationOperation<'ctx>) {
        let data = self
            .records
            .get_mut(&con.id)
            .expect("internal error: constraint not inserted before use!");
        data.operations.push(operation);
    }
}

#[derive(Debug, Clone, Default)]
pub struct TraceData<'ctx> {
    operations: Vec<UnificationOperation<'ctx>>,
}

#[derive(Debug, Clone)]
pub enum UnificationOperation<'ctx> {
    /// the given id was replaced with a type
    Concretise {
        preexisting: UniversalId<'ctx>,
        with: Ty<'ctx>,
        pos: ConstraintPosition,
    },
    /// The id `preexisting` was replaced with the id `new`
    Swap {
        preexisting: UniversalId<'ctx>,
        new: UniversalId<'ctx>,
        pos: ConstraintPosition,
    },
}

#[derive(Debug, Copy, Clone)]
pub enum ConstraintPosition {
    One,
    Two,
}

impl<'ctx> UnificationOperation<'ctx> {
    /// Undoes the operation to the constraint in place.
    fn step_back(&self, constraint: ConstraintInner<'ctx>) -> ConstraintInner<'ctx> {
        match (self, constraint) {
            (
                UnificationOperation::Concretise {
                    preexisting,
                    with,
                    pos,
                },
                ConstraintInner::IdToTy { id, ty },
            ) => {
                assert!(matches!(pos, ConstraintPosition::Two));
                assert_eq!(&ty, with);
                ConstraintInner::IdToId {
                    id,
                    to: *preexisting,
                }
            }
            // cannot reverse this process
            (UnificationOperation::Concretise { .. }, ConstraintInner::IdToId { .. }) => {
                unreachable!()
            }
            (
                UnificationOperation::Concretise {
                    preexisting,
                    with,
                    pos,
                },
                ConstraintInner::TyToTy { ty, to },
            ) => match pos {
                ConstraintPosition::One => {
                    assert_eq!(with, &ty);
                    ConstraintInner::IdToTy {
                        id: *preexisting,
                        ty: to,
                    }
                }
                ConstraintPosition::Two => {
                    assert_eq!(with, &to);
                    ConstraintInner::IdToTy {
                        id: *preexisting,
                        ty,
                    }
                }
            },
            (
                UnificationOperation::Swap {
                    preexisting,
                    new,
                    pos,
                },
                ConstraintInner::IdToTy { id, ty },
            ) => {
                assert_eq!(&id, new);
                assert!(matches!(pos, ConstraintPosition::One));
                ConstraintInner::IdToTy {
                    id: *preexisting,
                    ty,
                }
            }
            (
                UnificationOperation::Swap {
                    preexisting,
                    new,
                    pos,
                },
                ConstraintInner::IdToId { id, to },
            ) => match pos {
                ConstraintPosition::One => {
                    assert_eq!(&id, new);
                    ConstraintInner::IdToId {
                        id: *preexisting,
                        to,
                    }
                }
                ConstraintPosition::Two => {
                    assert_eq!(&to, new);
                    ConstraintInner::IdToId {
                        id,
                        to: *preexisting,
                    }
                }
            },
            (UnificationOperation::Swap { .. }, ConstraintInner::TyToTy { .. }) => unreachable!(),
        }
    }
}
