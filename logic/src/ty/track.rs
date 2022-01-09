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

use codespan_reporting::diagnostic::{Diagnostic, Label};
use rustc_hash::FxHashMap;

use crate::{
    diagnostics::span::{HasSpan, Spanned},
    id::UniversalId,
};

use super::{
    constraints::{Constraint, ConstraintInner},
    Ty,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct ConstraintId {
    pub(crate) inner: usize,
}

impl ConstraintId {
    #[allow(unused)]
    pub fn new(inner: usize) -> Self {
        Self { inner }
    }
}

/// OCR would be proud (they love having students draw out "trace tables" - writing down the values
/// of variables at different points of execution).
#[derive(Debug, Clone, Default)]
pub(crate) struct TraceTable<'ctx> {
    records: FxHashMap<ConstraintId, TraceData<'ctx>>,
}

impl<'ctx> TraceTable<'ctx> {
    #[allow(unused)]
    pub fn new() -> TraceTable<'ctx> {
        Default::default()
    }

    /// Records a unification operation.
    pub fn log_operation(&mut self, id: ConstraintId, operation: UnificationOperation<'ctx>) {
        self.records
            .entry(id)
            .and_modify(|data| {
                data.operations.push(operation.clone());
            })
            .or_insert_with(|| TraceData::new(vec![operation]));
    }
}

#[derive(Debug, Clone, Default)]
pub struct TraceData<'ctx> {
    operations: Vec<UnificationOperation<'ctx>>,
}

impl<'ctx> TraceData<'ctx> {
    pub fn new(operations: Vec<UnificationOperation<'ctx>>) -> Self {
        Self { operations }
    }
}

#[derive(Debug, Clone)]
pub enum UnificationOperation<'ctx> {
    /// the given id was replaced with a type
    Concretise {
        preexisting: Spanned<UniversalId<'ctx>>,
        with: Spanned<Ty<'ctx>>,
        pos: ConstraintPosition,
    },
    /// The id `preexisting` was replaced with the id `new`
    Swap {
        preexisting: Spanned<UniversalId<'ctx>>,
        new: Spanned<UniversalId<'ctx>>,
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
                assert!(matches!(pos, ConstraintPosition::One));
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

#[derive(Debug)]
pub struct ErrorReporter<'ctx> {
    trace_table: TraceTable<'ctx>,
    errored_on: Constraint<'ctx>,
}

impl<'ctx> ErrorReporter<'ctx> {
    pub(crate) fn new(trace_table: TraceTable<'ctx>, errored_on: Constraint<'ctx>) -> Self {
        Self {
            trace_table,
            errored_on,
        }
    }

    pub fn report<ID>(mut self, file_id: ID) -> Diagnostic<ID>
    where
        ID: Copy,
    {
        let mut diagnostic =
            Diagnostic::error().with_message("Your program contains a type error!");

        let operations = self
            .trace_table
            .records
            .remove(&self.errored_on.id)
            .expect("internal error");

        let constraint = self.errored_on;
        let mut inner = constraint.inner;

        if let ConstraintInner::TyToTy { ty, to } = &inner {
            diagnostic.labels.push(
                Label::primary(file_id, ty.span().index_only().range())
                    .with_message(format!("this is of type {:?}", ty.token)),
            );
            diagnostic.labels.push(
                Label::primary(file_id, to.span().index_only().range()).with_message(format!(
                    "this is of type `{:?}` \
                    which is not the same as type `{:?}`",
                    to.token, ty.token
                )),
            );
        } else {
            unreachable!();
        }

        for operation in operations.operations.iter().rev() {
            inner = operation.step_back(inner);
            match inner {
                ConstraintInner::IdToTy { ref id, ref ty } => {
                    Label::secondary(file_id, id.span().index_only().range())
                        .with_message("this item needs to be of the same type as...");
                    Label::secondary(file_id, ty.span().index_only().range())
                        .with_message(format!("...the type `{:?}`", ty.token));
                }
                ConstraintInner::IdToId { ref id, ref to } => {
                    Label::secondary(file_id, id.span().index_only().range())
                        .with_message("this item needs to be of the same type as");
                    Label::secondary(file_id, to.span().index_only().range())
                        .with_message("this item");
                }
                ConstraintInner::TyToTy { ref ty, ref to } => {
                    Label::secondary(file_id, ty.span().index_only().range())
                        .with_message("this type needs to be of the same type as");
                    Label::secondary(file_id, to.span().index_only().range()).with_message(
                        format!(
                            "this type, however `{:?}` is not the same as `{:?}`",
                            ty.token, to.token
                        ),
                    );
                }
            }
        }

        diagnostic
    }
}
