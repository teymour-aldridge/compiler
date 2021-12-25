//! Type checking.

use std::{collections::HashMap, fmt, hash::Hash};

use rustc_hash::{FxHashMap, FxHashSet};

pub mod error;
#[cfg(test)]
#[cfg(not(disable_fuzzcheck))]
mod fuzz;
#[cfg(test)]
mod fuzz2;
#[cfg(test)]
mod test;

use crate::{
    id::{TaggedAst, UniversalId},
    ty::constraints::collect,
};

use self::{constraints::Constraint, error::TyCheckError};

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
    Record(HashMap<&'ctx str, Ty<'ctx>>),
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
        })
    }
}

pub fn type_check<'ctx>(ast: &'ctx TaggedAst<'ctx>) -> Result<TyEnv<'ctx>, TyCheckError> {
    let constraints: FxHashSet<Constraint> = collect(ast)?.into_iter().collect();
    unify(constraints, TyEnv::new())
}

#[derive(Clone, Debug, Hash)]
/// A substitution to be made as part of type inference.
enum Substitution<'ctx> {
    /// Substitute the first id `X`, for the second id `Y`.
    XforY(UniversalId<'ctx>, UniversalId<'ctx>),
    /// Substitute a concrete type for the given id.
    ConcreteForX(Ty<'ctx>, UniversalId<'ctx>),
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

    fn feed_substitution(&mut self, u: Substitution<'ctx>) {
        match u {
            // we remove y from the system by equating it to x
            Substitution::XforY(x, y) => self.map.insert(
                y,
                Info {
                    ty: TyInfo::EqId(x),
                },
            ),
            // we remove x from the system by equating it to ty
            Substitution::ConcreteForX(ty, x) => self.map.insert(
                x,
                Info {
                    ty: TyInfo::EqTy(ty),
                },
            ),
        };
    }
}

/// Unify a set of constraints (i.e. solve them, if that is possible)
fn unify<'ctx>(
    set: FxHashSet<Constraint<'ctx>>,
    mut solved: TyEnv<'ctx>,
) -> Result<TyEnv<'ctx>, TyCheckError> {
    if set.is_empty() {
        return Ok(solved);
    }

    let mut iter = set.into_iter();

    let next = if let Some(next) = iter.next() {
        next
    } else {
        return Ok(solved);
    };

    let u = match next {
        Constraint::IdToTy { id, ty } => Some(Substitution::ConcreteForX(ty, id)),
        Constraint::IdToId { id, to } => {
            if id != to {
                Some(Substitution::XforY(to, id))
            } else {
                None
            }
        }
        Constraint::TyToTy { ty, to } => {
            if ty != to {
                return Err(TyCheckError::TypeMismatch);
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
            .map(|constraint| match (u.clone(), constraint) {
                // wherever we see y, replace with x
                (Substitution::XforY(x, y), Constraint::IdToTy { id, ty }) => Constraint::IdToTy {
                    id: if id == y { x } else { id },
                    ty,
                },
                // wherever we see y, replace with x
                (Substitution::XforY(x, y), Constraint::IdToId { id, to }) => Constraint::IdToId {
                    id: if id == y { x } else { id },
                    to: if to == y { x } else { to },
                },
                (Substitution::ConcreteForX(sub_with, x), Constraint::IdToTy { id, ty }) => {
                    if id == x {
                        Constraint::TyToTy {
                            ty: sub_with,
                            to: ty,
                        }
                    } else {
                        Constraint::IdToTy { id, ty }
                    }
                }
                (Substitution::ConcreteForX(sub_with, x), Constraint::IdToId { id, to }) => {
                    if id == x {
                        Constraint::IdToTy {
                            id: to,
                            ty: sub_with,
                        }
                    } else if to == x {
                        Constraint::IdToTy { id, ty: sub_with }
                    } else {
                        Constraint::IdToId { id, to }
                    }
                }
                // these substitutions cannot be applied to the constraint set
                (Substitution::XforY(_, _), constraint)
                | (Substitution::ConcreteForX(_, _), constraint) => constraint,
            })
            .collect();

        unify(new_set, solved)
    } else {
        unify(iter.collect(), solved)
    }
}
