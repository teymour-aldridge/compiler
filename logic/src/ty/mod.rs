//! Type checking.

#[cfg(test)]
mod test;

#[cfg(test)]
#[cfg(not(disable_fuzzcheck))]
mod fuzz;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    id::{Id, TaggedAst},
    ty::constraints::collect,
};

use self::constraints::{Constraint, ConstraintGatheringError};

mod constraints;

pub struct TyTable {
    #[allow(unused)]
    table: FxHashMap<Id, Ty>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Ty {
    Int,
    Bool,
    String,
}

#[derive(Debug)]
pub enum TyCheckError {
    ConstraintGatheringError(ConstraintGatheringError),
    TypeMismatch,
}

impl From<ConstraintGatheringError> for TyCheckError {
    fn from(err: ConstraintGatheringError) -> Self {
        Self::ConstraintGatheringError(err)
    }
}

pub fn type_check(ast: &TaggedAst) -> Result<TyEnv, TyCheckError> {
    // todo: report errors properly
    let constraints: FxHashSet<Constraint> = collect(ast)?.into_iter().collect();
    unify(constraints, TyEnv::new())
}

#[derive(Copy, Clone, Debug, Hash)]
enum Substitution {
    XforY(Id, Id),
    ConcreteForX(Ty, Id),
}

#[derive(Hash, Copy, Clone, Debug)]
pub enum TyInfo {
    EqId(Id),
    EqTy(Ty),
}

#[derive(Hash, Copy, Clone, Debug)]

pub struct Info {
    ty: TyInfo,
}

#[derive(Clone, Debug)]
pub struct TyEnv {
    map: FxHashMap<Id, Info>,
}

impl TyEnv {
    /// Obtain a reference to the underlying hash map.
    #[allow(unused)]
    pub(crate) fn map(&self) -> &FxHashMap<Id, Info> {
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
        self.map.get(&id).and_then(|info| match info.ty {
            TyInfo::EqId(equal_to) => self.ty_of(equal_to),
            TyInfo::EqTy(ty) => Some(ty),
        })
    }

    fn new() -> Self {
        Self {
            map: Default::default(),
        }
    }

    fn feed_substitution(&mut self, u: Substitution) {
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
fn unify(set: FxHashSet<Constraint>, mut solved: TyEnv) -> Result<TyEnv, TyCheckError> {
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
        solved.feed_substitution(u);

        // apply the newly generated substitution to the rest of the set
        let new_set: FxHashSet<Constraint> = iter
            .map(|constraint| match (u, constraint) {
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
