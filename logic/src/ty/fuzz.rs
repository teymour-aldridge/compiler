/// This test tries to ascertain that the order of constraints emitting into the unifier has no
/// bearing on its output.
///
/// To do this, it generates large numbers of combinations of constraints,
/// and instructions on how to later swap the order of the constraints. If the unifier can solve
/// the set, it then shuffles the set and asserts that the result is the same with the sets sent
/// in a diffent order.
///
/// At some point I also intend to try to implement some of the ideas outlined in the paper
/// https://sites.cs.ucsb.edu/~benh/research/papers/dewey15fuzzing.pdf (which seems to be very
/// neat).
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, iter::FromIterator};

use fuzzcheck::{DefaultMutator, SerdeSerializer};

use crate::id::Id;

use super::{constraints::Constraint, unify, Ty, TyEnv};

#[derive(Clone, DefaultMutator, Deserialize, Serialize)]
enum MutatorTy {
    Int,
    Bool,
}

#[derive(Clone, DefaultMutator, Deserialize, Serialize)]
#[allow(non_snake_case)]
enum ConstraintMutator {
    IdToId { x: usize, y: usize },
    IdToTy { id: usize, ty: MutatorTy },
}

#[derive(Clone, DefaultMutator, Deserialize, Serialize)]
struct ConstraintSet {
    constraints: Vec<ConstraintMutator>,
    shuffles: Vec<Shuffle>,
}

#[derive(Clone, DefaultMutator, Deserialize, Serialize)]
struct Shuffle {
    a: u8,
    b: u8,
}

#[test]
/// Tests to make sure that the unification algorithm generates the same result, regardless of the
/// order of the constraints.
fn fuzz_unifier() {
    let _ = fuzzcheck::fuzz_test(|constraint_set: &ConstraintSet| -> bool {
        let ConstraintSet {
            shuffles,
            constraints,
        } = constraint_set;

        let constraints = constraints
            .iter()
            .map(|constraint| match constraint {
                ConstraintMutator::IdToId { x, y } => Constraint::IdToId {
                    id: Id::new(*x),
                    to: Id::new(*y),
                },
                ConstraintMutator::IdToTy { id, ty } => Constraint::IdToTy {
                    id: Id::new(*id),
                    ty: match ty {
                        MutatorTy::Int => Ty::Int,
                        MutatorTy::Bool => Ty::Bool,
                    },
                },
            })
            .collect::<Vec<_>>();

        let hash_set = HashSet::from_iter(constraints.clone());
        let solved = unify(hash_set, TyEnv::new());
        match solved {
            Ok(prev) => {
                let mut new_constraints: Vec<Constraint> = constraints.clone();
                let len = constraints.len();
                // we can't shuffle a list with one item, so return true
                if len <= 1 {
                    return true;
                }
                for shuffle in shuffles {
                    let Shuffle { a, b } = shuffle;
                    // if the indices are too big, return true
                    if *a as usize > len - 1 || *b as usize > len - 1 {
                        return true;
                    }
                    new_constraints.swap(*a as usize, *b as usize);
                }
                let new = match unify(HashSet::from_iter(new_constraints), TyEnv::new()) {
                    Ok(env) => env,
                    Err(_) => return false,
                };

                for id in new.map().keys() {
                    if new.ty_of(*id) != prev.ty_of(*id) {
                        return false;
                    }
                }

                return true;
            }
            Err(_) => return true,
        }
    })
    .mutator(ConstraintSet::default_mutator())
    .serializer(SerdeSerializer::default())
    .default_sensor()
    .default_pool()
    .arguments_from_cargo_fuzzcheck()
    .launch();
}
