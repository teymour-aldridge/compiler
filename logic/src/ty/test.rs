use std::{collections::HashSet, iter::FromIterator};

use crate::{
    id::{tag, Id},
    ty::{constraints::Constraint, type_check, unify, Ty, TyEnv},
};

#[test]
fn simple_type_check() {
    let tree = crate::parse::parse(include_str!("examples/simple")).unwrap();
    let tagged = tag(tree);
    let ty_env = type_check(&tagged).expect("failed to type check");

    let x = match tagged.nodes.get(1).unwrap().as_expr().unwrap().token {
        crate::id::TaggedExprInner::BinOp(_, ref left, _) => match left.token {
            crate::id::TaggedExprInner::Ident(ref ident) => ident.id,
            _ => panic!(),
        },
        _ => panic!(),
    };

    let y = match tagged.nodes.get(2).unwrap().as_expr().unwrap().token {
        crate::id::TaggedExprInner::BinOp(_, ref left, _) => match left.token {
            crate::id::TaggedExprInner::Ident(ref ident) => ident.id,
            _ => panic!(),
        },
        _ => panic!(),
    };
    assert_eq!(ty_env.ty_of(x), Some(Ty::Int));
    assert_eq!(ty_env.ty_of(y), Some(Ty::Int))
}

#[test]
fn simple_unify_check() {
    let set = HashSet::from_iter(vec![
        Constraint::IdToTy {
            id: Id::new(1),
            ty: Ty::Int,
        },
        Constraint::IdToId {
            id: Id::new(2),
            to: Id::new(1),
        },
        Constraint::IdToId {
            id: Id::new(3),
            to: Id::new(1),
        },
        Constraint::IdToTy {
            id: Id::new(3),
            ty: Ty::Int,
        },
    ]);

    let env = unify(set, TyEnv::new()).unwrap();

    assert_eq!(env.ty_of(Id::new(1)).unwrap(), Ty::Int);
    assert_eq!(env.ty_of(Id::new(2)).unwrap(), Ty::Int);
    assert_eq!(env.ty_of(Id::new(3)).unwrap(), Ty::Int);
}
