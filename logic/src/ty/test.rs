use std::iter::FromIterator;

use rustc_hash::FxHashSet;

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
fn factorial_type_check() {
    let tree = crate::parse::parse(include_str!("examples/factorial")).unwrap();
    let tagged = tag(tree);

    let ty_env = type_check(&tagged).expect("failed to type check");

    let main_function = match &tagged.nodes[0] {
        crate::parse::Node::Func(ref func) => func.name.id,
        _ => panic!("failed to find type inferred for `main` function"),
    };

    let (factorial_function_ret, factorial_func) = match &tagged.nodes[1] {
        crate::parse::Node::Func(ref func) => (func.name.id, func),
        _ => panic!("failed to find type inferred for `factorial` function"),
    };

    assert_eq!(ty_env.ty_of(main_function), Some(Ty::Int));
    assert_eq!(ty_env.ty_of(factorial_function_ret), Some(Ty::Int));

    let if_branch = &factorial_func.block.inner.nodes[0].as_if().unwrap().r#if;

    assert_eq!(ty_env.ty_of(if_branch.condition.id), Some(Ty::Bool));

    match if_branch.condition.token {
        crate::id::TaggedExprInner::BinOp(_, ref left, _) => {
            assert_eq!(ty_env.ty_of(left.id), Some(Ty::Int))
        }
        _ => panic!("ast does not match expected structure for expr `n == 0`"),
    }
}

#[test]
fn test_record_type_check() {
    let tree = crate::parse::parse(include_str!("examples/record")).unwrap();
    let tagged = tag(tree);

    let ty_env = type_check(&tagged).unwrap();

    let main = tagged.nodes.get(0).unwrap();
    let (_, left, right) = main
        .as_func()
        .unwrap()
        .block
        .inner
        .nodes
        .get(0)
        .unwrap()
        .as_expr()
        .unwrap()
        .as_bin_op()
        .unwrap();
    assert_eq!(ty_env.ty_of(left.id), Some(Ty::Int));
    assert_eq!(ty_env.ty_of(right.id), Some(Ty::Int));

    let record = tagged.nodes.get(1).unwrap().as_record().unwrap();
    let record_id = record.fields.get(0).unwrap().name.id;
    assert_eq!(ty_env.ty_of(record_id), Some(Ty::Int))
}

#[test]
fn simple_unify_check() {
    let set = FxHashSet::from_iter(vec![
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
