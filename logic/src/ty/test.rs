use std::iter::FromIterator;

use rustc_hash::FxHashSet;

use crate::{
    diagnostics::span::{Span, Spanned},
    parse::{
        expr::{BinOp, Expr},
        parse,
        record::RecordRef,
        table::Id,
    },
    ty::{
        constraints::{Constraint, ConstraintInner},
        track::{ConstraintId, TraceTable},
        type_check, unify, PrimitiveType, Ty, TyEnv,
    },
};

#[test]
fn simple_type_check() {
    let tree = parse(include_str!("examples/simple")).unwrap();

    let ty_env = type_check(&tree).expect("failed to type check");

    let x = match tree
        .get(tree.root.1.inner.iter().nth(1).unwrap())
        .unwrap()
        .as_expr()
        .unwrap()
    {
        Expr::BinOp(_, ref left, _) => match tree.get_expr(left) {
            Expr::Ident(ref ident) => ident.id,
            _ => panic!(),
        },
        _ => panic!(),
    };

    let y = match tree
        .get(tree.root.1.inner.iter().nth(2).unwrap())
        .unwrap()
        .as_expr()
        .unwrap()
    {
        Expr::BinOp(_, ref left, _) => match tree.get_expr(left) {
            Expr::Ident(ref ident) => ident.id,
            _ => panic!(),
        },
        _ => panic!(),
    };
    assert_eq!(ty_env.ty_of(x), Some(Ty::PrimitiveType(PrimitiveType::Int)));
    assert_eq!(ty_env.ty_of(y), Some(Ty::PrimitiveType(PrimitiveType::Int)))
}

#[test]
fn factorial_type_check() {
    let tree = parse(include_str!("examples/factorial")).unwrap();

    let ty_env = type_check(&tree).expect("failed to type check");

    ty_env.pretty_print(&tree);

    let mut iter = tree.func.iter();
    let next = iter.next().unwrap();
    assert_eq!(tree.get_ident(next.1.name).inner(), "main");
    let main_function = next.1.name.id;

    let nth = tree.func.iter().nth(1).unwrap();
    let (_, factorial_func) = nth;

    assert_eq!(
        ty_env.ty_of(main_function),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );
    assert_eq!(
        ty_env.ty_of(factorial_func.name.id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );

    let if_branch = &tree
        .get(&tree.get_block(&factorial_func.block).inner[0])
        .unwrap()
        .as_if()
        .unwrap()
        .r#if;

    assert_eq!(
        ty_env.ty_of(if_branch.condition.id),
        Some(Ty::PrimitiveType(PrimitiveType::Bool))
    );

    match tree.get_expr(&if_branch.condition) {
        Expr::BinOp(_, ref left, _) => {
            assert_eq!(
                ty_env.ty_of(left.id),
                Some(Ty::PrimitiveType(PrimitiveType::Int))
            )
        }
        _ => panic!("ast does not match expected structure for expr `n == 0`"),
    }
}

#[test]
fn test_record_type_check() {
    let tree = parse(include_str!("examples/record")).unwrap();

    let ty_env = type_check(&tree).unwrap();

    ty_env.pretty_print(&tree);

    let (record_id, _) = tree.record_.iter().next().unwrap();

    let (_, main_func) = tree.func.iter().next().unwrap();
    let main_nodes = tree.block.get(&main_func.block.id).unwrap();
    let (_, left, right) = tree
        .expr
        .get(&main_nodes.inner[0].id)
        .unwrap()
        .as_bin_op()
        .unwrap();
    assert_eq!(
        ty_env.ty_of(right.id),
        Some(Ty::Record {
            ref_: RecordRef { id: *record_id }
        })
    );
    assert_eq!(
        ty_env.ty_of(left.id),
        Some(Ty::Record {
            ref_: RecordRef { id: *record_id }
        })
    );

    let (_, x, expr) = tree
        .get(&main_nodes.inner[1])
        .unwrap()
        .as_expr()
        .unwrap()
        .as_bin_op()
        .unwrap();
    assert_eq!(
        tree.get_ident(*tree.get_expr(x).as_ident().unwrap())
            .inner(),
        "x"
    );
    assert_eq!(
        ty_env.ty_of(x.id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );
    assert_eq!(
        ty_env.ty_of(expr.id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );

    let record = tree.record_.iter().next().unwrap();
    let record_id = record.1.fields.get(0).unwrap().name.id;
    assert_eq!(
        ty_env.ty_of(record_id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    )
}

#[test]
fn simple_unify_check() {
    let set = FxHashSet::from_iter(vec![
        Constraint::new(
            ConstraintId::new(0),
            ConstraintInner::IdToTy {
                id: Spanned::new(Span::null(), Id::new(1)),
                ty: Spanned::new(Span::null(), Ty::PrimitiveType(PrimitiveType::Int)),
            },
        ),
        Constraint::new(
            ConstraintId::new(1),
            ConstraintInner::IdToId {
                id: Spanned::new(Span::null(), Id::new(2)),
                to: Spanned::new(Span::null(), Id::new(1)),
            },
        ),
        Constraint::new(
            ConstraintId::new(2),
            ConstraintInner::IdToId {
                id: Spanned::new(Span::null(), Id::new(3)),
                to: Spanned::new(Span::null(), Id::new(1)),
            },
        ),
        Constraint::new(
            ConstraintId::new(3),
            ConstraintInner::IdToTy {
                id: Spanned::new(Span::null(), Id::new(3)),
                ty: Spanned::new(Span::null(), Ty::PrimitiveType(PrimitiveType::Int)),
            },
        ),
    ]);

    let env = unify(set, TyEnv::new(), &mut TraceTable::default()).unwrap();

    assert_eq!(
        env.ty_of(Id::new(1)).unwrap(),
        Ty::PrimitiveType(PrimitiveType::Int)
    );
    assert_eq!(
        env.ty_of(Id::new(2)).unwrap(),
        Ty::PrimitiveType(PrimitiveType::Int)
    );
    assert_eq!(
        env.ty_of(Id::new(3)).unwrap(),
        Ty::PrimitiveType(PrimitiveType::Int)
    );
}

#[test]
fn iterative_factorial_type_check() {
    let tree = parse(include_str!("examples/iterative-factorial")).unwrap();

    let env = type_check(&tree).expect("failed to type check");

    let item = tree.get(&tree.root.1.inner[0]).unwrap();
    let func = item.as_func().unwrap();
    assert_eq!(
        env.ty_of(func.name.id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );
    assert_eq!(
        env.ty_of(func.parameters.get(0).unwrap().id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );
    let (op, res, _literal) = tree
        .get(&tree.get_block(&func.block).inner[0])
        .unwrap()
        .as_expr()
        .unwrap()
        .as_bin_op()
        .unwrap();
    assert_eq!(*op, BinOp::SetEquals);
    assert_eq!(
        env.ty_of(res.id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );
}

#[test]
fn very_simple() {
    let table = parse(include_str!("examples/very-simple")).expect("failed to parse");
    let ty_env = type_check(&table).expect("failed to type check a well-typed program");
    let id = table
        .ident
        .iter()
        .find_map(|(id, ident)| (ident.inner() == "main").then(|| id))
        .unwrap();
    assert_eq!(
        ty_env.ty_of(*id).unwrap(),
        Ty::PrimitiveType(PrimitiveType::Int)
    );
}

#[test]
fn fib_type_check() {
    let tree = crate::parse::parse(include_str!("examples/fib")).unwrap();

    let env = type_check(&tree).expect("failed to type check fibonacci");

    env.pretty_print(&tree);

    let func = tree.func.iter().next().unwrap();
    assert_eq!(
        env.ty_of(func.1.name.id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );
    assert_eq!(
        env.ty_of(func.1.parameters.get(0).unwrap().id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );
}

#[test]
fn catalan_numbers_type_check() {
    let table = parse(include_str!("examples/catalan")).unwrap();
    let env = type_check(&table).expect("failed to type check a valid program");

    let (_, catalan_func) = table
        .func
        .iter()
        .find(|(_, func)| table.get_ident(func.name).inner() == "catalan")
        .unwrap();

    assert_eq!(
        env.ty_of(catalan_func.name.id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );

    let vertices = catalan_func.parameters[0];
    assert_eq!(
        env.ty_of(vertices.id),
        Some(Ty::PrimitiveType(PrimitiveType::Int))
    );
}

#[test]
fn true_true_errors() {
    let table = parse("True.True").unwrap();
    type_check(&table).expect_err("incorrectly claimed a program with a type error doesn't");
}
