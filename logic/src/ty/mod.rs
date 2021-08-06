//! Type checking.

use std::collections::HashMap;

use crate::{
    id::{Id, TaggedAst},
    ty::constraints::collect,
};

mod constraints;

pub struct TyTable {
    #[allow(unused)]
    table: HashMap<Id, Ty>,
}

pub enum Ty {
    Int,
    Bool,
}

pub fn type_check(ast: &TaggedAst) -> TyTable {
    // todo: report errors properly
    let _constraints = collect(ast).expect("constraint gathering error");

    todo!()
}
