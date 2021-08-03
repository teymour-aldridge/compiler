//! Type checking.

use std::collections::HashMap;

use crate::id::{Id, TaggedAst};

mod constraints;

pub struct TyTable {
    table: HashMap<Id, Ty>,
}

pub enum Ty {
    Int,
    Bool,
}

pub fn check(ty: &TaggedAst) -> TyTable {
    todo!()
}
