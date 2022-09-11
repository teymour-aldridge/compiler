use crate::parse::{
    ident::IdentRef,
    table::{Id, ParseTable},
};

use super::{
    constraints::{Constraint, ConstraintInner},
    Ty,
};

#[allow(dead_code)]
pub(crate) fn print_constraint_set<'a>(
    constraints: &mut impl Iterator<Item = &'a Constraint>,
    table: &ParseTable,
) {
    for constraint in constraints {
        let inner: &ConstraintInner = &constraint.inner;
        match inner {
            ConstraintInner::IdToTy { id, ty } => {
                let id = id.as_u32();
                let ident_str = if let Some(ident) = table.try_get_ident(IdentRef::new(Id::new(id)))
                {
                    format!("(note: name={})", ident.inner())
                } else {
                    "".to_string()
                };
                let ty: Ty = ty.token;
                println!("{id} {ident_str} = {ty:?}")
            }
            ConstraintInner::IdToId { id, to } => {
                let id = id.as_u32();
                let ident_str = if let Some(ident) = table.try_get_ident(IdentRef::new(Id::new(id)))
                {
                    format!("(note: name={})", ident.inner())
                } else {
                    "".to_string()
                };

                let to = to.as_u32();
                let to_ident_str =
                    if let Some(ident) = table.try_get_ident(IdentRef::new(Id::new(to))) {
                        format!("(note: name={})", ident.inner())
                    } else {
                        "".to_string()
                    };

                println!("{id} {ident_str} = {to} {to_ident_str}")
            }
            ConstraintInner::TyToTy { ty, to } => {
                println!("{ty:?} = {to:?}")
            }
        }
    }
}
