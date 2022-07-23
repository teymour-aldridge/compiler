#![deny(clippy::disallowed_types)]
#![cfg_attr(all(test, nightly), feature(no_coverage, trivial_bounds, type_alias_impl_trait))]

pub mod codegen;
pub mod diagnostics;
pub mod parse;
pub mod ty;
pub mod visitor;
