use std::fmt;

use super::{expr::Expr, ident::Ident, utils::Parse, Ast};

#[derive(Debug, PartialEq, Eq)]
pub struct Block<'a, IDENT = Ident<'a>, EXPR = Expr<'a, IDENT>> {
    pub(crate) inner: Ast<'a, IDENT, EXPR>,
}

impl fmt::Display for Block<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<'a> Parse<'a> for Block<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        input.increment_indent(2);
        let res = Ok(Self {
            inner: Ast::parse(input)?,
        });
        input.decrement_indent(2);
        res
    }
}
