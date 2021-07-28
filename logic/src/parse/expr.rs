use super::{ident::Ident, utils::Parse};

pub enum Expr<'a> {
    Ident(Ident<'a>),
}

impl<'a> Parse<'a> for Expr<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        Ident::parse(input).map(Expr::Ident)
    }
}
