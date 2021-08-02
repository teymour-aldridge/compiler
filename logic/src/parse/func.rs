use std::{
    fmt::{self, Write},
    marker::PhantomData,
};

use super::{
    block::Block,
    expr::Expr,
    ident::Ident,
    utils::{write_indentation, Parse},
};

#[derive(Debug, Eq, PartialEq)]
pub struct Func<'a, IDENT = Ident<'a>, EXPR = Expr<'a>> {
    pub(crate) name: IDENT,
    pub(crate) parameters: Vec<IDENT>,
    pub(crate) block: Block<'a, IDENT, EXPR>,
    pub(crate) indent: usize,
}

impl<'a> Parse<'a> for Func<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        input.advance_indent()?;
        input.parse_token("function")?;
        input.skip_whitespace()?;
        let name = Ident::parse(input)?;
        input.skip_whitespace()?;
        input.parse_token("(")?;

        let parameters = input.delimited_list(Ident::parse, ')', ",")?;

        input.parse_token(")")?;

        input.skip_whitespace()?;
        input.parse_token("\n")?;
        let block = Block::parse(input)?;
        input.advance_indent()?;
        input.parse_token("endfunction")?;
        input.skip_whitespace()?;
        Ok(Self {
            name,
            parameters,
            block,
            indent: input.indent,
        })
    }
}

impl fmt::Display for Func<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_indentation(self.indent, f)?;
        f.write_str("function ")?;
        self.name.fmt(f)?;
        f.write_str(" (")?;
        for param in &self.parameters {
            param.fmt(f)?;
            f.write_char(',')?;
        }
        f.write_str(")\n")?;
        self.block.fmt(f)?;
        f.write_char('\n')?;
        write_indentation(self.indent, f)?;
        f.write_str("endfunction")?;
        f.write_char('\n')
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Return<'a, EXPR = Expr<'a>> {
    pub(crate) expr: EXPR,
    pub(crate) indent: usize,
    pub(crate) _a: PhantomData<&'a ()>,
}

impl<'a> Parse<'a> for Return<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        input.parse_token("return ")?;

        Ok(Self {
            expr: Expr::parse(input)?,
            indent: input.indent,
            _a: PhantomData,
        })
    }
}

impl fmt::Display for Return<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("return ")?;
        self.expr.fmt(f)
    }
}
