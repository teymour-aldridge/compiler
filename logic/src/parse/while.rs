use std::fmt::{self, Write};

use crate::{diagnostics::span::Span, parse::utils::write_indentation};

use super::{block::Block, expr::Expr, ident::Ident, utils::Parse};

#[derive(Debug, Eq, PartialEq)]
/// A while loop.
pub struct While<'a, IDENT = Ident<'a>, EXPR = Expr<'a>> {
    pub(crate) condition: EXPR,
    pub(crate) block: Block<'a, IDENT, EXPR>,
    pub(crate) indent: usize,
    pub(crate) span: Span,
}

impl<'a> Parse<'a> for While<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError> {
        let rec = input.start_recording();
        input.parse_token("while")?;
        input.skip_whitespace()?;
        let condition = Expr::parse(input)?;
        input.advance_whitespace_and_new_line()?;

        let block = Block::parse(input)?;
        input.advance_indent()?;
        input.parse_token("endwhile")?;
        Ok(Self {
            condition,
            block,
            indent: input.indent,
            span: rec.finish_recording(input),
        })
    }
}

impl fmt::Display for While<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("while ")?;
        self.condition.fmt(f)?;
        f.write_char('\n')?;
        self.block.fmt(f)?;
        f.write_char('\n')?;
        write_indentation(self.indent, f)?;
        f.write_str("endwhile")?;
        f.write_char('\n')
    }
}
