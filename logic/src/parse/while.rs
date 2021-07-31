use std::fmt::{self, Write};

use crate::parse::utils::write_indentation;

use super::{block::Block, expr::Expr, utils::Parse};

#[derive(Debug, Eq, PartialEq)]
pub struct While<'a> {
    condition: Expr<'a>,
    block: Block<'a>,
    indent: usize,
}

impl<'a> Parse<'a> for While<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        input.advance_indent()?;
        input.parse_token("while")?;
        input.skip_whitespace()?;
        let condition = Expr::parse(input)?;
        input.advance_whitespace_and_new_line()?;

        let block = Block::parse(input)?;
        input.advance_whitespace_and_new_line()?;
        input.advance_indent()?;
        input.parse_token("endwhile")?;
        input.skip_whitespace()?;
        input.assert_new_line()?;
        Ok(Self {
            condition,
            block,
            indent: input.indent,
        })
    }
}

impl fmt::Display for While<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_indentation(self.indent, f)?;
        f.write_str("while ")?;
        self.condition.fmt(f)?;
        f.write_char('\n')?;
        self.block.fmt(f)?;
        f.write_char('\n')?;
        write_indentation(self.indent, f)?;
        f.write_str("endwhile")
    }
}
