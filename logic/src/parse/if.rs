use std::fmt::{self, Write};

use crate::{
    diagnostics::span::Span,
    parse::utils::{write_indentation, ParseError},
};

use super::{block::Block, expr::Expr, ident::Ident, utils::Parse};

#[derive(Debug, PartialEq, Eq)]
pub struct If<'a, IDENT = Ident<'a>, EXPR = Expr<'a>> {
    pub(crate) r#if: Branch<'a, IDENT, EXPR>,
    pub(crate) else_ifs: Vec<Branch<'a, IDENT, EXPR>>,
    pub(crate) r#else: Option<Block<'a, IDENT, EXPR>>,
    pub(crate) indent: usize,
    pub(crate) span: Span,
}

impl<'a> Parse<'a> for If<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError> {
        let rec = input.start_recording();

        input.advance_indent()?;
        input.parse_token("if")?;
        let r#if = Branch {
            condition: {
                let cond = Expr::parse_bp_stop_if(input, 0, |input| input.starts_with("then"))?
                    .ok_or(ParseError::UnexpectedEndOfInput)?;
                input.skip_whitespace()?;
                input.parse_token("then")?;
                input.skip_whitespace()?;
                cond
            },
            block: {
                let block = Block::parse(input)?;
                input.skip_whitespace()?;
                block
            },
        };

        let mut elseifs = vec![];

        loop {
            input.advance_indent()?;
            if input.starts_with("elseif") {
                input.parse_token("elseif")?;
                let condition =
                    Expr::parse_bp_stop_if(input, 0, |input| input.starts_with("then"))?
                        .ok_or(ParseError::UnexpectedEndOfInput)?;
                input.parse_token("then")?;
                input.advance_whitespace_and_new_line()?;
                let block = Block::parse(input)?;
                input.advance_whitespace_and_new_line()?;
                elseifs.push(Branch { condition, block });
            } else if input.starts_with("else") {
                input.advance_whitespace_and_new_line()?;
                let block = Block::parse(input)?;
                input.advance_whitespace_and_new_line()?;
                input.advance_indent()?;
                input.parse_token("endif")?;
                input.skip_whitespace()?;
                return Ok(Self {
                    r#if,
                    else_ifs: elseifs,
                    r#else: Some(block),
                    indent: input.indent,
                    span: rec.finish_recording(input),
                });
            } else {
                input.parse_token("endif")?;
                input.skip_whitespace()?;
                input.assert_new_line()?;
                return Ok(Self {
                    r#if,
                    else_ifs: elseifs,
                    r#else: None,
                    indent: input.indent,
                    span: rec.finish_recording(input),
                });
            }
        }
    }
}

impl fmt::Display for If<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_indentation(self.indent, f)?;
        f.write_str("if ")?;
        self.r#if.condition.fmt(f)?;
        f.write_str(" then")?;
        f.write_char('\n')?;
        self.r#if.block.fmt(f)?;
        f.write_char('\n')?;

        for else_if in &self.else_ifs {
            write_indentation(self.indent, f)?;
            f.write_str("elseif ")?;
            else_if.condition.fmt(f)?;
            f.write_str("then")?;
            f.write_char('\n')?;
            else_if.block.fmt(f)?;
            f.write_char('\n')?;
        }
        if let Some(ref r#else) = self.r#else {
            write_indentation(self.indent, f)?;
            f.write_str("else")?;
            f.write_char('\n')?;
            r#else.fmt(f)?;
            f.write_char('\n')?;
        }
        f.write_str("endif")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Branch<'a, IDENT = Ident<'a>, EXPR = Expr<'a>> {
    pub(crate) condition: EXPR,
    pub(crate) block: Block<'a, IDENT, EXPR>,
}
