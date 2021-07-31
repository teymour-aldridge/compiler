use std::fmt;

use crate::parse::utils::{write_indentation, ParseError};

use super::{block::Block, expr::Expr, utils::Parse};

#[derive(Debug, PartialEq, Eq)]
pub struct If<'a> {
    r#if: Branch<'a>,
    else_ifs: Vec<Branch<'a>>,
    r#else: Option<Block<'a>>,
    indent: usize,
}

impl<'a> Parse<'a> for If<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        input.advance_indent()?;
        input.parse_token("if")?;
        let r#if = Branch {
            condition: {
                let cond = Expr::parse_bp_stop_if(input, 0, |input| input.starts_with("then"))?
                    .ok_or(ParseError::UnexpectedEndOfInput)?;
                input.skip_whitespace()?;
                input.parse_token("then")?;
                cond
            },
            block: Block::parse(input)?,
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
                let block = Block::parse(input)?;
                elseifs.push(Branch { condition, block });
            } else if input.starts_with("else") {
                let block = Block::parse(input)?;
                input.advance_indent()?;
                input.parse_token("endif")?;
                return Ok(Self {
                    r#if,
                    else_ifs: elseifs,
                    r#else: Some(block),
                    indent: input.indent,
                });
            } else {
                input.parse_token("endif")?;
                return Ok(Self {
                    r#if,
                    else_ifs: elseifs,
                    r#else: None,
                    indent: input.indent,
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
        f.write_str("then")?;
        self.r#if.block.fmt(f)?;
        for else_if in &self.else_ifs {
            write_indentation(self.indent, f)?;
            f.write_str("elseif ")?;
            else_if.condition.fmt(f)?;
            f.write_str("then")?;
            else_if.block.fmt(f)?;
        }
        if let Some(ref r#else) = self.r#else {
            write_indentation(self.indent, f)?;
            f.write_str("else")?;
            r#else.fmt(f)?;
        }
        f.write_str("endif")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Branch<'a> {
    condition: Expr<'a>,
    block: Block<'a>,
}
