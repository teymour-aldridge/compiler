use core::fmt;
use std::{fmt::Write, marker::PhantomData};

use super::{
    block::Block,
    expr::Expr,
    ident::Ident,
    utils::{write_indentation, Parse, ParseError},
};

#[derive(Debug, PartialEq, Eq)]
pub struct ForLoop<'a, IDENT = Ident<'a>, EXPR = Expr<'a, IDENT>> {
    pub(crate) var: IDENT,
    pub(crate) between: Between<'a, IDENT, EXPR>,
    pub(crate) block: Block<'a, IDENT, EXPR>,
    pub(crate) indent: usize,
}

impl<'a> Parse<'a> for ForLoop<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        input.parse_token("for")?;
        input.skip_whitespace()?;

        let var = Ident::parse(input)?;
        input.skip_whitespace()?;
        input.parse_token("=")?;
        input.skip_whitespace()?;
        let between = Between::parse(input)?;

        input.advance_whitespace_and_new_line()?;

        let block = Block::parse(input)?;

        input.parse_token("\n")?;

        input.advance_indent()?;
        input.parse_token("next")?;
        input.skip_whitespace()?;

        let ident = Ident::parse(input)?;

        if *ident != *var {
            return Err(ParseError::UnexpectedToken {
                token: *ident,
                explanation: format!(
                    "Expected the identifier `{}` here, but instead found `{}`.",
                    *var, *ident
                ),
            });
        }

        input.skip_whitespace()?;

        if !input.is_empty() && input.chars().next() != Some('\n') {
            return Err(ParseError::UnexpectedToken {
                token: input.peek_n(1).unwrap_or(""),
                explanation: "Expected a new line here.".to_string(),
            });
        }

        Ok(Self {
            var,
            between,
            block,
            indent: input.indent,
        })
    }
}

impl fmt::Display for ForLoop<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_indentation(self.indent, f)?;
        f.write_str("for ")?;
        self.var.fmt(f)?;
        f.write_str(" = ")?;
        self.between.start.fmt(f)?;
        f.write_str(" to ")?;
        self.between.stop.fmt(f)?;
        if let Some(ref step) = self.between.step {
            f.write_str(" step ")?;
            step.fmt(f)?;
        }
        f.write_char('\n')?;
        self.block.fmt(f)?;
        f.write_char('\n')?;
        write_indentation(self.indent, f)?;
        f.write_str("next ")?;
        self.var.fmt(f)?;
        f.write_char('\n')
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Between<'a, IDENT = Ident<'a>, EXPR = Expr<'a>> {
    pub(crate) start: EXPR,
    pub(crate) stop: EXPR,
    pub(crate) step: Option<EXPR>,
    pub(crate) _i: PhantomData<&'a IDENT>,
}

impl<'a> Parse<'a> for Between<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        let start = Expr::parse_bp_stop_if(input, 0, |input| input.starts_with("to"))?
            .ok_or(ParseError::__NonExhaustive)?;

        input.skip_whitespace()?;

        input.parse_token("to")?;

        input.skip_whitespace()?;

        let stop = Expr::parse_bp_stop_if(input, 0, |input| input.starts_with("step"))?
            .ok_or(ParseError::__NonExhaustive)?;

        let step = if input.clone().parse_token("step").is_ok() {
            input.parse_token("step")?;
            Some(
                Expr::parse_bp_stop_if(input, 0, |input| input.starts_with('\n'))?
                    .ok_or(ParseError::__NonExhaustive)?,
            )
        } else {
            None
        };

        Ok(Self {
            start,
            stop,
            step,
            _i: PhantomData,
        })
    }
}
