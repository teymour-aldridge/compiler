use crate::diagnostics::span::{HasSpan, IndexOnlySpan, Span};

use super::{
    block::{Block, BlockRef},
    expr::{Expr, ExprRef},
    ident::{Ident, IdentRef},
    table::{Id, ItemKind, ItemRef, ParseContext},
    utils::{Input, Parse, ParseError},
};

#[derive(Debug, PartialEq, Eq)]
pub struct ForLoop {
    pub(crate) var: IdentRef,
    pub(crate) between: Between,
    pub(crate) block: BlockRef,
    pub(crate) indent: usize,
    pub(crate) span: Span,
}

pub struct ForRef {
    pub(crate) id: Id,
}

impl From<ForRef> for ItemRef {
    fn from(f: ForRef) -> Self {
        ItemRef {
            id: f.id,
            item_kind: ItemKind::For,
        }
    }
}

impl<'i> Parse<'i> for ForLoop {
    type Context = ParseContext<'i>;
    type Output = ForRef;

    fn parse(input: &mut Input<'i>, ctx: &mut ParseContext<'i>) -> Result<ForRef, ParseError> {
        let rec = input.start_recording();
        input.parse_token("for")?;
        input.skip_whitespace()?;

        let var = Ident::parse(input, ctx)?;

        input.skip_whitespace()?;
        input.parse_token("=")?;
        input.skip_whitespace()?;
        let between = Between::parse(input, ctx)?;

        input.advance_whitespace_and_new_line()?;

        let block = Block::parse(input, ctx, false)?;

        input.advance_indent()?;
        input.parse_token("next")?;
        input.skip_whitespace()?;

        let ident = Ident::parse(input, ctx)?;

        let ident_value = *ctx.table.get_ident(ident);
        let var_value = *ctx.table.get_ident(var);
        if ident_value != var_value {
            return Err(ParseError::UnexpectedToken {
                explanation: format!(
                    "Expected the identifier `{}` here, but instead found `{}`.",
                    var_value, ident_value
                ),
                span: IndexOnlySpan::from(ident_value.span(&ctx.table)),
            });
        }

        input.skip_whitespace()?;

        if !input.is_empty() && !input.starts_with('\n') {
            return Err(ParseError::UnexpectedToken {
                explanation: "Expected a new line here.".to_string(),
                span: {
                    let moved = input.chars().next().unwrap().len_utf8();
                    IndexOnlySpan::new(input.position().index, input.position().index + moved)
                },
            });
        }

        let new_id = ctx.new_id();
        ctx.table.for_.insert(
            new_id,
            Self {
                var,
                between,
                block,
                indent: input.indent,
                span: rec.finish_recording(input),
            },
        );

        Ok(ForRef { id: new_id })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Between {
    pub(crate) start: ExprRef,
    pub(crate) stop: ExprRef,
    pub(crate) step: Option<ExprRef>,
}

impl<'i> Parse<'i> for Between {
    type Context = ParseContext<'i>;
    type Output = Self;

    fn parse(input: &mut Input<'i>, ctx: &mut ParseContext<'i>) -> Result<Self, ParseError> {
        let start = Expr::parse_bp_stop_if(input, 0, |input| input.starts_with("to"), ctx)?
            .ok_or(ParseError::__NonExhaustive)?;

        input.skip_whitespace()?;

        input.parse_token("to")?;

        input.skip_whitespace()?;

        let stop = Expr::parse_bp_stop_if(input, 0, |input| input.starts_with("step"), ctx)?
            .ok_or(ParseError::__NonExhaustive)?;

        let step = if input.clone().parse_token("step").is_ok() {
            input.parse_token("step")?;
            Some(
                Expr::parse_bp_stop_if(input, 0, |input| input.starts_with('\n'), ctx)?
                    .ok_or(ParseError::__NonExhaustive)?,
            )
        } else {
            None
        };

        Ok(Self { start, stop, step })
    }
}
