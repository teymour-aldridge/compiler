use crate::{diagnostics::span::Span, parse::utils::ParseError};

use super::{
    block::{Block, BlockRef},
    expr::{Expr, ExprRef},
    table::{Id, ItemKind, ItemRef, ParseContext},
    utils::Parse,
};

#[derive(Debug, PartialEq, Eq)]
pub struct If {
    pub(crate) r#if: Branch,
    pub(crate) else_ifs: Vec<Branch>,
    pub(crate) r#else: Option<BlockRef>,
    pub(crate) indent: usize,
    pub(crate) span: Span,
}

pub struct IfRef {
    id: Id,
}

impl From<IfRef> for ItemRef {
    fn from(if_ref: IfRef) -> Self {
        ItemRef {
            id: if_ref.id,
            item_kind: ItemKind::If,
        }
    }
}

impl<'i> Parse<'i> for If {
    type Context = ParseContext<'i>;
    type Output = IfRef;

    fn parse(
        input: &mut super::utils::Input<'i>,
        ctx: &mut ParseContext<'i>,
    ) -> Result<IfRef, super::utils::ParseError> {
        let rec = input.start_recording();

        input.parse_token("if")?;
        let r#if = Branch {
            condition: {
                let cond =
                    Expr::parse_bp_stop_if(input, 0, |input| input.starts_with("then"), ctx)?
                        .ok_or(ParseError::UnexpectedEndOfInput {
                            span: input.current_span(),
                        })?;
                input.skip_whitespace()?;
                input.parse_token("then")?;
                input.skip_whitespace()?;
                cond
            },
            block: { Block::parse(input, ctx, false)? },
        };

        let mut elseifs = vec![];

        loop {
            input.advance_indent()?;
            if input.starts_with("elseif") {
                input.parse_token("elseif")?;
                let condition =
                    Expr::parse_bp_stop_if(input, 0, |input| input.starts_with("then"), ctx)?
                        .ok_or(ParseError::UnexpectedEndOfInput {
                            span: input.current_span(),
                        })?;
                input.parse_token("then")?;
                input.advance_whitespace_and_new_line()?;
                let block = Block::parse(input, ctx, false)?;
                elseifs.push(Branch { condition, block });
            } else if input.starts_with("else") {
                input.parse_token("else")?;
                input.advance_whitespace_and_new_line()?;
                let block = Block::parse(input, ctx, false)?;
                input.advance_indent()?;
                input.parse_token("endif")?;
                input.skip_whitespace()?;

                let id = ctx.new_id();
                ctx.table.if_.insert(
                    id,
                    Self {
                        r#if,
                        else_ifs: elseifs,
                        r#else: Some(block),
                        indent: input.indent,
                        span: rec.finish_recording(input),
                    },
                );
                return Ok(IfRef { id });
            } else {
                input.parse_token("endif")?;
                input.skip_whitespace()?;
                input.assert_new_line()?;

                let id = ctx.new_id();
                ctx.table.if_.insert(
                    id,
                    Self {
                        r#if,
                        else_ifs: elseifs,
                        r#else: None,
                        indent: input.indent,
                        span: rec.finish_recording(input),
                    },
                );
                return Ok(IfRef { id });
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Branch {
    pub(crate) condition: ExprRef,
    pub(crate) block: BlockRef,
}
