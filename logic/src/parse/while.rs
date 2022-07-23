use crate::diagnostics::span::Span;

use super::{
    block::{Block, BlockRef},
    expr::{Expr, ExprRef},
    table::{Id, ItemKind, ItemRef, ParseContext},
    utils::{Input, Parse},
};

#[derive(Debug, Eq, PartialEq)]
/// A while loop.
pub struct While {
    pub(crate) block: BlockRef,
    pub(crate) condition: ExprRef,
    pub(crate) indent: usize,
    pub(crate) span: Span,
}

pub struct WhileRef {
    id: Id,
}

impl From<WhileRef> for ItemRef {
    fn from(w: WhileRef) -> Self {
        ItemRef {
            id: w.id,
            item_kind: ItemKind::While,
        }
    }
}

impl<'i> Parse<'i> for While {
    type Context = ParseContext<'i>;
    type Output = WhileRef;

    fn parse(
        input: &mut Input<'i>,
        ctx: &mut ParseContext<'i>,
    ) -> Result<WhileRef, super::utils::ParseError> {
        let rec = input.start_recording();
        input.parse_token("while")?;
        input.skip_whitespace()?;
        let condition = Expr::parse(input, ctx)?;
        input.advance_whitespace_and_new_line()?;

        let block = Block::parse(input, ctx, false)?;
        input.advance_indent()?;
        input.parse_token("endwhile")?;

        let id = ctx.new_id();
        let me = Self {
            condition,
            block,
            indent: input.indent,
            span: rec.finish_recording(input),
        };

        ctx.table.while_.insert(id, me);
        Ok(WhileRef { id })
    }
}
