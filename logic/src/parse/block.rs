use super::{
    parse_statements,
    table::{Id, ItemRef, ParseContext},
    utils::{Input, ParseError},
};

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Block {
    pub(crate) inner: Vec<ItemRef>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct BlockRef {
    pub(crate) id: Id,
}

impl Block {
    pub fn parse<'i>(
        input: &mut Input<'i>,
        ctx: &mut ParseContext<'i>,
        remove_additions: bool,
    ) -> Result<BlockRef, ParseError> {
        input.increment_indent(2);
        ctx.push_scope();

        let refs = parse_statements(input, ctx)?;

        input.decrement_indent(2);
        ctx.pop_scope(remove_additions);

        let id = ctx.new_id();
        ctx.table.block.insert(id, Self { inner: refs });
        Ok(BlockRef { id })
    }
}
