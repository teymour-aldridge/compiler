use std::collections::BTreeMap;

use super::{
    block::{Block, BlockRef},
    expr::{Expr, ExprRef},
    ident::{Ident, IdentRef},
    table::{Id, ItemKind, ItemRef, ParseContext},
    utils::{Parse, ParseError},
};

#[derive(Debug, Eq, PartialEq)]
/// A function.
pub struct Func {
    pub(crate) name: IdentRef,
    pub(crate) parameters: Vec<IdentRef>,
    pub(crate) block: BlockRef,
    pub(crate) indent: usize,
}

impl<'i> Parse<'i> for Func {
    type Output = FuncRef;
    type Context = ParseContext<'i>;

    fn parse(
        input: &mut super::utils::Input<'i>,
        ctx: &mut ParseContext<'i>,
    ) -> Result<FuncRef, ParseError> {
        input.parse_token("function")?;
        input.skip_whitespace()?;
        let name = Ident::parse(input, ctx)?;

        // here we do some scope/name resolution stuff (a quick aside from the
        // actual parsing of the function)
        let mut local_variables = BTreeMap::new();
        std::mem::swap(&mut ctx.tagging.variable_ids, &mut local_variables);

        // now back to parsing
        input.skip_whitespace()?;
        input.parse_token("(")?;

        input.skip_whitespace()?;

        // parse all the parameters
        let parameters = if !input.starts_with(')') {
            input.delimited_list(Ident::parse, ')', ",", ctx)?
        } else {
            vec![]
        };

        input.parse_token(")")?;

        input.skip_whitespace()?;
        input.parse_token("\n")?;

        let block = Block::parse(input, ctx, true)?;

        // we now resume our scope-related handling...

        std::mem::swap(&mut ctx.tagging.variable_ids, &mut local_variables);
        // ...back to parsing

        input.advance_indent()?;
        input.parse_token("endfunction")?;
        input.skip_whitespace()?;

        let me = Self {
            name,
            parameters,
            block,
            indent: input.indent,
        };
        let id = ctx.new_id();
        ctx.table.func.insert(id, me);
        Ok(FuncRef { id })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FuncRef {
    pub(crate) id: Id,
}

impl From<FuncRef> for ItemRef {
    fn from(f: FuncRef) -> Self {
        ItemRef {
            id: f.id,
            item_kind: ItemKind::Func,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Return {
    pub(crate) expr: ExprRef,
    pub(crate) indent: usize,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct ReturnRef {
    id: Id,
}

impl From<ReturnRef> for ItemRef {
    fn from(ret: ReturnRef) -> Self {
        ItemRef {
            id: ret.id,
            item_kind: ItemKind::Return,
        }
    }
}

impl<'i> Parse<'i> for Return {
    type Context = ParseContext<'i>;
    type Output = ReturnRef;

    fn parse(
        input: &mut super::utils::Input<'i>,
        ctx: &mut ParseContext<'i>,
    ) -> Result<ReturnRef, ParseError> {
        input.parse_token("return ")?;

        let me = Self {
            expr: Expr::parse(input, ctx)?,
            indent: input.indent,
        };
        let id = ctx.new_id();
        ctx.table.return_.insert(id, me);

        Ok(ReturnRef { id })
    }
}
