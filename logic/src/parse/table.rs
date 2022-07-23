use std::{collections::BTreeMap, fmt};

use super::{
    block::{Block, BlockRef},
    expr::{Expr, ExprRef},
    func::{Func, FuncRef, Return},
    ident::{Ident, IdentRef},
    parse_statements,
    r#for::ForLoop,
    r#if::If,
    r#while::While,
    record::{Record, RecordRef},
    utils::{Input, ParseError},
};

/// Contains all the items parsed by the compiler.
///
/// This is still an abstract syntax tree, but we maintain a reference to each
/// node directly (rather than maintaining a reference to only the root node
/// and then traversing the tree wherever necessary).
#[derive(Default, Debug)]
pub struct ParseTable<'i> {
    pub(crate) block: BTreeMap<Id, Block>,
    pub(crate) expr: BTreeMap<Id, Expr<'i>>,
    pub(crate) for_: BTreeMap<Id, ForLoop>,
    pub(crate) ident: BTreeMap<Id, Ident<'i>>,
    pub(crate) if_: BTreeMap<Id, If>,
    pub(crate) record_: BTreeMap<Id, Record>,
    pub(crate) return_: BTreeMap<Id, Return>,
    /// We create a root element in order to ensure that everything (except the
    /// root) has a parent.
    pub(crate) root: (Id, Block),
    pub(crate) func: BTreeMap<Id, Func>,
    pub(crate) while_: BTreeMap<Id, While>,
}

impl<'i> ParseTable<'i> {
    /// Retrieves the identifier corresponding to the provided reference,
    /// panicking if it does not exist.
    #[inline]
    pub(crate) fn get_ident(&self, ref_: IdentRef) -> &Ident<'i> {
        self.try_get_ident(ref_).unwrap()
    }

    #[inline]
    pub(crate) fn try_get_ident(&self, ref_: IdentRef) -> Option<&Ident<'i>> {
        self.ident.get(&ref_.id)
    }

    #[inline]
    /// Tries to retrieve the given identifier. This function is identical to
    /// [`ParseTable::get_ident`], except that it also returns the [`Id`] of the
    /// ident.
    pub(crate) fn get_ident_with_id(&self, ref_: IdentRef) -> WithId<&Ident> {
        self.try_get_ident_with_id(ref_).unwrap()
    }

    #[inline]
    pub(crate) fn try_get_ident_with_id(&self, ref_: IdentRef) -> Option<WithId<&Ident<'i>>> {
        self.ident.get_key_value(&ref_.id).map(|(id, item)| WithId {
            inner: item,
            id: *id,
        })
    }

    /// Tries to retrieve the item which the [`ItemRef`] identifies. This will
    /// return an enumeration with all the possible types ([`Item`]).
    pub(crate) fn get(&self, ref_: &ItemRef) -> Option<Item> {
        match ref_.item_kind {
            ItemKind::Root => self
                .root
                .1
                .inner
                .iter()
                .find(|root_item| (root_item.id == ref_.id))
                .map(|ref_| self.get(ref_))
                .flatten(),
            ItemKind::If => self.if_.get(&ref_.id).map(Item::If),
            ItemKind::While => self.while_.get(&ref_.id).map(Item::While),
            ItemKind::For => self.for_.get(&ref_.id).map(Item::For),
            ItemKind::Record => self.record_.get(&ref_.id).map(Item::Record),
            ItemKind::Ident => self.ident.get(&ref_.id).map(Item::Ident),
            ItemKind::Expr => self.expr.get(&ref_.id).map(Item::Expr),
            ItemKind::Block => self.block.get(&ref_.id).map(Item::Block),
            ItemKind::Func => self.func.get(&ref_.id).map(Item::Func),
            ItemKind::Return => self.return_.get(&ref_.id).map(Item::Return),
        }
    }

    pub(crate) fn get_expr(&self, expr_ref: &ExprRef) -> &Expr {
        self.try_get_expr(expr_ref).unwrap()
    }

    pub(crate) fn try_get_expr(&self, expr_ref: &ExprRef) -> Option<&Expr> {
        self.expr.get(&expr_ref.id)
    }

    pub(crate) fn get_expr_with_id(&self, condition: ExprRef) -> WithId<&Expr> {
        self.try_get_expr_with_id(condition).unwrap()
    }

    pub(crate) fn try_get_expr_with_id(&self, condition: ExprRef) -> Option<WithId<&Expr>> {
        self.expr
            .get_key_value(&condition.id)
            .map(|(id, inner)| WithId { inner, id: *id })
    }

    pub(crate) fn get_func(&self, func: super::func::FuncRef) -> &Func {
        self.try_get_func(func).unwrap()
    }

    pub(crate) fn try_get_func(&self, func: FuncRef) -> Option<&Func> {
        self.func.get(&func.id)
    }

    pub(crate) fn get_block_with_id(&self, block: super::block::BlockRef) -> WithId<&Block> {
        self.try_get_block_with_id(block).unwrap()
    }

    pub(crate) fn try_get_block_with_id(&self, block: BlockRef) -> Option<WithId<&Block>> {
        self.block
            .get_key_value(&block.id)
            .map(|(id, inner)| WithId { inner, id: *id })
    }

    pub(crate) fn get_block(&self, block: &BlockRef) -> &Block {
        self.try_get_block(block).unwrap()
    }

    pub(crate) fn try_get_block(&self, block: &BlockRef) -> Option<&Block> {
        self.block.get(&block.id)
    }

    pub(crate) fn get_record(&self, record_ty: RecordRef) -> &Record {
        self.try_get_record(record_ty).unwrap()
    }

    pub(crate) fn try_get_record(&self, record_ty: RecordRef) -> Option<&Record> {
        self.record_.get(&record_ty.id)
    }
}

#[derive(Debug)]
pub struct WithId<T> {
    pub inner: T,
    pub id: Id,
}

impl<T> WithId<T> {
    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn id(&self) -> Id {
        self.id
    }
}

/// Contains a reference to a concrete item.
pub enum Item<'i> {
    If(&'i If),
    While(&'i While),
    For(&'i ForLoop),
    Record(&'i Record),
    Ident(&'i Ident<'i>),
    Expr(&'i Expr<'i>),
    Block(&'i Block),
    Func(&'i Func),
    Return(&'i Return),
}

impl<'i> Item<'i> {
    pub fn as_if(&self) -> Option<&&'i If> {
        if let Self::If(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_while(&self) -> Option<&&'i While> {
        if let Self::While(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_expr(&self) -> Option<&&'i Expr<'i>> {
        if let Self::Expr(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_func(&self) -> Option<&&'i Func> {
        if let Self::Func(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

pub struct ParseContext<'i> {
    pub(crate) table: ParseTable<'i>,
    pub(crate) tagging: Tagging<'i>,
}

impl ParseContext<'_> {
    pub fn new() -> Self {
        ParseContext {
            table: ParseTable::default(),
            tagging: Tagging {
                variable_ids: Default::default(),
                id_to_names: Default::default(),
                monotonic: IdGen::new(0),
                scopes: vec![],
            },
        }
    }

    pub fn new_id(&mut self) -> Id {
        self.tagging.monotonic.generate_id()
    }

    pub fn push_scope(&mut self) {
        self.tagging.push_scope()
    }

    pub fn pop_scope(&mut self, remove_additions: bool) {
        self.tagging.pop_scope(remove_additions)
    }
}

#[derive(Debug, Default)]
pub struct IdGen {
    current_id: u32,
}

impl IdGen {
    pub fn new(current_id: u32) -> Self {
        Self { current_id }
    }

    pub fn generate_id(&mut self) -> Id {
        let res = self.current_id;
        self.current_id += 1;
        Id { id: res }
    }
}

pub fn parse<'i>(input: &'i str) -> Result<ParseTable<'i>, ParseError> {
    let mut input = Input::new(input);
    let mut ctx = ParseContext::new();

    ctx.push_scope();

    let statements = parse_statements(&mut input, &mut ctx)?;

    let id = ctx.new_id();
    let block = Block { inner: statements };

    ctx.table.root = (id, block);

    ctx.pop_scope(true);

    debug_assert!(ctx.tagging.scopes.is_empty());

    Ok(ctx.table)
}

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
/// Uniquely identifies any item in the codebase.
pub struct ItemRef {
    pub(crate) id: Id,
    pub(crate) item_kind: ItemKind,
}

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone, PartialOrd, Ord, Default)]
pub struct Id {
    /// We use a [`u32`] because we can then achieve 64-bit alignment for [`ItemRef`]
    id: u32,
}

impl Id {
    pub fn new(id: u32) -> Self {
        Self { id }
    }

    pub fn as_u32(&self) -> u32 {
        self.id
    }
}

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.id.fmt(f)
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub enum ItemKind {
    Root,
    If,
    While,
    For,
    Record,
    Ident,
    Expr,
    Block,
    Func,
    Return,
}

#[derive(Debug, Default)]
/// Stores and tracks when variables enter/leave scopes. We calculate this
/// information during parsing. It does make the parsing process a little more
/// complex, but there doesn't seem to be a better stage to do this as. We used
/// to do this during a seperate phase (after parsing, but before the rest of
/// the analysis), and it doesn't seem to make much difference doing it this
/// way.
pub struct Tagging<'i> {
    pub(crate) variable_ids: BTreeMap<Ident<'i>, Id>,
    pub(crate) id_to_names: BTreeMap<Id, Ident<'i>>,
    pub(crate) monotonic: IdGen,
    pub(crate) scopes: Vec<Scope<'i>>,
}

impl<'i> Tagging<'i> {
    fn push_scope(&mut self) {
        self.scopes.push(Scope::default())
    }

    fn pop_scope(&mut self, remove_additions: bool) {
        if let Some(scope) = self.scopes.pop() {
            let mut for_removal = vec![];
            self.variable_ids
                .iter_mut()
                .map(|(key, val)| {
                    if let Some(edit) = scope.edits.iter().find(|edit| match edit {
                        Edit::Overwrite { ident, id, with: _ } => ident == key && val != id,
                        Edit::Add(tagged) => tagged.inner == *key,
                    }) {
                        match edit {
                            Edit::Overwrite {
                                ident: _,
                                id,
                                with: _,
                            } => *val = *id,
                            Edit::Add(ident) if remove_additions => {
                                for_removal.push(ident.inner);
                            }
                            _ => {}
                        }
                    }
                })
                .for_each(drop);
            for each in for_removal {
                self.variable_ids.remove(&each);
            }
        };
    }
}

#[derive(Debug, Default)]
/// A [`Scope`] refers quite literally to a syntactical scope (e.g. there is one
/// inside each function, block, etc). We store all the variable changes (i.e.
/// which variables were created, overwritten, etc.) so that we can then restore
/// the information when we "pop" a scope (i.e. return to the parent scope).
pub struct Scope<'i> {
    pub(crate) edits: Vec<Edit<'i>>,
}

#[derive(Debug)]
/// A change which occurs within a [`Scope`].
pub enum Edit<'i> {
    Overwrite { ident: Ident<'i>, id: Id, with: Id },
    Add(WithId<Ident<'i>>),
}

#[cfg(test)]
mod test {}
