use crate::parse::{
    block::Block,
    expr::Expr,
    func::{Func, Return},
    ident::Ident,
    r#for::ForLoop,
    r#if::If,
    r#while::While,
    record::Record,
    table::{ParseTable, WithId},
};

/// The [`Visitor`] trait makes it possible to traverse the [`ParseTable`] as
/// though it were a tree. Every method in this trait has a signature something
/// like
///
/// ```ignore
/// fn visit_x(&mut self, x: &'i x, table: &'i ParseTable<'i>) -> Vec<Self::Output>
/// ```
///
/// - `&mut self` means that you can store useful state inside the type on which
///   you are implementing [`Visitor`].
/// - `&'i x` is the item currently being visited
/// - The [`ParseTable`] contains all the definitions in the current module.
/// - The `Vec<Output>` is optional and can contain the output of the visitor.
///   If you don't emit anything using this mechanism, then you can simply set
///   `Output = ()`. As `std::mem::size_of::<()>() == 0`
pub trait Visitor<'i> {
    type Output;

    fn visit_table(&mut self, table: &'i ParseTable<'i>) -> Vec<Self::Output> {
        self.visit_block(&table.root.1, table)
    }

    fn visit_rec(&mut self, rec: &'i Record, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_expr(&mut self, expr: &'i Expr<'i>, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_for(&mut self, stmt: &'i ForLoop, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_if(&mut self, stmt: &'i If, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_while(&mut self, stmt: &'i While, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_ret(&mut self, ret: &'i Return, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_func(&mut self, func: &'i Func, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_ident(&mut self, ident: &'i Ident<'i>, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_block(&mut self, block: &'i Block, table: &'i ParseTable<'i>) -> Vec<Self::Output> {
        let mut output = vec![];
        for item in &block.inner {
            let n = match table.get(item).unwrap() {
                crate::parse::table::Item::If(if_) => self.visit_if(if_, table),
                crate::parse::table::Item::While(w) => self.visit_while(w, table),
                crate::parse::table::Item::For(f) => self.visit_for(f, table),
                crate::parse::table::Item::Record(r) => self.visit_rec(r, table),
                crate::parse::table::Item::Ident(i) => self.visit_ident(i, table),
                crate::parse::table::Item::Expr(e) => self.visit_expr(e, table),
                // todo: should this be possible?
                crate::parse::table::Item::Block(b) => {
                    output.extend(self.visit_block(b, table));
                    continue;
                }
                crate::parse::table::Item::Func(func) => self.visit_func(func, table),
                crate::parse::table::Item::Return(ret) => self.visit_ret(ret, table),
            };
            output.push(n)
        }
        output
    }
}
/// The [`IdVisitor`] trait makes it possible to traverse the [`ParseTable`] as
/// though it were a tree. Every method in this trait has a signature something
/// like
///
/// ```ignore
/// fn visit_x(&mut self, x: &'i WithId<&'i x>, table: &'i ParseTable<'i>) -> Vec<Self::Output>
/// ```
///
/// - `&mut self` means that you can store useful state inside the type on which
///   you are implementing [`Visitor`].
/// - `&'i WithId<&'i x>` is the type currently being visited (but unlike the
///   case of the [`Visitor] trait, with the item's id).
/// - The [`ParseTable`] contains all the definitions in the current module.
/// - The `Vec<Output>` is optional and can contain the output of the visitor.
///   If you don't emit anything using this mechanism, then you can simply set
///   `Output = ()`. As `std::mem::size_of::<()>() == 0`
pub trait IdVisitor<'i> {
    type Output;

    fn visit_table(&mut self, table: &'i ParseTable<'i>) -> Vec<Self::Output> {
        self.visit_block(
            WithId {
                id: table.root.0,
                inner: &table.root.1,
            },
            table,
        )
    }

    fn visit_rec(&mut self, rec: WithId<&'i Record>, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_expr(&mut self, expr: WithId<&'i Expr<'i>>, table: &'i ParseTable<'i>)
        -> Self::Output;

    fn visit_for(&mut self, stmt: WithId<&'i ForLoop>, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_if(&mut self, stmt: WithId<&'i If>, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_while(&mut self, stmt: WithId<&'i While>, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_ret(&mut self, ret: WithId<&'i Return>, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_func(&mut self, func: WithId<&'i Func>, table: &'i ParseTable<'i>) -> Self::Output;

    fn visit_ident(
        &mut self,
        ident: WithId<&'i Ident<'i>>,
        table: &'i ParseTable<'i>,
    ) -> Self::Output;

    fn visit_block(
        &mut self,
        block: WithId<&'i Block>,
        table: &'i ParseTable<'i>,
    ) -> Vec<Self::Output> {
        let mut output = vec![];
        for item in &block.inner().inner {
            let n = match table.get(item).unwrap() {
                crate::parse::table::Item::If(if_) => self.visit_if(
                    WithId {
                        inner: if_,
                        id: item.id,
                    },
                    table,
                ),
                crate::parse::table::Item::While(w) => self.visit_while(
                    WithId {
                        inner: w,
                        id: item.id,
                    },
                    table,
                ),
                crate::parse::table::Item::For(f) => self.visit_for(
                    WithId {
                        inner: f,
                        id: item.id,
                    },
                    table,
                ),
                crate::parse::table::Item::Record(r) => self.visit_rec(
                    WithId {
                        inner: r,
                        id: item.id,
                    },
                    table,
                ),
                crate::parse::table::Item::Ident(i) => self.visit_ident(
                    WithId {
                        inner: i,
                        id: item.id,
                    },
                    table,
                ),
                crate::parse::table::Item::Expr(e) => self.visit_expr(
                    WithId {
                        inner: e,
                        id: item.id,
                    },
                    table,
                ),
                // todo: should this be possible?
                crate::parse::table::Item::Block(b) => {
                    output.extend(self.visit_block(
                        WithId {
                            inner: b,
                            id: item.id,
                        },
                        table,
                    ));
                    continue;
                }
                crate::parse::table::Item::Func(func) => self.visit_func(
                    WithId {
                        inner: func,
                        id: item.id,
                    },
                    table,
                ),
                crate::parse::table::Item::Return(ret) => self.visit_ret(
                    WithId {
                        inner: ret,
                        id: item.id,
                    },
                    table,
                ),
            };
            output.push(n)
        }
        output
    }
}
