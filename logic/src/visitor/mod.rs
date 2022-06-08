use crate::id::{
    TaggedAst, TaggedBlock, TaggedExpr, TaggedFor, TaggedFunc, TaggedIdent, TaggedIf, TaggedNode,
    TaggedRecord, TaggedReturn, TaggedWhile,
};

/// The <`Visitor`> trait makes it possible to traverse the AST and output
/// information.
pub trait Visitor<'ctx> {
    type Output;

    fn visit_ast(&mut self, ast: &'ctx TaggedAst<'ctx>) -> Vec<Self::Output> {
        let mut res = vec![];
        for node in &ast.nodes {
            res.push(self.visit_node(node));
        }
        res
    }

    fn visit_node(&mut self, node: &'ctx TaggedNode<'ctx>) -> Self::Output {
        match node {
            crate::parse::Node::Expr(expr) => self.visit_expr(expr),
            crate::parse::Node::For(stmt) => self.visit_for(stmt),
            crate::parse::Node::If(stmt) => self.visit_if(stmt),
            crate::parse::Node::While(stmt) => self.visit_while(stmt),
            crate::parse::Node::Return(ret) => self.visit_ret(ret),
            crate::parse::Node::Func(func) => self.visit_func(func),
            crate::parse::Node::Record(rec) => self.visit_rec(rec),
        }
    }

    fn visit_rec(&mut self, rec: &'ctx TaggedRecord<'ctx>) -> Self::Output;

    fn visit_expr(&mut self, expr: &'ctx TaggedExpr<'ctx>) -> Self::Output;

    fn visit_for(&mut self, stmt: &'ctx TaggedFor<'ctx>) -> Self::Output;

    fn visit_if(&mut self, stmt: &'ctx TaggedIf<'ctx>) -> Self::Output;

    fn visit_while(&mut self, stmt: &'ctx TaggedWhile<'ctx>) -> Self::Output;

    fn visit_ret(&mut self, ret: &'ctx TaggedReturn<'ctx>) -> Self::Output;

    fn visit_func(&mut self, func: &'ctx TaggedFunc<'ctx>) -> Self::Output;

    fn visit_ident(&mut self, ident: &'ctx TaggedIdent<'ctx>) -> Self::Output;

    fn visit_block(&mut self, block: &'ctx TaggedBlock<'ctx>) -> Vec<Self::Output> {
        self.visit_ast(&block.inner)
    }
}
