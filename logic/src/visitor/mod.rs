use crate::id::{
    TaggedAst, TaggedBlock, TaggedExpr, TaggedFor, TaggedFunc, TaggedIdent, TaggedIf, TaggedNode,
    TaggedReturn, TaggedWhile,
};

pub trait Visitor<'a, 'ctx> {
    type Output;

    fn visit_ast(&mut self, ast: &'a TaggedAst<'ctx>) -> Vec<Self::Output> {
        let mut res = vec![];
        for node in &ast.nodes {
            res.push(self.visit_node(node));
        }
        res
    }

    fn visit_node(&mut self, node: &'a TaggedNode<'ctx>) -> Self::Output {
        match node {
            crate::parse::Node::Expr(expr) => self.visit_expr(expr),
            crate::parse::Node::For(stmt) => self.visit_for(stmt),
            crate::parse::Node::If(stmt) => self.visit_if(stmt),
            crate::parse::Node::While(stmt) => self.visit_while(stmt),
            crate::parse::Node::Return(ret) => self.visit_ret(ret),
            crate::parse::Node::Func(func) => self.visit_func(func),
            crate::parse::Node::Record(_) => todo!(),
        }
    }

    fn visit_expr(&mut self, expr: &'a TaggedExpr<'ctx>) -> Self::Output;

    fn visit_for(&mut self, stmt: &'a TaggedFor<'ctx>) -> Self::Output;

    fn visit_if(&mut self, stmt: &'a TaggedIf<'ctx>) -> Self::Output;

    fn visit_while(&mut self, stmt: &'a TaggedWhile<'ctx>) -> Self::Output;

    fn visit_ret(&mut self, ret: &'a TaggedReturn<'ctx>) -> Self::Output;

    fn visit_func(&mut self, func: &'a TaggedFunc<'ctx>) -> Self::Output;

    fn visit_ident(&mut self, ident: &'a TaggedIdent<'ctx>) -> Self::Output;

    fn visit_block(&mut self, block: &'a TaggedBlock<'ctx>) -> Vec<Self::Output> {
        self.visit_ast(&block.inner)
    }
}
