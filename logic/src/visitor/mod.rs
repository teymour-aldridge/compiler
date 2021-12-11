use crate::id::{
    TaggedAst, TaggedBlock, TaggedExpr, TaggedFor, TaggedFunc, TaggedIdent, TaggedIf, TaggedNode,
    TaggedReturn, TaggedWhile,
};

pub trait Visitor {
    type Output;

    fn visit_ast(&mut self, ast: &TaggedAst) -> Vec<Self::Output> {
        let mut res = vec![];
        for node in &ast.nodes {
            res.push(self.visit_node(&node));
        }
        res
    }

    fn visit_node(&mut self, node: &TaggedNode) -> Self::Output {
        match node {
            crate::parse::Node::Expr(expr) => self.visit_expr(expr),
            crate::parse::Node::For(stmt) => self.visit_for(stmt),
            crate::parse::Node::If(stmt) => self.visit_if(stmt),
            crate::parse::Node::While(stmt) => self.visit_while(stmt),
            crate::parse::Node::Return(ret) => self.visit_ret(ret),
            crate::parse::Node::Func(func) => self.visit_func(func),
        }
    }

    fn visit_expr(&mut self, expr: &TaggedExpr) -> Self::Output;

    fn visit_for(&mut self, stmt: &TaggedFor) -> Self::Output;

    fn visit_if(&mut self, stmt: &TaggedIf) -> Self::Output;

    fn visit_while(&mut self, stmt: &TaggedWhile) -> Self::Output;

    fn visit_ret(&mut self, ret: &TaggedReturn) -> Self::Output;

    fn visit_func(&mut self, func: &TaggedFunc) -> Self::Output;

    fn visit_ident(&mut self, ident: &TaggedIdent) -> Self::Output;

    fn visit_block(&mut self, block: &TaggedBlock) -> Vec<Self::Output> {
        self.visit_ast(&block.inner)
    }
}
