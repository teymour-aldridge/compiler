use std::fmt::{self, Write};

use self::{
    expr::Expr,
    func::{Func, Return},
    ident::Ident,
    r#for::ForLoop,
    r#if::If,
    r#while::While,
    utils::{write_indentation, Input, Parse, ParseError},
};

pub mod r#block;
pub mod expr;
pub mod r#for;
pub mod func;
pub mod ident;
pub mod r#if;
pub mod lit;
pub mod utils;
pub mod r#while;

#[cfg(test)]
#[cfg(not(disable_fuzzcheck))]
mod fuzz;
#[cfg(test)]
mod test;

#[derive(Debug, PartialEq, Eq)]
pub struct Ast<'a, IDENT = Ident<'a>, EXPR = Expr<'a>> {
    pub nodes: Vec<Node<'a, IDENT, EXPR>>,
    pub(crate) indent: usize,
}

impl fmt::Display for Ast<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for node in &self.nodes {
            write_indentation(self.indent, f)?;
            node.fmt(f)?;
            f.write_char('\n')?;
        }

        Ok(())
    }
}

impl<'a> Parse<'a> for Ast<'a> {
    fn parse(input: &mut utils::Input<'a>) -> Result<Self, utils::ParseError> {
        let mut nodes = vec![];
        loop {
            loop {
                let mut tmp = input.clone();
                tmp.skip_whitespace()?;
                if tmp.starts_with('\n') {
                    input.skip_whitespace()?;
                    input.parse_token("\n")?;
                } else {
                    break;
                }
            }

            if input.is_empty()
                || (input.indent >= 2 && input.count_indent()? == input.indent - 2)
                || input.chars().all(|char| char.is_whitespace())
            {
                return Ok(Self {
                    nodes,
                    indent: input.indent,
                });
            } else {
                input.advance_indent()?;
                nodes.push(Node::parse(input)?);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Node<'a, IDENT = Ident<'a>, EXPR = Expr<'a, IDENT>> {
    Expr(EXPR),
    For(ForLoop<'a, IDENT, EXPR>),
    If(If<'a, IDENT, EXPR>),
    While(While<'a, IDENT, EXPR>),
    Return(Return<'a, EXPR>),
    Func(Func<'a, IDENT, EXPR>),
}

impl<'a, IDENT, EXPR> Node<'a, IDENT, EXPR> {
    pub fn as_expr(&self) -> Option<&EXPR> {
        if let Self::Expr(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl<'a> Parse<'a> for Node<'a> {
    fn parse(input: &mut utils::Input<'a>) -> Result<Self, utils::ParseError> {
        if input.starts_with("for ") {
            ForLoop::parse(input).map(Self::For)
        } else if input.starts_with("return ") {
            Return::parse(input).map(Self::Return)
        } else if input.starts_with("if ") {
            If::parse(input).map(Self::If)
        } else if input.starts_with("while ") {
            While::parse(input).map(Self::While)
        } else if input.starts_with("function ") {
            Func::parse(input).map(Self::Func)
        } else {
            Expr::parse(input).map(Self::Expr)
        }
    }
}

impl fmt::Display for Node<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Expr(e) => e.fmt(f),
            Node::For(l) => l.fmt(f),
            Node::If(i) => i.fmt(f),
            Node::While(w) => w.fmt(f),
            Node::Return(r) => r.fmt(f),
            Node::Func(func) => func.fmt(f),
        }
    }
}

pub fn parse<'a>(input: &'a str) -> Result<Ast<'a>, ParseError> {
    let mut input = Input::new(input);
    Ast::parse(&mut input)
}
