use std::fmt::{self, Write};

use self::{
    expr::Expr,
    r#for::ForLoop,
    utils::{write_indentation, Input, Parse, ParseError},
};

pub mod r#block;
pub mod expr;
pub mod r#for;
pub mod ident;
#[cfg(test)]
mod test;
pub mod utils;

#[derive(Debug, PartialEq, Eq)]
pub struct Ast<'a> {
    pub nodes: Vec<Node<'a>>,
    indent: usize,
}

impl fmt::Display for Ast<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for node in &self.nodes {
            write_indentation(self.indent, f)?;
            node.fmt(f)?;
            f.write_char(' ')?;
        }

        Ok(())
    }
}

impl<'a> Parse<'a> for Ast<'a> {
    fn parse(input: &mut utils::Input<'a>) -> Result<Self, utils::ParseError<'a>> {
        let mut nodes = vec![];
        loop {
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
pub enum Node<'a> {
    Expr(Expr<'a>),
    For(ForLoop<'a>),
}

impl<'a> Parse<'a> for Node<'a> {
    fn parse(input: &mut utils::Input<'a>) -> Result<Self, utils::ParseError<'a>> {
        if input.starts_with("for") {
            ForLoop::parse(input).map(Self::For)
        } else {
            dbg!(&input);
            Expr::parse(input).map(Self::Expr)
        }
    }
}

impl fmt::Display for Node<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Expr(e) => e.fmt(f),
            Node::For(l) => l.fmt(f),
        }
    }
}

pub fn parse<'a>(input: &'a str) -> Result<Ast<'a>, ParseError<'a>> {
    let mut input = Input::new(input);
    Ast::parse(&mut input)
}
