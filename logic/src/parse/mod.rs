use std::fmt;

use self::{
    expr::Expr,
    utils::{Input, Parse, ParseError},
};

pub mod expr;
pub mod ident;
#[cfg(test)]
mod test;
pub mod utils;

#[derive(Debug, PartialEq, Eq)]
pub struct Ast<'a> {
    pub nodes: Vec<Node<'a>>,
}

impl fmt::Display for Ast<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for node in &self.nodes {
            node.fmt(f)?;
        }

        Ok(())
    }
}

impl<'a> Parse<'a> for Ast<'a> {
    fn parse(input: &mut utils::Input<'a>) -> Result<Self, utils::ParseError<'a>> {
        let mut nodes = vec![];
        loop {
            input.skip_whitespace()?;
            if input.is_empty() {
                return Ok(Self { nodes });
            } else {
                nodes.push(Node::parse(input)?)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Node<'a> {
    Expr(Expr<'a>),
}

impl<'a> Parse<'a> for Node<'a> {
    fn parse(input: &mut utils::Input<'a>) -> Result<Self, utils::ParseError<'a>> {
        Expr::parse(input).map(Self::Expr)
    }
}

impl fmt::Display for Node<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Expr(e) => e.fmt(f),
        }
    }
}

pub fn parse<'a>(input: &'a str) -> Result<Ast<'a>, ParseError<'a>> {
    let mut input = Input::new(input);
    Ast::parse(&mut input)
}
