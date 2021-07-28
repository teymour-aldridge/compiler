use self::{
    assign::Assignment,
    expr::Expr,
    ident::Ident,
    utils::{Input, Parse, ParseError},
};

pub mod assign;
pub mod expr;
pub mod ident;
pub mod utils;

pub struct Ast<'a> {
    pub nodes: Vec<Node<'a>>,
}

impl<'a> Parse<'a> for Ast<'a> {
    fn parse(input: &mut utils::Input<'a>) -> Result<Self, utils::ParseError<'a>> {
        let mut nodes = vec![];
        loop {
            input.skip_whitespace()?;
            if input.is_empty() {
                println!("empty");
                return Ok(Self { nodes });
            } else {
                nodes.push(Node::parse(input)?)
            }
        }
    }
}

pub enum Node<'a> {
    Assignment(Assignment<'a>),
    Expr(Expr<'a>),
}

impl<'a> Parse<'a> for Node<'a> {
    fn parse(input: &mut utils::Input<'a>) -> Result<Self, utils::ParseError<'a>> {
        let mut peek = *input;
        if Ident::parse(&mut peek).is_ok() {
            peek.skip_whitespace()?;
            if peek.peek_char() == Some('=') {
                Assignment::parse(input).map(Self::Assignment)
            } else {
                Expr::parse(input).map(Self::Expr)
            }
        } else {
            Expr::parse(input).map(Self::Expr)
        }
    }
}

pub fn parse<'a>(input: &'a str) -> Result<Ast<'a>, ParseError<'a>> {
    let mut input = Input::new(input);
    Ast::parse(&mut input)
}
