//! The parser for programs. It's a recursive descent parser which I try to make emit friendly and
//! helpful error messages.
//!
//! There is something approaching a formal grammar in [`crate::parse::fuzz`].

use self::{
    expr::Expr,
    func::{Func, Return},
    r#for::ForLoop,
    r#if::If,
    r#while::While,
    record::Record,
    table::{ItemRef, ParseContext},
    utils::{Input, Parse, ParseError},
};

pub mod r#block;
pub mod expr;
pub mod r#for;
pub mod func;
pub mod ident;
pub mod r#if;
pub mod lit;
pub mod record;
pub mod table;
pub mod utils;
pub mod r#while;

#[cfg(all(test, fuzzing))]
mod fuzz;

#[cfg(test)]
mod test;
#[cfg(test)]
mod test_scopes;
#[cfg(test)]
/// Tests to check that the compiler produces well-formed and comprehensible
/// error messages.
pub mod ui;

pub use table::parse;

/// Parses a list of statements, returning a list of [`ItemRef`]s.
fn parse_statements<'i>(
    input: &mut Input<'i>,
    ctx: &mut ParseContext<'i>,
) -> Result<Vec<ItemRef>, ParseError> {
    let mut nodes = vec![];
    loop {
        // remove any lines which only consist of whitespace characters
        loop {
            let mut tmp = *input;
            tmp.skip_whitespace()?;
            if tmp.starts_with('\n') {
                input.skip_whitespace()?;
                input.parse_token("\n")?;
            } else {
                break;
            }
        }

        if input.is_empty()
            // note: we rely here on the fact that `&&` is short-circuiting (i.e. if the first
            // operand is false, then the second operand should not be evaluated)
            || (input.indent >= 2 && input.count_indent()? == input.indent - 2)
            || input.chars().all(|char| char.is_whitespace())
        {
            return Ok(nodes);
        } else {
            input.advance_indent()?;
            // comments
            if input.starts_with(";;") {
                input.eat_until_or_end(|c| c == '\n')?;
            } else {
                nodes.push(Node::parse(input, ctx)?);
            }
        }
    }
}

#[cfg(test)]
#[test]
fn empty_statements() {
    assert_eq!(
        parse_statements(&mut Input::new(""), &mut ParseContext::new()),
        Ok(Vec::new())
    );
}

#[derive(Debug, PartialEq, Eq)]
pub struct Node;

impl<'i> Parse<'i> for Node {
    type Output = ItemRef;
    type Context = ParseContext<'i>;

    fn parse(
        input: &mut utils::Input<'i>,
        ctx: &mut ParseContext<'i>,
    ) -> Result<ItemRef, ParseError> {
        if input.starts_with("for ") {
            ForLoop::parse(input, ctx).map(From::from)
        } else if input.starts_with("return ") {
            Return::parse(input, ctx).map(From::from)
        } else if input.starts_with("if ") {
            If::parse(input, ctx).map(From::from)
        } else if input.starts_with("while ") {
            While::parse(input, ctx).map(From::from)
        } else if input.starts_with("function ") {
            Func::parse(input, ctx).map(From::from)
        } else if input.starts_with("record") {
            Record::parse(input, ctx).map(From::from)
        } else {
            Expr::parse(input, ctx).map(From::from)
        }
    }
}
