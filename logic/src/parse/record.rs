/// Parses records.
use std::{
    fmt::{self, Write},
    marker::PhantomData,
};

use crate::{diagnostics::span::HasSpan, parse::utils::ParseError, ty::Ty};

use super::{
    ident::Ident,
    utils::{write_indentation, Parse},
};

#[derive(Debug, PartialEq, Eq, Hash)]
/// A "record" (aka struct).
pub struct Record<'a, IDENT = Ident<'a>> {
    pub(crate) name: IDENT,
    pub(crate) fields: Vec<Field<'a, IDENT>>,
    pub(crate) indent: usize,
}

impl fmt::Display for Record<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_indentation(self.indent, f)?;
        f.write_str("record")?;
        self.name.fmt(f)?;
        f.write_char('\n')?;

        for each in &self.fields {
            write_indentation(self.indent + 2, f)?;
            each.fmt(f)?;
            f.write_char('\n')?;
        }

        f.write_str("endrecord")
    }
}

impl<'a> Parse<'a> for Record<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError> {
        let indent = input.indent;
        input.advance_indent()?;
        input.parse_token("record")?;
        input.skip_whitespace()?;
        let name = Ident::parse(input)?;
        input.advance_whitespace_and_new_line()?;
        input.increment_indent(2);

        let mut fields = vec![];
        loop {
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
                || (input.indent >= 2 && input.count_indent()? == input.indent - 2)
                || input.chars().all(|char| char.is_whitespace())
            {
                input.decrement_indent(2);
                input.parse_token("endrecord")?;
                return Ok(Self {
                    name,
                    fields,
                    indent,
                });
            } else {
                input.advance_indent()?;
                fields.push(Field::parse(input)?);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
/// A field of a [Record].
pub struct Field<'a, IDENT = Ident<'a>> {
    pub(crate) name: IDENT,
    pub(crate) ty: Ty,
    pub(crate) _a: PhantomData<&'a IDENT>,
}

impl<'a> Parse<'a> for Field<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError> {
        let name = Ident::parse(input)?;
        input.skip_whitespace()?;
        input.parse_token("of")?;
        input.skip_whitespace()?;
        let ty = Ident::parse(input)?;
        let ty = match *ty {
            "Bool" => Ty::Bool,
            "Int" => Ty::Int,
            "String" => Ty::String,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    explanation: "Expected a type here (one of `Bool`, `Int` or `String`)."
                        .to_string(),
                    span: ty.span().into(),
                })
            }
        };

        Ok(Field {
            name,
            ty,
            _a: PhantomData,
        })
    }
}

impl fmt::Display for Field<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        f.write_str(" of ")?;
        self.ty.fmt(f)
    }
}
