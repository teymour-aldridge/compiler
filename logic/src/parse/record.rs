use crate::{
    diagnostics::span::{HasSpan, Span, Spanned},
    parse::utils::ParseError,
    ty::PrimitiveType,
};

use super::{
    ident::{Ident, IdentRef},
    table::{Id, ItemKind, ItemRef, ParseContext, ParseTable},
    utils::Parse,
};

#[derive(Debug, PartialEq, Eq, Hash)]
/// A "record" (aka struct).
pub struct Record {
    pub(crate) name: IdentRef,
    pub(crate) fields: Vec<Field>,
    pub(crate) indent: usize,
}

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub struct RecordRef {
    pub(crate) id: Id,
}

impl From<RecordRef> for ItemRef {
    fn from(r: RecordRef) -> Self {
        ItemRef {
            id: r.id,
            item_kind: ItemKind::Record,
        }
    }
}

impl HasSpan for Record {
    fn span<'i>(&self, table: &'i ParseTable<'i>) -> Span {
        let name = table.get_ident(self.name);
        // todo: report proper span
        Span::new(name.span(table).start(), name.span(table).stop())
    }
}

impl<'i> Parse<'i> for Record {
    type Context = ParseContext<'i>;
    type Output = RecordRef;

    fn parse(
        input: &mut super::utils::Input<'i>,
        ctx: &mut ParseContext<'i>,
    ) -> Result<RecordRef, super::utils::ParseError> {
        let indent = input.indent;
        input.parse_token("record")?;
        input.skip_whitespace()?;
        let name = Ident::parse(input, ctx)?;
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
                input.advance_indent()?;
                input.parse_token("endrecord")?;
                let id = ctx.new_id();
                ctx.table.record_.insert(
                    id,
                    Self {
                        name,
                        fields,
                        indent,
                    },
                );
                return Ok(RecordRef { id });
            } else {
                input.advance_indent()?;
                fields.push(Field::parse(input, ctx)?);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
/// A field of a [Record].
pub struct Field {
    pub(crate) name: IdentRef,
    /// The type of this field.
    ///
    /// todo: allow non-primitive types as fields
    pub(crate) ty: Spanned<PrimitiveType>,
}

impl<'i> Parse<'i> for Field {
    type Context = ParseContext<'i>;
    type Output = Self;

    fn parse(
        input: &mut super::utils::Input<'i>,
        ctx: &mut ParseContext<'i>,
    ) -> Result<Self, super::utils::ParseError> {
        let name = Ident::parse(input, ctx)?;
        input.skip_whitespace()?;
        input.parse_token("of")?;
        input.skip_whitespace()?;
        let ty_symbol_ref = Ident::parse(input, ctx)?;
        let ty_symbol = ctx.table.get_ident(ty_symbol_ref);
        let ty = match ty_symbol.inner() {
            "Bool" => PrimitiveType::Bool,
            "Int" => PrimitiveType::Int,
            "StrSlice" => PrimitiveType::StrSlice,
            "Pointer" => PrimitiveType::Pointer,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    // don't say pointer here, because this is only available internally
                    explanation: "Expected a type here (one of `Bool`, `Int` or `String`)."
                        .to_string(),
                    span: ty_symbol.span(&ctx.table).into(),
                });
            }
        };

        Ok(Field {
            name,
            ty: Spanned::new(ty_symbol.span(&ctx.table), ty),
        })
    }
}
