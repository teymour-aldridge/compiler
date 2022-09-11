use std::{
    fmt::{self, Write},
    hash,
};

use crate::diagnostics::span::{HasSpan, IndexOnlySpan, Span};

use super::{
    table::{Edit, Id, ParseContext, ParseTable, WithId},
    utils::{Parse, ParseError},
};

/// These may not be used as identifiers.
pub const KEYWORDS: &[&str] = &[
    "for",
    "if",
    "while",
    "function",
    "return",
    "endfunction",
    "endwhile",
    "endif",
    "True",
    "False",
];

#[derive(Debug, Copy, Clone, Eq)]
/// An identifier.
pub struct Ident<'i> {
    pub(crate) inner: &'i str,
    pub(crate) span: Span,
}

impl<'i> Ident<'i> {
    /// Get a reference to the ident's inner.
    pub fn inner(&self) -> &str {
        self.inner
    }
}

impl HasSpan for Ident<'_> {
    fn span<'i>(&self, _: &'i ParseTable<'i>) -> Span {
        self.span
    }
}

impl<'i> hash::Hash for Ident<'i> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl PartialEq for Ident<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl PartialOrd for Ident<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.inner.partial_cmp(other.inner)
    }
}

impl Ord for Ident<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.inner.cmp(other)
    }
}

impl<'i> std::ops::Deref for Ident<'i> {
    type Target = &'i str;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'i> Parse<'i> for Ident<'i> {
    type Output = IdentRef;
    type Context = ParseContext<'i>;

    fn parse(
        input: &mut super::utils::Input<'i>,
        ctx: &mut ParseContext<'i>,
    ) -> Result<IdentRef, super::utils::ParseError> {
        input.skip_whitespace()?;
        let recording = input.start_recording();
        input
            // eat valid characters
            .eat_until_or_end(|char| !char.is_alphanumeric() && char != '_')
            // then we generate some error messages (if needed) and return them
            // to the user
            .and_then(|inner| {
                // first check if the input was not a valid identifier (and then return an error if
                // this was not the case)...
                if inner.is_empty() {
                    Err(ParseError::UnexpectedEndOfInput {
                        span: input.current_span(),
                    })
                } else if !inner.chars().next().unwrap().is_alphabetic() {
                    Err(ParseError::InvalidIdent {
                        span: input.finish_recording(recording).into(),
                        explanation: format!(
                            "The identifier you have provided is not valid,
                            because it starts with `{}` instead of a letter!",
                            inner.chars().next().unwrap()
                        ),
                    })
                } else if KEYWORDS.contains(&inner) {
                    Err(ParseError::UnexpectedToken {
                        explanation: format!(
                            "Expected an identifier here, but `{}` is a keyword.",
                            inner
                        ),
                        span: { IndexOnlySpan::from(recording.finish_recording(input)) },
                    })
                // ...otherwise parse the identifier
                } else {
                    let me = Self {
                        inner,
                        span: recording.finish_recording(input),
                    };

                    // We cannot just return the identifier as we must first
                    // check whether it is a new addition to the scope, or has
                    // already been defined. Once we retrieve this information
                    // we either introduce a new id (if the variable was not yet
                    // defined) or return the pre-existing one (once we have
                    // retrieved it)
                    if let Some(id) = ctx.tagging.variable_ids.get(&me) {
                        Ok(IdentRef { id: *id })
                    } else {
                        let new_id = ctx.new_id();
                        ctx.tagging.variable_ids.insert(me, new_id);
                        ctx.tagging.id_to_names.insert(new_id, me);
                        if let Some(scope) = ctx.tagging.scopes.iter_mut().last() {
                            scope.edits.push(Edit::Add(WithId {
                                inner: me,
                                id: new_id,
                            }))
                        }
                        ctx.table.ident.insert(new_id, me);
                        Ok(IdentRef { id: new_id })
                    }
                }
            })
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
/// Uniquely identifies an [`Ident`].
pub struct IdentRef {
    pub(crate) id: Id,
}

impl IdentRef {
    pub fn new(id: Id) -> Self {
        Self { id }
    }
}

impl fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)?;
        f.write_char(' ')
    }
}

#[cfg(test)]
mod test_parse_valid_idents {
    use crate::parse::{
        ident::Ident,
        table::ParseContext,
        utils::{Input, Parse},
    };

    fn test_inner(string: &str) {
        let mut input = Input::new(string);
        let mut ctx = ParseContext::new();
        Ident::parse(&mut input, &mut ctx).expect("failed to parse");
        assert!(input.is_empty());
        assert_eq!(string, ctx.table.ident.iter().next().unwrap().1.inner);
    }

    #[test]
    fn regressions() {
        test_inner("x");
        test_inner("function_call");
    }

    #[test]
    fn test_invalid_idents_do_not_parse() {
        let mut ctx = ParseContext::new();
        Ident::parse(&mut Input::new("////"), &mut ctx)
            .expect_err("this identifier should not be valid");
    }

    #[test]
    fn idents_are_not_incorrectly_duplicated() {
        let ctx = crate::parse::parse("x\nx").unwrap();
        assert_eq!(ctx.ident.len(), 1);
    }
}
