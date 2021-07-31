use std::fmt;

use crate::diagnostics::span::Span;

use super::utils::{Parse, ParseError};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Ident<'a> {
    inner: &'a str,
    span: Span,
}

impl<'a> std::ops::Deref for Ident<'a> {
    type Target = &'a str;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a> Parse<'a> for Ident<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        input.skip_whitespace()?;
        let rec = input.start_recording();
        input
            .eat_until_or_end(|char| !char.is_alphanumeric())
            .and_then(|inner| {
                if inner.is_empty() {
                    Err(ParseError::UnexpectedEndOfInput)
                } else if !inner.chars().next().unwrap().is_alphabetic() {
                    println!("failed to parse ident");
                    Err(ParseError::__NonExhaustive)
                } else {
                    Ok(Self {
                        inner,
                        span: rec.finish_recording(input),
                    })
                }
            })
    }
}

impl fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

#[cfg(test)]
mod test_parse_valid_idents {
    #[cfg(feature = "_proptest")]
    use proptest::prelude::*;

    use crate::parse::{
        ident::Ident,
        utils::{Input, Parse},
    };

    #[cfg(feature = "_proptest")]
    proptest! {
        #[test]
        fn test(input in "[a-zA-Z0-9]+") {
            test_inner(&input)
        }
    }

    fn test_inner(string: &str) {
        let mut input = Input::new(string);
        let ident = Ident::parse(&mut input).expect("failed to parse");
        assert!(input.is_empty());
        assert_eq!(string, ident.inner);
    }

    #[test]
    fn regressions() {
        test_inner("x");
    }

    #[test]
    fn test_invalid_idents_do_not_parse() {
        Ident::parse(&mut Input::new("////")).expect_err("this identifier should not be valid");
    }
}
