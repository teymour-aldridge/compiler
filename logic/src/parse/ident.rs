use crate::diagnostics::span::Span;

use super::utils::Parse;

#[derive(Debug, Copy, Clone)]
pub struct Ident<'a> {
    inner: &'a str,
    span: Span,
}

impl<'a> Parse<'a> for Ident<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        let rec = input.start_recording();
        input
            .eat_until_or_end(|char| !char.is_alphanumeric())
            .map(|inner| Self {
                inner,
                span: rec.finish_recording(input),
            })
    }
}

#[cfg(test)]
mod test_parse_valid_idents {
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
}
