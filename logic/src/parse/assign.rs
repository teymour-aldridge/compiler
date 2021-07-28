use std::fmt::{self, Write};

use super::{expr::Expr, ident::Ident, utils::Parse};

#[derive(Debug, PartialEq, Eq)]
pub struct Assignment<'a> {
    pub to: Ident<'a>,
    pub value: Expr<'a>,
}

impl<'a> Parse<'a> for Assignment<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        let to = Ident::parse(input)?;

        input.skip_whitespace()?;
        input.parse_token("=")?;
        input.skip_whitespace()?;
        let value = Expr::parse(input)?;

        Ok(Self { to, value })
    }
}

impl fmt::Display for Assignment<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to.fmt(f)?;
        f.write_char('=')?;
        self.value.fmt(f)
    }
}

#[cfg(test)]
mod test_simple_assignment {
    use proptest::prelude::*;

    use crate::parse::{
        assign::Assignment,
        utils::{Input, Parse},
    };
    #[cfg(feature = "_proptest")]
    proptest! {
    #[test]
        fn test(input in "[a-zA-Z0-9]+[ ]*=[ ]*[a-zA-Z0-9]+") {
            test_inner(&input);
        }
    }

    fn test_inner(input: &str) {
        let mut input = Input::new(input);
        Assignment::parse(&mut input).expect("faild to parse the valid assignment");
        assert!(input.is_empty())
    }

    #[test]
    fn regressions() {
        test_inner("A=A");
    }
}
