use crate::diagnostics::span::Span;
use std::fmt::{self, Write};

use crate::parse::utils::ParseError;

use super::utils::Parse;

#[derive(Debug, Eq, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Number(Number<'a>),
    Bool(bool),
}

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::String(string) => {
                f.write_char('"')?;
                string.fmt(f)?;
                f.write_char('"')
            }
            Literal::Number(number) => number.fmt(f),
            Literal::Bool(b) => f.write_str(if *b { "True" } else { "False" }),
        }
    }
}

impl<'a> Parse<'a> for Literal<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, ParseError> {
        input.skip_whitespace()?;
        if input.starts_with('"') {
            input.parse_token("\"")?;
            let string = input.eat_until(|char| char == '"')?;
            input.parse_token("\"")?;
            Ok(Self::String(string))
        } else if let Some('0'..='9') = input.chars().next() {
            Ok(Self::Number(Number::parse(input)?))
        } else if input.starts_with("True") {
            input.parse_token("True")?;
            Ok(Self::Bool(true))
        } else if input.starts_with("False") {
            input.parse_token("False")?;
            Ok(Self::Bool(false))
        } else {
            Err(ParseError::UnexpectedToken {
                explanation: "Expected a literal (e.g. a string or a boolean or an integer) here."
                    .to_string(),
                span: Span::new(*input.position(), *input.position()).into(),
            })
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Number<'a> {
    pub(crate) int: &'a str,
    pub(crate) float: Option<&'a str>,
    pub(crate) exp: Option<&'a str>,
}

impl Number<'_> {
    pub(crate) fn as_int(&self) -> i32 {
        let int_part = self.int.parse::<i32>().unwrap();
        if let Some(exp) = self.exp {
            i32::pow(int_part, exp.parse::<u32>().unwrap())
        } else {
            int_part
        }
    }
}

impl fmt::Display for Number<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.int.fmt(f)?;
        if let Some(float) = self.float {
            f.write_char('.')?;
            float.fmt(f)?;
        }
        if let Some(exp) = self.exp {
            f.write_char('e')?;
            exp.fmt(f)?;
        }
        Ok(())
    }
}

impl<'a> Parse<'a> for Number<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError> {
        enum State {
            Int,
            Float(usize),
            Exp(usize),
            End,
        }

        let mut ret = Self {
            int: "",
            float: None,
            exp: None,
        };

        let mut state = State::Int;

        let mut index = 0;
        let mut nth = 0;

        loop {
            state = match state {
                State::Int => match input.peek_nth(nth) {
                    num @ Some('0'..='9') => {
                        let num = num.unwrap();
                        index += num.len_utf8();
                        nth += 1;
                        ret.int = input.get(0..index).unwrap();
                        State::Int
                    }
                    num @ Some('.') => {
                        let num = num.unwrap();
                        index += num.len_utf8();
                        nth += 1;
                        State::Float(index)
                    }
                    num @ Some('e') => {
                        let num = num.unwrap();
                        index += num.len_utf8();
                        nth += 1;
                        State::Exp(index)
                    }
                    Some(_) | None => State::End,
                },
                State::Float(start_index) => match input.peek_nth(nth) {
                    num @ Some('0'..='9') => {
                        let num = num.unwrap();
                        index += num.len_utf8();
                        nth += 1;
                        if let Some(ref mut float) = ret.float {
                            *float = input.get(start_index..index).unwrap();
                        } else {
                            ret.float = Some(input.get(start_index..index).unwrap());
                        }
                        State::Float(start_index)
                    }
                    num @ Some('e') => {
                        let num = num.unwrap();
                        index += num.len_utf8();
                        nth += 1;
                        State::Exp(index)
                    }
                    Some(_) | None => State::End,
                },
                State::Exp(start_index) => match input.peek_nth(nth) {
                    num @ Some('0'..='9') => {
                        let num = num.unwrap();
                        index += num.len_utf8();
                        nth += 1;
                        if let Some(ref mut exp) = ret.exp {
                            *exp = input.get(start_index..index).unwrap();
                        } else {
                            ret.exp = Some(input.get(start_index..index).unwrap());
                        }
                        State::Exp(start_index)
                    }
                    Some(_) | None => State::End,
                },
                State::End => break,
            }
        }

        if ret.int.is_empty() {
            return Err(ParseError::UnexpectedEndOfInput);
        }

        input.advance_n(nth)?;

        Ok(ret)
    }
}

#[cfg(test)]
mod test_literals {
    use crate::parse::utils::{Input, Parse};

    use super::Literal;

    fn inner(expression: impl ToString, should_parse: bool) {
        let inner = expression.to_string();
        let mut exp = Input::new(&inner);
        match Literal::parse(&mut exp) {
            Ok(t) => {
                if !should_parse {
                    panic!("This input should not have parsed, but it did: {:#?}", t);
                }
                let inner = t.to_string();
                let mut out = Input::new(&inner);
                let res = Literal::parse(&mut out).unwrap();
                assert_eq!(t, res);
            }
            Err(e) => {
                if should_parse {
                    panic!("The input should have parsed, but it did not {:#?}", e);
                }
            }
        }
    }

    #[test]
    fn regression_1() {
        inner("1.2e5", true);
    }

    #[test]
    fn regression_2() {
        inner("\"\"", true);
    }

    #[test]
    fn regression_3() {
        inner("\"abc\"", true);
    }

    #[test]
    fn regression_4() {
        inner("", false);
    }

    #[test]
    fn regression_5() {
        inner("\" aaaaa", false);
    }
}
