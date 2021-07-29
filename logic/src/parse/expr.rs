use std::fmt::{self, Write};

use super::{
    ident::Ident,
    utils::{Input, Parse, ParseError},
};

#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'a> {
    Ident(Ident<'a>),
    BinOp(BinOp, Box<Expr<'a>>, Box<Expr<'a>>),
}

impl<'a> Parse<'a> for Expr<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        Self::parse_bp(input, 0)
    }
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ident(ident) => ident.fmt(f),
            Expr::BinOp(operator, left, right) => {
                left.fmt(f)?;
                f.write_char(' ')?;
                operator.fmt(f)?;
                f.write_char(' ')?;
                right.fmt(f)
            }
        }
    }
}

impl<'a> Expr<'a> {
    fn parse_bp(input: &mut Input<'a>, min_bp: u8) -> Result<Self, ParseError<'a>> {
        input.skip_whitespace()?;
        let mut lhs = {
            if let Ok(_) = Ident::parse(&mut *input) {
                let ident = Ident::parse(input)?;

                Self::Ident(ident)
            } else if let Ok(_) = Op::parse(input, true) {
                return Err(ParseError::UnexpectedToken {
                    token: "",
                    explanation: "unary operators are not yet supported".to_string(),
                });
            } else {
                return Err(ParseError::__NonExhaustive);
            }
        };

        loop {
            input.skip_whitespace()?;
            if input.is_empty() {
                break;
            }
            let op = Op::parse(input, false)?;

            let (left_bp, right_bp) = op.bp();

            if left_bp < right_bp {
                break;
            }

            lhs = {
                let rhs = Self::parse_bp(input, right_bp)?;
                Self::BinOp(op.try_into_bin_op().unwrap(), Box::new(lhs), Box::new(rhs))
            };
            continue;
        }

        Ok(lhs)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
    BinOp(BinOp),
}

impl Op {
    fn parse<'a>(input: &mut Input<'a>, _: bool) -> Result<Self, ParseError<'a>> {
        input.skip_whitespace()?;
        Ok(match input.advance_one()? {
            "+" => Op::BinOp(BinOp::Add),
            "-" => Op::BinOp(BinOp::Subtract),
            "/" => Op::BinOp(BinOp::Divide),
            "*" => Op::BinOp(BinOp::Multiply),
            token => {
                return Err(ParseError::UnexpectedToken {
                    token,
                    explanation: format!(
                        "Expected an operator here. Instead we found `{}`.",
                        token
                    ),
                })
            }
        })
    }

    fn bp(&self) -> (u8, u8) {
        match self {
            Op::BinOp(op) => op.bp(),
        }
    }

    pub fn try_into_bin_op(self) -> Result<BinOp, Self> {
        if let Self::BinOp(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    SetEquals,
}

impl BinOp {
    fn bp(&self) -> (u8, u8) {
        match self {
            BinOp::Add | BinOp::Subtract => (5, 6),
            BinOp::Divide | BinOp::Multiply => (7, 8),
            BinOp::SetEquals => (2, 1),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            BinOp::Add => "+",
            BinOp::Subtract => "-",
            BinOp::Divide => "/",
            BinOp::Multiply => "*",
            BinOp::SetEquals => "=",
        })
    }
}
