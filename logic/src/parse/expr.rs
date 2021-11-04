use std::fmt::{self, Display, Write};

use crate::diagnostics::span::{IndexOnlySpan, Spanned};

use super::{
    ident::Ident,
    lit::Literal,
    utils::{Input, Parse, ParseError},
};

#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'a, IDENT = Ident<'a>> {
    Ident(IDENT),
    Literal(Spanned<Literal<'a>>),
    BinOp(Spanned<BinOp>, Box<Expr<'a, IDENT>>, Box<Expr<'a, IDENT>>),
    UnOp(Spanned<UnOp>, Box<Expr<'a, IDENT>>),
    FunctionCall(IDENT, Vec<Expr<'a, IDENT>>),
}

impl<'a> Parse<'a> for Expr<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError> {
        Self::parse_bp(input, 0).and_then(|op| op.ok_or(ParseError::__NonExhaustive))
    }
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_char('(')?;
        match self {
            Expr::Ident(ident) => ident.fmt(f),
            Expr::BinOp(operator, left, right) => {
                left.fmt(f)?;
                f.write_char(' ')?;
                operator.fmt(f)?;
                f.write_char(' ')?;
                right.fmt(f)
            }
            Expr::UnOp(op, left) => {
                op.fmt(f)?;
                f.write_char(' ')?;
                left.fmt(f)
            }
            Expr::Literal(lit) => lit.fmt(f),
            Expr::FunctionCall(name, args) => {
                name.fmt(f)?;
                f.write_char('(')?;
                for arg in args {
                    arg.fmt(f)?;
                    f.write_char(',')?;
                }
                f.write_char(')')
            }
        }?;
        f.write_char(')')
    }
}

impl<'a> Expr<'a> {
    pub(crate) fn parse_bp(input: &mut Input<'a>, min_bp: u8) -> Result<Option<Self>, ParseError> {
        Self::parse_bp_stop_if(input, min_bp, |_| false)
    }

    pub(crate) fn parse_bp_stop_if(
        input: &mut Input<'a>,
        min_bp: u8,
        stop_if: impl Fn(&str) -> bool + Copy,
    ) -> Result<Option<Self>, ParseError> {
        input.skip_whitespace()?;
        let mut lhs = {
            if input.starts_with('(') {
                input.advance_one()?;
                let expr = Self::parse_bp_stop_if(input, 0, stop_if)?;
                input.skip_whitespace()?;
                if input.starts_with(')') {
                    input.advance_one()?;
                    expr
                } else {
                    // todo: make a proper error about mismatched brackets
                    return Err(ParseError::UnexpectedEndOfInput);
                }
            } else if let Ok(_) = Ident::parse(&mut input.clone()) {
                let ident = Ident::parse(input)?;

                input.skip_whitespace()?;

                if let Some('(') = input.peek_char() {
                    fn parse<'a>(input: &mut Input<'a>) -> Result<Expr<'a>, ParseError> {
                        Expr::parse_bp_stop_if(input, 0, |input| {
                            input.starts_with(")") || input.starts_with(",")
                        })
                        .and_then(|ok| ok.ok_or(ParseError::__NonExhaustive))
                    }

                    input.parse_token("(")?;
                    let args = input.delimited_list(parse, ')', ",")?;
                    input.parse_token(")")?;
                    Some(Self::FunctionCall(ident, args))
                } else {
                    Some(Self::Ident(ident))
                }
            } else if let Ok(_) = Literal::parse(&mut input.clone()) {
                let rec = input.start_recording();
                let lit = Literal::parse(input)?;

                Some(Self::Literal(Spanned::new(
                    rec.finish_recording(input),
                    lit,
                )))
            } else {
                None
            }
        };

        loop {
            input.skip_whitespace()?;

            if input.is_empty()
                || input.starts_with('\n')
                || (stop_if)(&*input)
                || input.starts_with(')')
            {
                break;
            }

            let op = match Op::parse(&mut input.clone(), lhs.is_none()) {
                Ok(op) => op,
                Err(err) => match err {
                    ParseError::UnexpectedEndOfInput => break,
                    e => {
                        return Err(e);
                    }
                },
            };

            let (left_bp, right_bp) = op.bp();
            if left_bp < min_bp {
                break;
            }

            let rec = input.start_recording();
            Op::parse(input, false)?;
            let op_span = rec.finish_recording(input);

            let rhs = Self::parse_bp_stop_if(input, right_bp, stop_if)?;

            if op.is_bin_op() {
                match (lhs, rhs) {
                    (Some(left), Some(right)) => {
                        lhs = Some(Self::BinOp(
                            Spanned::new(op_span, op.try_into_bin_op().unwrap()),
                            Box::new(left),
                            Box::new(right),
                        ));
                    }
                    _ => return Err(ParseError::__NonExhaustive),
                }
            } else {
                if let Some(rhs) = rhs {
                    lhs = Some(Self::UnOp(
                        Spanned::new(op_span, op.try_into_un_op().unwrap()),
                        Box::new(rhs),
                    ))
                } else {
                    return Err(ParseError::__NonExhaustive);
                }
            }
        }

        Ok(lhs)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
    BinOp(BinOp),
    UnOp(UnOp),
}

impl Op {
    fn parse<'a>(input: &mut Input<'a>, prefix: bool) -> Result<Self, ParseError> {
        let rec = input.start_recording();
        input.skip_whitespace()?;
        Ok(match input.advance_one()? {
            "+" if !prefix => Op::BinOp(BinOp::Add),
            "-" if !prefix => Op::BinOp(BinOp::Subtract),
            "/" => Op::BinOp(BinOp::Divide),
            "*" => Op::BinOp(BinOp::Multiply),
            "+" if prefix => Op::UnOp(UnOp::Positive),
            "-" if prefix => Op::UnOp(UnOp::Negative),
            "=" => {
                if input.chars().next() == Some('=') {
                    input.advance_one()?;
                    Op::BinOp(BinOp::IsEqual)
                } else {
                    Op::BinOp(BinOp::SetEquals)
                }
            }
            token => {
                return Err(ParseError::UnexpectedToken {
                    explanation: format!(
                        "Expected an operator here. Instead we found `{}`.",
                        token
                    ),
                    span: {
                        let span = rec.finish_recording(input);
                        IndexOnlySpan::from(span)
                    },
                })
            }
        })
    }

    fn bp(&self) -> (u8, u8) {
        match self {
            Op::BinOp(op) => op.bp(),
            Op::UnOp(op) => op.bp(),
        }
    }

    pub fn try_into_bin_op(self) -> Result<BinOp, Self> {
        match self {
            Self::BinOp(v) => Ok(v),
            Op::UnOp(_) => Err(self),
        }
    }

    pub fn try_into_un_op(self) -> Result<UnOp, Self> {
        if let Self::UnOp(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    /// Returns `true` if the op is [`BinOp`].
    pub fn is_bin_op(&self) -> bool {
        matches!(self, Self::BinOp(..))
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinOp {
    Add,
    Subtract,
    Divide,
    Multiply,
    IsEqual,
    SetEquals,
}

impl BinOp {
    fn bp(&self) -> (u8, u8) {
        match self {
            BinOp::Add | BinOp::Subtract => (5, 6),
            BinOp::Divide | BinOp::Multiply | BinOp::IsEqual => (7, 8),
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
            BinOp::IsEqual => "==",
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
    Positive,
    Negative,
}

impl UnOp {
    fn bp(&self) -> (u8, u8) {
        match self {
            UnOp::Positive | UnOp::Negative => (99, 9),
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            UnOp::Positive => "+",
            UnOp::Negative => "-",
        })
    }
}
