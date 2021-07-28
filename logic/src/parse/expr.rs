use super::{
    ident::Ident,
    utils::{Input, Parse, ParseError},
};

pub enum Expr<'a> {
    Ident(Ident<'a>),
    BinOp(BinOp, Box<Expr<'a>>, Box<Expr<'a>>),
}

impl<'a> Parse<'a> for Expr<'a> {
    fn parse(input: &mut super::utils::Input<'a>) -> Result<Self, super::utils::ParseError<'a>> {
        Self::parse_bp(input, 0)
    }
}

impl<'a> Expr<'a> {
    fn parse_bp(input: &mut Input<'a>, min_bp: u8) -> Result<Self, ParseError<'a>> {
        input.skip_whitespace()?;
        let char = input.peek_char().ok_or(ParseError::UnexpectedEndOfInput)?;
        let mut lhs = match char {
            '(' => {
                input.advance_one()?;
                let lhs = Self::parse_bp(input, 0)?;
                input.skip_whitespace()?;
                input.parse_token(")")?;
                Some(lhs)
            }
            '[' => {
                todo!()
            }
            _ => None,
        };

        loop {
            let op = Op::parse(input, lhs.is_some())?;

            let (l_bp, r_bp) = op.bp();

            if l_bp < min_bp {
                break;
            }

            let rhs = Self::parse_bp(input, r_bp)?;

            lhs = match op {
                Op::BinOp(bin_op) => Some(Expr::BinOp(
                    bin_op,
                    Box::new(lhs.ok_or(ParseError::UnexpectedEndOfInput)?),
                    Box::new(rhs),
                )),
            };
        }

        lhs.ok_or(ParseError::UnexpectedEndOfInput)
    }
}

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
}

pub enum BinOp {
    Add,
    Subtract,
    Divide,
    Multiply,
}

impl BinOp {
    fn bp(&self) -> (u8, u8) {
        match self {
            BinOp::Add | BinOp::Subtract => (5, 6),
            BinOp::Divide | BinOp::Multiply => (7, 8),
        }
    }
}
