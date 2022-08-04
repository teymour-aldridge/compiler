use std::{
    collections::BTreeMap,
    fmt::{self, Display},
};

use crate::diagnostics::span::{HasSpan, IndexOnlySpan, Span, Spanned};

use super::{
    ident::{Ident, IdentRef},
    lit::Literal,
    table::{Id, ItemKind, ItemRef, ParseContext, ParseTable},
    utils::{Input, Parse, ParseError},
};

#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'i> {
    Ident(IdentRef),
    Literal(Spanned<Literal<'i>>),
    BinOp(Spanned<BinOp>, ExprRef, ExprRef),
    UnOp(Spanned<UnOp>, ExprRef),
    FunctionCall(IdentRef, Vec<ExprRef>),
    Constructor(Constructor),
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone)]
pub struct ExprRef {
    pub(crate) id: Id,
}

impl From<ExprRef> for ItemRef {
    fn from(e: ExprRef) -> Self {
        ItemRef {
            id: e.id,
            item_kind: ItemKind::Expr,
        }
    }
}
impl Expr<'_> {
    pub fn as_bin_op(&self) -> Option<(&BinOp, &ExprRef, &ExprRef)> {
        if let Self::BinOp(op, a, b) = self {
            Some((op, a, b))
        } else {
            None
        }
    }

    pub fn as_un_op(&self) -> Option<(&UnOp, &ExprRef)> {
        if let Self::UnOp(op, a) = self {
            Some((op, a))
        } else {
            None
        }
    }

    pub fn as_constructor(&self) -> Option<&Constructor> {
        if let Self::Constructor(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_ident(&self) -> Option<&IdentRef> {
        if let Self::Ident(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_literal(&self) -> Option<&Spanned<Literal<'_>>> {
        if let Self::Literal(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the expr is [`Ident`].
    ///
    /// [`Ident`]: Expr::Ident
    #[must_use]
    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Ident(..))
    }

    /// Returns `true` if the expr is [`FunctionCall`].
    ///
    /// [`FunctionCall`]: Expr::FunctionCall
    #[must_use]
    pub fn is_function_call(&self) -> bool {
        matches!(self, Self::FunctionCall(..))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Constructor {
    pub(crate) name: IdentRef,
    /// The fields of the constructor.
    ///
    /// todo: the parser should always sort fields alphabetically to ensure correctness in later
    /// stages of the compiler
    pub(crate) fields: BTreeMap<IdentRef, ExprRef>,
    pub(crate) span: Span,
}

impl HasSpan for Constructor {
    fn span<'i>(&self, _: &'i ParseTable<'i>) -> Span {
        self.span
    }
}

impl<'i> Parse<'i> for Constructor {
    type Context = ParseContext<'i>;
    type Output = Self;

    fn parse(input: &mut Input<'i>, ctx: &mut ParseContext<'i>) -> Result<Self, ParseError> {
        let recording = input.start_recording();
        let name = Ident::parse(input, ctx)?;
        input.skip_whitespace()?;
        input.parse_token("{")?;
        let mut fields = vec![];
        loop {
            input.skip_whitespace()?;
            if input.starts_with('}') {
                break;
            }

            let field_name = Ident::parse(input, ctx)?;
            input.skip_whitespace()?;
            input.parse_token(":")?;
            input.skip_whitespace()?;
            let expr = Expr::parse_bp_stop_if(
                input,
                0,
                |input| input.starts_with(',') || input.starts_with('}'),
                ctx,
            )?
            .unwrap();
            input.skip_whitespace()?;
            if input.starts_with(',') {
                input.parse_token(",")?;
            }

            fields.push((field_name, expr));
        }
        input.parse_token("}")?;

        let span = recording.finish_recording(input);

        Ok(Constructor {
            name,
            fields: fields.into_iter().collect(),
            span,
        })
    }
}

impl<'i> Parse<'i> for Expr<'i> {
    type Output = ExprRef;
    type Context = ParseContext<'i>;

    fn parse(
        input: &mut super::utils::Input<'i>,
        ctx: &mut ParseContext<'i>,
    ) -> Result<ExprRef, super::utils::ParseError> {
        Self::parse_bp(input, 0, ctx).and_then(|op| {
            op.ok_or(ParseError::ExprError {
                span: Span::new(*input.position(), *input.position()).into(),
                explanation: "Error parsing this expression.".to_string(),
            })
        })
    }
}

impl<'i> Expr<'i> {
    pub(crate) fn parse_bp(
        input: &mut Input<'i>,
        min_bp: u8,
        ctx: &mut ParseContext<'i>,
    ) -> Result<Option<ExprRef>, ParseError> {
        Self::parse_bp_stop_if(input, min_bp, |_| false, ctx)
    }

    pub(crate) fn parse_bp_stop_if(
        input: &mut Input<'i>,
        min_bp: u8,
        stop_if: impl Fn(&str) -> bool + Copy,
        ctx: &mut ParseContext<'i>,
    ) -> Result<Option<ExprRef>, ParseError> {
        if min_bp == u8::MAX {
            return Ok(None);
        }

        input.skip_whitespace()?;
        let mut lhs = {
            if input.starts_with('(') {
                let opening_bracket_span = input.start_recording();
                input.advance_one()?;
                let opening_bracket_span = opening_bracket_span.finish_recording(input);
                let expr = Self::parse_bp_stop_if(input, 0, stop_if, ctx)?;
                input.skip_whitespace()?;
                if input.starts_with(')') {
                    input.advance_one()?;
                    expr
                } else {
                    // todo: make a proper error about mismatched brackets
                    return if !input.is_empty() {
                        let mut input2 = *input;
                        let closing_bracket_span = input2.start_recording();
                        input2.advance_one()?;
                        let closing_bracket_span = closing_bracket_span.finish_recording(&input2);
                        Err(ParseError::MismatchedBrackets {
                            opening_span: IndexOnlySpan::from(opening_bracket_span),
                            expected_closing_span: Some(IndexOnlySpan::from(closing_bracket_span)),
                        })
                    } else {
                        Err(ParseError::MismatchedBrackets {
                            opening_span: IndexOnlySpan::from(opening_bracket_span),
                            expected_closing_span: None,
                        })
                    };
                }
            } else if Ident::parse(&mut input.clone(), ctx).is_ok() {
                let is_constructor = {
                    let mut peek = *input;
                    Ident::parse(&mut peek, ctx)?;
                    peek.skip_whitespace()?;
                    peek.starts_with('{')
                };

                if is_constructor {
                    Some(Self::Constructor(Constructor::parse(input, ctx)?)).map(|expr| {
                        let id = ctx.new_id();
                        ctx.table.expr.insert(id, expr);
                        ExprRef { id }
                    })
                } else {
                    let ident = Ident::parse(input, ctx)?;

                    input.skip_whitespace()?;

                    if let Some('(') = input.peek_char() {
                        fn parse<'i>(
                            input: &mut Input<'i>,
                            ctx: &mut ParseContext<'i>,
                        ) -> Result<ExprRef, ParseError> {
                            Expr::parse_bp_stop_if(
                                input,
                                0,
                                |input| input.starts_with(')') || input.starts_with(','),
                                ctx,
                            )
                            .and_then(|ok| ok.ok_or(ParseError::__NonExhaustive))
                        }

                        input.parse_token("(")?;

                        let args = if !input.starts_with(')') {
                            input.delimited_list(parse, ')', ",", ctx)?
                        } else {
                            vec![]
                        };
                        input.parse_token(")")?;
                        Some(Self::FunctionCall(ident, args)).map(|expr| {
                            let id = ctx.new_id();
                            ctx.table.expr.insert(id, expr);
                            ExprRef { id }
                        })
                    } else {
                        Some(Self::Ident(ident)).map(|expr| {
                            let id = ctx.new_id();
                            ctx.table.expr.insert(id, expr);
                            ExprRef { id }
                        })
                    }
                }
            } else if Literal::parse(&mut input.clone(), ctx).is_ok() {
                let rec = input.start_recording();
                let lit = Literal::parse(input, ctx)?;

                Some(Self::Literal(Spanned::new(
                    rec.finish_recording(input),
                    lit,
                )))
                .map(|expr| {
                    let id = ctx.new_id();
                    ctx.table.expr.insert(id, expr);
                    ExprRef { id }
                })
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
                || input.starts_with(']')
            {
                break;
            }

            let op = match Op::parse(&mut input.clone(), lhs.is_none()) {
                Ok(op) => op,
                Err(err) => match err {
                    ParseError::UnexpectedEndOfInput { span: _ } => break,
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

            if right_bp == u8::MAX {
                let rhs = Expr::parse_bp_stop_if(input, 0, stop_if, ctx)?;
                input.parse_token("]")?;
                lhs = Some(Self::BinOp(
                    Spanned::new(op_span, op.try_into_bin_op().unwrap()),
                    lhs.unwrap(),
                    rhs.unwrap(),
                ))
                .map(|expr| {
                    let id = ctx.new_id();
                    ctx.table.expr.insert(id, expr);
                    ExprRef { id }
                });
            } else {
                let rhs = Self::parse_bp_stop_if(input, right_bp, stop_if, ctx)?;

                if op.is_bin_op() {
                    match (lhs, rhs) {
                        (Some(left), Some(right)) => {
                            lhs = Some(Self::BinOp(
                                Spanned::new(op_span, op.try_into_bin_op().unwrap()),
                                left,
                                right,
                            ))
                            .map(|expr| {
                                let id = ctx.new_id();
                                ctx.table.expr.insert(id, expr);
                                ExprRef { id }
                            });
                        }
                        _ => return Err(ParseError::__NonExhaustive),
                    }
                } else if let Some(rhs) = rhs {
                    lhs = Some(Self::UnOp(
                        Spanned::new(op_span, op.try_into_un_op().unwrap()),
                        rhs,
                    ))
                    .map(|expr| {
                        let id = ctx.new_id();
                        ctx.table.expr.insert(id, expr);
                        ExprRef { id }
                    })
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
    fn parse(input: &mut Input<'_>, prefix: bool) -> Result<Self, ParseError> {
        let rec = input.start_recording();
        input.skip_whitespace()?;
        Ok(match input.advance_one()? {
            "+" if !prefix => Op::BinOp(BinOp::Add),
            "-" if !prefix => Op::BinOp(BinOp::Subtract),
            "/" => Op::BinOp(BinOp::Divide),
            "*" if !prefix => Op::BinOp(BinOp::Multiply),
            "." => Op::BinOp(BinOp::Dot),
            "+" if prefix => Op::UnOp(UnOp::Positive),
            "-" if prefix => Op::UnOp(UnOp::Negative),
            "*" if prefix => Op::UnOp(UnOp::Deref),
            "[" => Op::BinOp(BinOp::Index),
            "=" => {
                if input.starts_with('=') {
                    input.advance_one()?;
                    Op::BinOp(BinOp::IsEqual)
                } else {
                    Op::BinOp(BinOp::SetEquals)
                }
            }
            "!" if input.starts_with('=') => {
                input.advance_one()?;
                Op::BinOp(BinOp::IsNotEqual)
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
    IsNotEqual,
    SetEquals,
    Dot,
    Index,
}

impl BinOp {
    fn bp(&self) -> (u8, u8) {
        match self {
            BinOp::Add | BinOp::Subtract => (5, 6),
            BinOp::Divide | BinOp::Multiply | BinOp::IsEqual | BinOp::Dot | BinOp::IsNotEqual => {
                (7, 8)
            }
            BinOp::SetEquals => (2, 1),
            BinOp::Index => (11, u8::MAX),
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
            BinOp::Dot => ".",
            BinOp::IsNotEqual => "!=",
            // this must be handled a level up
            BinOp::Index => panic!(),
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
    Positive,
    Negative,
    Deref,
}

impl UnOp {
    fn bp(&self) -> (u8, u8) {
        match self {
            UnOp::Positive | UnOp::Negative | UnOp::Deref => (99, 9),
        }
    }

    /// Returns `true` if the un op is [`Deref`].
    ///
    /// [`Deref`]: UnOp::Deref
    pub fn is_deref(&self) -> bool {
        matches!(self, Self::Deref)
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            UnOp::Positive => "+",
            UnOp::Negative => "-",
            UnOp::Deref => "*",
        })
    }
}

impl HasSpan for Expr<'_> {
    /// todo: tidy this up
    fn span<'i>(&self, table: &'i ParseTable<'i>) -> Span {
        match self {
            Expr::Ident(ident) => table.get_ident(*ident).span(table),
            Expr::Literal(lit) => lit.span(table),
            Expr::BinOp(_, left, right) => Span::new(
                table.get_expr(left).span(table).start(),
                table.get_expr(right).span(table).stop(),
            ),
            Expr::UnOp(op, expr) => Span::new(
                op.span(table).start(),
                table.get_expr(expr).span(table).stop(),
            ),
            Expr::FunctionCall(ident, params) => {
                // todo: include bracket span
                Span::new(
                    table.get_ident(*ident).span(table).start(),
                    params
                        .last()
                        .map(|last| table.get_expr(last).span(table).stop())
                        .unwrap_or_else(|| table.get_ident(*ident).span(table).stop()),
                )
            }
            Expr::Constructor(con) => {
                // todo: fix this
                table.get_ident(con.name).span(table)
            }
        }
    }
}
