//! Generates arbitrary valid inputs for the compiler. This is better than feeding raw inputs into
//! the compiler because many more of them are likely to advance beyond the parsing stage which
//! means that we can test more stages.

#![cfg_attr(feature = "fuzzcheck", feature(no_coverage))]

#[cfg(feature = "fuzzcheck")]
pub use inner::*;

#[cfg(feature = "fuzzcheck")]

mod inner {
    use std::fmt::{self, Display, Write};

    use fuzzcheck::mutators::map::MapMutator;
    use fuzzcheck::mutators::tuples::{Tuple2, Tuple2Mutator};
    use fuzzcheck::{
        make_mutator,
        mutators::{
            boxed::BoxMutator, option::OptionMutator, recursive::RecurToMutator,
            tuples::TupleMutatorWrapper, vector::VecMutator,
        },
        DefaultMutator, Mutator,
    };

    use serde::{Deserialize, Serialize};

    pub fn block_with_string_mutator() -> impl Mutator<(String, Block)> {
        MapMutator::new(
            Block::default_mutator(),
            |block_with_string: &(String, Block)| -> Option<Block> {
                Some(block_with_string.1.clone())
            },
            |block: &Block| -> (String, Block) { (block.to_string(), block.clone()) },
            |_, cplx| cplx,
        )
    }

    #[derive(DefaultMutator, Serialize, Deserialize, Debug, Clone)]
    pub struct Block {
        inner: Vec<Node>,
    }

    impl fmt::Display for Block {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.fmt(0, f)
        }
    }

    impl Block {
        fn fmt(&self, units: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for node in &self.inner {
                node.fmt(units, f)?;
                f.write_char('\n')?;
            }
            Ok(())
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Node {
        If {
            if_branch: (Expr, Vec<Node>),
            elseif_branches: Vec<(Expr, Vec<Node>)>,
            else_branch: Option<Vec<Node>>,
        },
        For {
            variable_of_indexation: Ident,
            start: Expr,
            stop: Expr,
            block: Vec<Node>,
        },
        Expr(Expr),
        While {
            condition: Expr,
            block: Vec<Node>,
        },
        Record {
            name: Ident,
            fields: Vec<(Ident, Ty)>,
        },
        Assignment {
            to: Ident,
            value: Box<Expr>,
        },
        Function {
            name: Ident,
            params: Vec<Ident>,
            block: Vec<Node>,
        },
    }

    make_mutator! {
        name: NodeMutator,
        recursive: true,
        default: true,
        type:
            pub enum Node {
                If {
                    #[field_mutator(
                            TupleMutatorWrapper<
                                    Tuple2Mutator<
                                            <Expr as DefaultMutator>::Mutator,
                                            VecMutator<
                                                    Node,
                                                    RecurToMutator<
                                                        NodeMutator<M1_0, M1_1, M1_2, M2_0, M3_0, M4_0, M4_1, M5_0, M5_1, M6_0, M6_1>
                                                        >
                                                >
                                        >,
                                    Tuple2<Expr, Vec<Node>>
                                >
                             = {
                            TupleMutatorWrapper::new(Tuple2Mutator::new(Expr::default_mutator(), VecMutator::new(self_.into(), 0..=usize::MAX)))
                        }
                    )]
                    if_branch: (Expr, Vec<Node>),
                    #[field_mutator(
                        VecMutator<
                            (Expr, Vec<Node>),
                            TupleMutatorWrapper<
                                    Tuple2Mutator<
                                        <Expr as DefaultMutator>::Mutator,
                                        VecMutator<
                                            Node,
                                            RecurToMutator<
                                                NodeMutator<M1_0, M1_1, M1_2, M2_0, M3_0, M4_0, M4_1, M5_0, M5_1, M6_0, M6_1>>
                                            >
                                        >,
                                        Tuple2<Expr, Vec<Node>
                                    >
                                >
                            > = {
                            VecMutator::new(
                                TupleMutatorWrapper::new(
                                    Tuple2Mutator::new(
                                        Expr::default_mutator(), VecMutator::new(self_.into(), 0..=usize::MAX)
                                    )
                                ),
                                0..=usize::MAX
                            )
                        }
                    )]

                    elseif_branches: Vec<(Expr, Vec<Node>)>,
                    #[field_mutator(
                        OptionMutator<Vec<Node>, VecMutator<Node, RecurToMutator<NodeMutator<M1_0, M1_1, M1_2, M2_0, M3_0, M4_0, M4_1, M5_0, M5_1, M6_0, M6_1>>>> = {
                            OptionMutator::new(VecMutator::new(self_.into(), 0..=usize::MAX))
                        }
                    )]

                    else_branch: Option<Vec<Node>>,
                },
                For {
                    variable_of_indexation: Ident,
                    start: Expr,
                    stop: Expr,
                    #[field_mutator(
                        VecMutator<Node, RecurToMutator<NodeMutator<M1_0, M1_1, M1_2, M2_0, M3_0, M4_0, M4_1, M5_0, M5_1, M6_0, M6_1>>> = {
                            VecMutator::new(self_.into(), 0..=usize::MAX)
                        }
                    )]
                    block: Vec<Node>,
                },
                Expr(Expr),
                While {
                    condition: Expr,
                    #[field_mutator(
                        VecMutator<Node, RecurToMutator<NodeMutator<M1_0, M1_1, M1_2, M2_0, M3_0, M4_0, M4_1, M5_0, M5_1, M6_0, M6_1>>> = {
                            VecMutator::new(self_.into(), 0..=usize::MAX)
                        }
                    )]
                    block: Vec<Node>,
                },
                Record {
                    name: Ident,
                    fields: Vec<(Ident, Ty)>,
                },
                Assignment {
                    to: Ident,
                    value: Box<Expr>,
                },
                Function {
                    name: Ident,
                    params: Vec<Ident>,
                    #[field_mutator(
                        VecMutator<Node, RecurToMutator<NodeMutator<M1_0, M1_1, M1_2, M2_0, M3_0, M4_0, M4_1, M5_0, M5_1, M6_0, M6_1>>> = {
                            VecMutator::new(self_.into(), 0..=usize::MAX)
                        }
                    )]
                    block: Vec<Node>,
                },

            }
    }

    fn fmt_block(block: &Vec<Node>, units: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for node in block {
            node.fmt(units, f)?;
            f.write_char('\n')?;
        }
        Ok(())
    }

    impl Node {
        fn fmt(&self, units: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Node::If {
                    if_branch,
                    elseif_branches,
                    else_branch,
                } => {
                    fmt_indent(units, f)?;

                    f.write_str("if ")?;
                    if_branch.0.fmt(None, f)?;
                    f.write_str(" then\n")?;
                    fmt_block(&if_branch.1, units + 2, f)?;

                    for (expr, block) in elseif_branches {
                        fmt_indent(units, f)?;
                        f.write_str("elseif ")?;
                        expr.fmt(None, f)?;
                        f.write_str(" then\n")?;

                        fmt_block(&block, units + 2, f)?;
                    }

                    if let Some(branch) = else_branch {
                        fmt_indent(units, f)?;
                        f.write_str("else\n")?;

                        fmt_block(branch, units + 2, f)?;
                    }

                    fmt_indent(units, f)?;
                    f.write_str("endif")?;

                    Ok(())
                }
                Node::Expr(e) => e.fmt(Some(units), f),

                Node::For {
                    variable_of_indexation,
                    start,
                    stop,
                    block,
                } => {
                    fmt_indent(units, f)?;
                    f.write_str("for ")?;
                    variable_of_indexation.fmt(f)?;
                    f.write_str(" = ")?;
                    start.fmt(None, f)?;
                    f.write_str(" to ")?;
                    stop.fmt(None, f)?;

                    f.write_char('\n')?;

                    fmt_block(block, units + 2, f)?;

                    fmt_indent(units, f)?;
                    f.write_str("next ")?;
                    variable_of_indexation.fmt(f)
                }
                Node::While { condition, block } => {
                    fmt_indent(units, f)?;
                    f.write_str("while ")?;
                    condition.fmt(None, f)?;

                    f.write_char('\n')?;

                    fmt_block(block, units + 2, f)?;

                    fmt_indent(units, f)?;
                    f.write_str("endwhile")
                }
                Node::Record { name, fields } => {
                    fmt_indent(units, f)?;
                    f.write_str("record ")?;
                    name.fmt(f)?;
                    f.write_char('\n')?;

                    for (name, ty) in fields {
                        fmt_indent(units + 2, f)?;
                        name.fmt(f)?;
                        f.write_str(" of ")?;
                        ty.fmt(f)?;
                        f.write_char('\n')?;
                    }
                    fmt_indent(units, f)?;
                    f.write_str("endrecord")
                }
                Node::Assignment { to, value } => {
                    fmt_indent(units, f)?;
                    to.fmt(f)?;
                    f.write_str(" = ")?;
                    value.fmt(None, f)
                }
                Node::Function {
                    name,
                    params,
                    block,
                } => {
                    fmt_indent(units, f)?;
                    f.write_str("function ")?;
                    name.fmt(f)?;
                    f.write_str(" (")?;
                    for param in params {
                        param.fmt(f)?;
                        f.write_char(',')?;
                    }
                    f.write_str(")\n")?;
                    fmt_block(block, units + 2, f)?;
                    fmt_indent(units, f)?;
                    f.write_str("endfunction")
                }
            }
        }
    }

    #[derive(DefaultMutator, Serialize, Deserialize, Debug, Copy, Clone)]

    pub enum Ty {
        String,
        Int,
        Bool,
    }

    impl Ty {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let symbol = match self {
                Ty::String => "String",
                Ty::Int => "Int",
                Ty::Bool => "Bool",
            };
            f.write_str(symbol)
        }
    }

    #[derive(DefaultMutator, Serialize, Deserialize, Debug, Copy, Clone)]
    pub enum UnOp {
        Positive,
        Negative,
    }

    impl UnOp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let symbol = match self {
                UnOp::Positive => "+",
                UnOp::Negative => "-",
            };
            f.write_str(symbol)
        }

        fn is_prefix(&self) -> bool {
            true
        }
    }

    #[derive(DefaultMutator, Serialize, Deserialize, Debug, Copy, Clone)]

    pub enum BinOp {
        Add,
        Subtract,
        Divide,
        Multiply,
        Dot,
        IsEqual,
        IsNotEqual,
    }

    impl BinOp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let symbol = match self {
                BinOp::Add => "+",
                BinOp::Subtract => "-",
                BinOp::Divide => "/",
                BinOp::Multiply => "*",
                // note: `SetEquals` is handled separately
                BinOp::IsEqual => "==",
                BinOp::Dot => ".",
                BinOp::IsNotEqual => "!=",
            };
            f.write_str(symbol)
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Expr {
        BinOp {
            op: BinOp,
            left: Box<Expr>,
            right: Box<Expr>,
        },
        UnOp {
            operator: UnOp,
            operand: Box<Expr>,
        },
        Literal(Literal),
        Ident(Ident),
        Call {
            name: Ident,
            args: Vec<Expr>,
        },
        Constructor {
            name: Ident,
            fields: Vec<(Ident, Box<Expr>)>,
        },
    }

    make_mutator! {
        name: ExprMutator,
        default: true,
        recursive: true,
        type:
            pub enum Expr {
                BinOp {
                    op: BinOp,
                    #[field_mutator(
                        BoxMutator<RecurToMutator<ExprMutator<M0_0, M1_0, M2_0, M3_0, M4_0, M5_0>>> = { BoxMutator::new(self_.into()) }
                    )]
                    left: Box<Expr>,
                    #[field_mutator(
                        BoxMutator<RecurToMutator<ExprMutator<M0_0, M1_0, M2_0, M3_0, M4_0, M5_0>>> = { BoxMutator::new(self_.into()) }
                    )]
                    right: Box<Expr>,
                },
                UnOp {
                    operator: UnOp,
                    #[field_mutator(
                        BoxMutator<RecurToMutator<ExprMutator<M0_0, M1_0, M2_0, M3_0, M4_0, M5_0>>> = { BoxMutator::new(self_.into()) }
                    )]
                    operand: Box<Expr>,
                },
                Literal(Literal),
                Ident(Ident),
                Call {
                    name: Ident,
                    #[field_mutator(
                        VecMutator<Expr, RecurToMutator<ExprMutator<M0_0, M1_0, M2_0, M3_0, M4_0, M5_0>>>
                            = { VecMutator::new(self_.into(), 0..=usize::MAX) }
                    )]
                    args: Vec<Expr>,
                },
                Constructor {
                    name: Ident,
                    #[field_mutator(
                        VecMutator<(Ident, Box<Expr>), TupleMutatorWrapper<Tuple2Mutator<<Ident as DefaultMutator>::Mutator, BoxMutator<RecurToMutator<ExprMutator<M0_0, M1_0, M2_0, M3_0, M4_0, M5_0>>>>, Tuple2<Ident, Box<Expr>>>>
                        = {
                            VecMutator::new(TupleMutatorWrapper::new(
                                Tuple2Mutator::new(Ident::default_mutator(), BoxMutator::new(self_.into()))
                            ), 0..=usize::MAX)
                        }
                    )]
                    fields: Vec<(Ident, Box<Expr>)>
                }
            }
    }

    impl Expr {
        fn fmt(&self, indent: Option<usize>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            if let Some(indent) = indent {
                fmt_indent(indent, f)?
            }
            match self {
                Expr::BinOp { op, left, right } => {
                    left.fmt(None, f)?;
                    op.fmt(f)?;
                    right.fmt(None, f)?;
                }
                Expr::UnOp { operator, operand } => {
                    if operator.is_prefix() {
                        operator.fmt(f)?;
                        operand.fmt(None, f)?;
                    } else {
                        operand.fmt(None, f)?;
                        operator.fmt(f)?;
                    }
                }
                Expr::Literal(lit) => lit.fmt(f)?,
                Expr::Ident(i) => i.fmt(f)?,
                Expr::Call { name, args } => {
                    name.fmt(f)?;
                    f.write_char('(')?;
                    for arg in args {
                        arg.fmt(None, f)?;
                        f.write_char(',')?;
                    }
                    f.write_char(')')?;
                }
                Expr::Constructor { name, fields } => {
                    name.fmt(f)?;
                    f.write_str(" { ")?;
                    for (name, expression) in fields {
                        name.fmt(f)?;
                        f.write_str(": ")?;
                        expression.fmt(None, f)?;
                        f.write_char(',')?;
                    }
                    f.write_char('}')?;
                }
            };
            Ok(())
        }
    }

    #[derive(DefaultMutator, Serialize, Deserialize, Debug, Clone)]
    pub enum Literal {
        Number(Number),
        String(Ident),
        Boolean(bool),
    }

    impl Literal {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Literal::Number(number) => number.fmt(f),
                Literal::String(s) => {
                    f.write_char('"')?;
                    s.fmt(f)?;
                    f.write_char('"')
                }
                Literal::Boolean(boolean) => f.write_str(if *boolean { "True" } else { "False" }),
            }
        }
    }

    #[derive(DefaultMutator, Serialize, Deserialize, Debug, Copy, Clone)]
    pub enum Number {
        Integer(u64),
        // Float(f64),
    }

    impl Number {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Number::Integer(i) => i.fmt(f),
                // Number::Float(float) => float.fmt(f),
            }
        }
    }

    #[derive(DefaultMutator, Serialize, Deserialize, Debug, Clone)]
    pub struct Ident {
        start: Alphabetic,
        rest: Vec<Either>,
    }

    #[derive(DefaultMutator, Serialize, Deserialize, Debug, Clone)]
    pub enum Either {
        Alphabetic(Alphabetic),
        Number(u8),
    }

    impl fmt::Display for Either {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Either::Alphabetic(a) => a.fmt(f),
                Either::Number(n) => n.fmt(f),
            }
        }
    }

    #[derive(DefaultMutator, Serialize, Deserialize, Debug, Copy, Clone)]
    pub struct Alphabetic {
        uppercase: bool,
        inner: AlphabeticInner,
    }

    #[derive(DefaultMutator, Serialize, Deserialize, Debug, Copy, Clone)]
    pub enum AlphabeticInner {
        A,
        B,
        C,
        D,
        // no 'e' and 'f' because it prevents the fuzzer from generating keywords
        G,
        H,
        I,
        J,
        K,
        L,
        M,
        N,
        O,
        P,
        Q,
        R,
        S,
        T,
        U,
        V,
    }

    impl fmt::Display for Alphabetic {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let symbol = match self.inner {
                AlphabeticInner::A => 'a',
                AlphabeticInner::B => 'b',
                AlphabeticInner::C => 'c',
                AlphabeticInner::D => 'd',
                AlphabeticInner::G => 'g',
                AlphabeticInner::H => 'h',
                AlphabeticInner::I => 'i',
                AlphabeticInner::J => 'j',
                AlphabeticInner::K => 'k',
                AlphabeticInner::L => 'l',
                AlphabeticInner::M => 'm',
                AlphabeticInner::N => 'n',
                AlphabeticInner::O => 'o',
                AlphabeticInner::P => 'p',
                AlphabeticInner::Q => 'q',
                AlphabeticInner::R => 'r',
                AlphabeticInner::S => 's',
                AlphabeticInner::T => 't',
                AlphabeticInner::U => 'u',
                AlphabeticInner::V => 'v',
            };

            if self.uppercase {
                f.write_char(symbol.to_uppercase().next().unwrap())
            } else {
                f.write_char(symbol.to_lowercase().next().unwrap())
            }
        }
    }

    impl Ident {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.start.fmt(f)?;
            for each in &self.rest {
                each.fmt(f)?;
            }
            Ok(())
        }
    }

    fn fmt_indent(units: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for _ in 0..units {
            f.write_char(' ')?;
        }
        Ok(())
    }
}
