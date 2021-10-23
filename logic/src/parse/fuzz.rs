use fuzzcheck::{
    alternation, concatenation, literal,
    mutators::grammar::{ASTMutator, AST},
    recurse, recursive, repetition, SerdeSerializer,
};

use crate::parse::expr;

use super::parse;

fn run_test(input: &str) -> bool {
    if let Ok(ast) = parse(input) {
        let out = ast.to_string();
        let reconstructed_ast = if let Ok(ast) = parse(&out) {
            ast
        } else {
            return false;
        };
        ast == reconstructed_ast
    } else {
        false
    }
}

#[test]
fn fuzz_parser() {
    let ident = concatenation! {
        alternation! {
            // this range chosen to avoid collisions with keywords
            literal!('a'..='h'),
            literal!('A'..='Z')
        },
        repetition! {
            alternation! {
                // this range chosen to avoid collisions with keywords
                literal!('a'..='h'),
                literal!('A'..='Z'),
                literal!('_')
            },
            1..50
        }
    };

    let string = concatenation! {
        literal!('"'),
        ident.clone(),
        literal!('"')
    };

    let number = repetition! {
        literal!('0'..='9'),
        1..10
    };

    let float = concatenation! {
        number.clone(),
        literal!('.'),
        number.clone(),
        literal!('e'),
        number.clone()
    };

    let expression = recursive! {
        e in alternation! {
            ident.clone(),
            string.clone(),
            number.clone(),
            float.clone(),
            // function calls
            concatenation! {
                ident.clone(),
                literal!('('),
                repetition! {
                    concatenation! {
                        recurse!(e),
                        literal!(',')
                    },
                    1..50
                },
                literal!(')')
            },
            concatenation! {
                literal!('('),
                recurse!(e),
                literal!(')')
            },
            concatenation! {
                literal!('('),
                recurse!(e),
                alternation! {
                    literal!('+'),
                    literal!('-'),
                    literal!('*'),
                    literal!('/')
                },
                recurse!(e),
                literal!(')')
            },
            concatenation! {
                alternation! {
                    literal!('+'),
                    literal!('-')
                },
                literal!('('),
                recurse!(e),
                literal!(')')
            }
        }
    };

    let statement = alternation! {
        concatenation! {
            expression.clone(),
            literal!('\n')
        },
        concatenation! {
            ident.clone(),
            literal!('='),
            expression.clone(),
            literal!('\n')
        },
        concatenation! {
            literal!('r'),
            literal!('e'),
            literal!('t'),
            literal!('u'),
            literal!('r'),
            literal!('n'),
            literal!(' '),
            expression.clone(),
            literal!('\n')
        }
    };

    let for_loop_ident = concatenation! {
        literal!('i')
    };

    let indented_statement = repetition! {
        concatenation! {
            literal!(' '),
            literal!(' '),
            statement.clone()
        },
        1..16
    };

    let for_loop = concatenation! {
        concatenation! {
            literal!('f'),
            literal!('o'),
            literal!('r'),
            literal!(' '),
            for_loop_ident.clone()
        },
        concatenation! {
            literal!('='),
            literal!(' ')
        },
        expression.clone(),
        concatenation! {
            literal!(' '),
            literal!('t'),
            literal!('o'),
            literal!(' ')
        },
        expression.clone(),
        concatenation! {
            literal!(' '),
            literal!('s'),
            literal!('t'),
            literal!('e'),
            literal!('p'),
            literal!(' ')
        },
        expression.clone(),
        literal!('\n'),
        indented_statement.clone(),
        literal!('n'),
        literal!('e'),
        literal!('x'),
        literal!('t'),
        literal!(' '),
        for_loop_ident,
        literal!('\n')
    };

    let while_loop = concatenation! {
        literal!('w'),
        literal!('h'),
        literal!('i'),
        literal!('l'),
        literal!('e'),
        literal!(' '),
        expression.clone(),
        literal!('\n'),
        indented_statement.clone(),
        literal!('e'),
        literal!('n'),
        literal!('d'),
        literal!('w'),
        literal!('h'),
        literal!('i'),
        literal!('l'),
        literal!('e'),
        literal!('\n')
    };

    let function = concatenation! {
        literal!('f'),
        literal!('u'),
        literal!('n'),
        literal!('c'),
        literal!('t'),
        literal!('i'),
        literal!('o'),
        literal!('n'),
        literal!(' '),
        ident.clone(),
        literal!('('),
        repetition! {
            concatenation! {
                ident.clone(),
                literal!(',')
            },
            1..50
        },
        literal!(')'),
        literal!('\n'),
        indented_statement.clone(),
        literal!('e'),
        literal!('n'),
        literal!('d'),
        literal!('f'),
        literal!('u'),
        literal!('n'),
        literal!('c'),
        literal!('t'),
        literal!('i'),
        literal!('o'),
        literal!('n'),
        literal!('\n')
    };

    let if_statement = concatenation! {
        literal!('i'),
        literal!('f'),
        literal!(' '),
        expression.clone(),
        literal!('t'),
        literal!('h'),
        literal!('e'),
        literal!('n'),
        literal!('\n'),
        indented_statement.clone(),
        repetition! {
            concatenation! {
                literal!('e'),
                literal!('l'),
                literal!('s'),
                literal!('e'),
                literal!('i'),
                literal!('f'),
                literal!(' '),
                expression.clone(),
                literal!('t'),
                literal!('h'),
                literal!('e'),
                literal!('n'),
                literal!('\n'),
                indented_statement.clone()
            },
            0..32
        },
        repetition! {
            concatenation! {
                literal!('e'),
                literal!('l'),
                literal!('s'),
                literal!('e'),
                literal!('\n'),
                indented_statement.clone()
            },
            0..=1
        },
        literal!('e'),
        literal!('n'),
        literal!('d'),
        literal!('i'),
        literal!('f')
    };

    let block = alternation! {
        for_loop.clone(),
        while_loop.clone(),
        function.clone(),
        if_statement
    };

    let ast = repetition! {
        alternation! {
            block,
            statement
        },
        0..100
    };

    let ast_mutator = ASTMutator::from_grammar(ast);
    let with_string = ast_mutator.with_string();

    let _ = fuzzcheck::fuzz_test(|(_, string): &(AST, String)| run_test(string))
        .mutator(with_string)
        .serializer(SerdeSerializer::default())
        .default_sensor()
        .default_pool()
        .arguments_from_cargo_fuzzcheck()
        .stop_after_first_test_failure(true)
        .launch();
}
