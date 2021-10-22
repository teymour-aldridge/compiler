use fuzzcheck::{
    alternation, concatenation, literal,
    mutators::grammar::{ASTMutator, AST},
    recurse, recursive, repetition, SerdeSerializer,
};

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
    let ident = repetition! {
        literal!('a'..='z'),
        0..50
    };

    let string = concatenation! {
        literal!('"'),
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
                    literal!('+')
                },
                recurse!(e),
                literal!(')')
            },
            concatenation! {
                alternation! {
                    literal!('+'),
                    literal!('+')
                },
                literal!('('),
                recurse!(e),
                literal!(')')
            }
        }
    };
    let ast = repetition! {
        alternation! {
            concatenation! {
                expression,
                literal!('\n')
            }
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
