//! A fuzz test using Loïc Lecrenier's excellent Fuzzcheck library. It has been exceedingly useful
//! in detecting faults.
//!
//! This also doubles as a formal grammar for the language, and helps with finding input values that
//! are well-suited to being used for benchmarks.

use fuzzcheck::mutators::grammar::*;

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
    let ident = regex("[a-hA-Z][a-h0-9A-Z_]+");

    let string = concatenation([literal('"'), ident.clone(), literal('"')]);

    let number = regex("[0-9]+");

    let float = concatenation([
        number.clone(),
        literal('.'),
        number.clone(),
        literal('e'),
        number.clone(),
    ]);

    let expression = recursive(|e| {
        alternation([
            ident.clone(),
            string.clone(),
            number.clone(),
            float.clone(),
            // function calls
            concatenation([
                ident.clone(),
                literal('('),
                repetition(concatenation([recurse(e), literal(',')]), 1..),
                literal(')'),
            ]),
            // field accesses
            concatenation([
                ident.clone(),
                literal('.'),
                alternation([
                    // methods
                    concatenation([
                        ident.clone(),
                        literal('('),
                        regex("self,"),
                        repetition(concatenation([recurse(e), literal(',')]), 1..),
                        literal(')'),
                    ]),
                    // fields
                    ident.clone(),
                ]),
            ]),
            // expressions with brackets
            concatenation([literal('('), recurse(e), literal(')')]),
            // binary operators
            concatenation([
                literal('('),
                recurse(e),
                regex("[-+*/]"),
                recurse(e),
                literal(')'),
            ]),
            // unary operators
            concatenation([regex("[-+]"), literal('('), recurse(e), literal(')')]),
            // constructor
            concatenation([
                ident.clone(),
                literal('{'),
                repetition(
                    concatenation([ident.clone(), literal(':'), recurse(e), literal(',')]),
                    1..100,
                ),
                literal('}'),
            ]),
        ])
    });

    let statement = concatenation([
        alternation([
            expression.clone(),
            concatenation([ident.clone(), literal('='), expression.clone()]),
            concatenation([regex("return "), expression.clone()]),
        ]),
        literal('\n'),
    ]);

    let for_loop_ident = literal('i');

    let indented_statements = repetition(concatenation([regex("[\n]+  "), statement.clone()]), 1..);

    let for_loop = concatenation([
        regex("for "),
        for_loop_ident.clone(),
        regex("= "),
        expression.clone(),
        regex(" to "),
        expression.clone(),
        regex(" step "),
        expression.clone(),
        literal('\n'),
        indented_statements.clone(),
        regex("next "),
        for_loop_ident,
        literal('\n'),
    ]);

    let while_loop = concatenation([
        regex("while "),
        expression.clone(),
        literal('\n'),
        indented_statements.clone(),
        regex("endwhile\n"),
    ]);

    let function = concatenation([
        regex("function "),
        ident.clone(),
        literal('('),
        repetition(concatenation([ident.clone(), literal(',')]), 1..),
        literal(')'),
        literal('\n'),
        indented_statements.clone(),
        regex("endfunction\n"),
    ]);

    let if_statement = concatenation([
        regex("if "),
        expression.clone(),
        regex(" then\n"),
        indented_statements.clone(),
        repetition(
            concatenation([
                regex("elseif "),
                expression,
                regex(" then\n"),
                indented_statements.clone(),
            ]),
            0..,
        ),
        repetition(concatenation([regex("else\n"), indented_statements]), 0..=1),
        regex("endif\n"),
    ]);

    let record = concatenation([
        regex("record "),
        ident.clone(),
        literal('\n'),
        repetition(
            concatenation([
                regex("  "),
                ident,
                regex(" of "),
                alternation([regex("Bool"), regex("Int"), regex("String")]),
                literal('\n'),
            ]),
            1..100,
        ),
        regex("endrecord"),
    ]);

    let block = alternation([for_loop, while_loop, function, if_statement, record]);

    let ast = repetition(alternation([block, statement]), 0..);

    let ast_mutator = grammar_based_ast_mutator(ast);

    let result = fuzzcheck::fuzz_test(|ast: &AST| run_test(&ast.to_string()))
        .mutator(ast_mutator)
        .serde_serializer()
        .default_sensor_and_pool()
        .arguments_from_cargo_fuzzcheck()
        .launch();
    assert!(!result.found_test_failure);
}
