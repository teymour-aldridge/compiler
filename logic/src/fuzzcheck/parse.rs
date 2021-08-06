#[rustversion::nightly]
mod parse {

    use std::rc::Rc;

    use fuzzcheck::{
        mutators::grammar::{grammar_based_string_mutator, Grammar},
        FuzzerBuilder, SerdeSerializer,
    };

    use crate::parse::{
        utils::{Input, Parse},
        Ast,
    };

    /// Produces binary operators.
    fn bin_op_grammar() -> Rc<Grammar> {
        Grammar::alternation(vec![
            Grammar::literal('+'..='+'),
            Grammar::literal('/'..='/'),
        ])
    }

    fn number_literal_grammar() -> Rc<Grammar> {
        Grammar::literal('0'..='9')
    }

    fn expression_grammar() -> Rc<Grammar> {
        Grammar::alternation(vec![Grammar::concatenation(vec![
            number_literal_grammar(),
            bin_op_grammar(),
            number_literal_grammar(),
        ])])
    }

    fn parse(input: &String) -> bool {
        let mut input = Input::new(&input);
        let ast = if let Ok(ast) = Ast::parse(&mut input) {
            ast
        } else {
            return false;
        };

        let intermediate = ast.to_string();

        let mut intermediate = Input::new(&intermediate);

        let reparsed = if let Ok(ast) = Ast::parse(&mut intermediate) {
            ast
        } else {
            return false;
        };

        ast == reparsed
    }

    #[test]
    fn fuzz_parser() {
        FuzzerBuilder::test(parse)
            .mutator(grammar_based_string_mutator(expression_grammar()))
            .serializer(SerdeSerializer::default())
            .arguments_from_cargo_fuzzcheck()
            .observe_only_files_from_current_dir()
            .launch()
    }
}
