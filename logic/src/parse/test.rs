mod parse_reparse {
    use crate::parse::{parse, utils::Input};

    fn inner((input, should_parse): (&str, bool)) {
        match parse(&mut Input::new(input)) {
            Ok(_) => {
                if !should_parse {
                    panic!("The input `{}` should not have parsed, but it did.", input);
                }
            }
            Err(e) => {
                if should_parse {
                    panic!(
                        "The input `{}` should have parsed but it did not, with error `{:#?}`",
                        input, e
                    );
                } else {
                }
            }
        };
    }

    #[test]
    fn parse_expr() {
        inner(("a + b * c * d + e", true));
    }

    #[test]
    fn parse_assignment() {
        inner(("a = b + c", true))
    }

    #[test]
    fn parse_for() {
        inner((include_str!("examples/for"), true));
    }

    #[test]
    fn parse_for2() {
        inner((include_str!("examples/fuzzcheck/for2"), true));
    }

    #[test]
    fn fail_to_parse_invalid_for() {
        inner((include_str!("examples/invalid-for"), false));
        inner((include_str!("examples/invalid-for2"), false));
        inner((include_str!("examples/invalid-for3"), false));
        inner((include_str!("examples/invalid-for4"), false));
        inner((include_str!("examples/invalid-for5"), false));
    }

    #[test]
    fn parse_function() {
        inner((include_str!("examples/valid-func"), true));
    }

    #[test]
    fn comments() {
        inner((include_str!("examples/comments"), true));
    }

    #[test]
    fn fuzzcheck_regressions() {
        inner((include_str!("examples/fuzzcheck/for"), true));
        inner((include_str!("examples/fuzzcheck/func-call"), false));
        inner((include_str!("examples/fuzzcheck/if"), true));
        inner((include_str!("examples/fuzzcheck/complex-if"), true));
        inner((include_str!("examples/fuzzcheck/diverse-cov"), true));
        inner((include_str!("examples/fuzzcheck/record"), true));
        inner((include_str!("examples/fuzzcheck/simple-constructor"), true));
        inner((include_str!("examples/fuzzcheck/high-cov"), true));
        inner((include_str!("examples/fuzzcheck/diverse-cov"), true));
        inner((include_str!("examples/fuzzcheck/complex-expr"), true));
        inner((include_str!("examples/fuzzcheck/floats"), true));
    }

    #[test]
    fn parse_while() {
        inner((include_str!("examples/while"), true));
    }

    #[test]
    fn parse_if() {
        inner((include_str!("examples/if"), true));
    }

    #[test]
    fn fuzzcheck_record_failure() {
        inner(("record MJ\n  VY of String\nendrecord", true));
    }
}
