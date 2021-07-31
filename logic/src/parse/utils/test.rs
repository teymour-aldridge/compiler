mod advance_positions {
    use crate::parse::utils::Input;
    #[cfg(feature = "_proptest")]
    use proptest::prelude::*;

    #[cfg(feature = "_proptest")]
    proptest! {
        #[test]
        fn test(mut input in "[a-zA-Z]+") {
            test_inner(&mut input);
        }
    }

    fn test_inner(input: &mut str) {
        let mut cursor = Input::new(input);

        cursor.advance_one().expect("failed to advance one");

        assert_eq!(cursor.position().column, 1);
    }

    #[test]
    // add results from the property test above here.
    fn regressions() {
        test_inner(&mut "A".to_string());
    }
}

mod skip_whitespace {
    #[cfg(feature = "_proptest")]
    mod proptest {
        use proptest::prelude::*;

        use crate::parse::utils::Input;

        proptest! {
            #[test]
            fn skip_whitespace(input in "[ ]+[a-zA-Z,{}]+") {
                let mut input = Input::new(&input);
                input.skip_whitespace().unwrap();
                assert_ne!(input.peek_char().unwrap(), ' ');
            }
        }

        #[test]
        fn regressions() {}
    }
}

mod test_parse_token {
    use crate::parse::utils::Input;

    #[test]
    fn regression_1() {
        let mut cursor = Input::new("\"");
        cursor.parse_token("\"").expect("failed to parse");
        assert!(cursor.is_empty())
    }
}

mod test_peek_n {
    use crate::parse::utils::Input;

    #[test]
    fn regression_1() {
        let cursor = Input::new("\"");
        assert_eq!(cursor.peek_n(1), Some("\""));
    }
}
