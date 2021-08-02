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

mod write_indent {
    #[cfg(feature = "_proptest")]
    mod proptest {
        use std::fmt;

        use proptest::prelude::*;

        use crate::parse::utils::{write_indentation, Input};

        proptest! {
            #[test]
            fn test(units in 1..10000usize) {
                struct X {units: usize}
                impl fmt::Display for X {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        write_indentation(self.units, f)
                    }
                }
                let output = X {units}.to_string();
                assert_eq!(Input::new(&output).count_indent().unwrap(), units);
                let concat = output + "a";
                let mut input = Input::new(&concat);
                input.set_indent(units);
                assert!(input.advance_indent().is_ok());
                assert_eq!(input.inner, "a")
            }
        }
    }
}
