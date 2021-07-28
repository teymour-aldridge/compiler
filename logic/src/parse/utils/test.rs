mod advance_positions {
    use crate::parse::utils::Input;
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
    fn regressions() {
        test_inner(&mut "A".to_string());
    }
}
