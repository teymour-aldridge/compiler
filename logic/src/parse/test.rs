mod parse_reparse {
    use crate::parse::{
        utils::{Input, Parse},
        Ast,
    };

    fn inner((input, should_parse): (&str, bool)) {
        let ast = match Ast::parse(&mut Input::new(input)) {
            Ok(t) => {
                if !should_parse {
                    panic!("The input `{}` should not have parsed, but it did.", input);
                }
                t
            }
            Err(e) => {
                if should_parse {
                    panic!(
                        "The input `{}` should have parsed but it did not, with error `{:#?}`",
                        input, e
                    );
                } else {
                    return;
                }
            }
        };
        let output = ast.to_string();
        match Ast::parse(&mut Input::new(&output)) {
            Ok(reconstructed) => {
                if ast != reconstructed {
                    println!("INPUT: {}", input);
                    println!("INTERMEDIATE: {}", output);
                    panic!("the reconstructed ast does not equal the initially parsed one");
                }
            }
            Err(e) => {
                println!("INPUT: {}", input);
                println!("INTERMEDIATE: {}", output);
                panic!(
                    "failed to parse the reconstructed output, with error: {:#?}",
                    e
                );
            }
        };
    }

    codegen::regressions! {
        inner,
        [
            ("-͓", false),
            ("x7=27=", false),
            ("
            ۆ", false),
            ("-[YYYYY
            0YYYYY", false),
            ("(-٫55580,", false),
            ("(X~Y", false),
            ("=[", false),
            ("- Y", false),
            ("([385", false),
            ("(-[=18J/3", false),
            ("49", false),
            ("޶-W-", false),
            ("z -", false),
            ("XY=-[Y", false),
            ("(ȧ0a782(", false),
            ("YY=-ͪYZYY=", false),
            ("Z=ˀY", true),
            ("[7-", false),
            (" V", false),
            ("A0", true),
            ("A ", true),
            ("A + A + - * C", true),
            ("AA09AA090O09", true),
            (" + B******************** * C", false),
            ("AA+++o++++/", false),
            ("ⲲCCCCCCC", true),
            ("OOOOOOOOOOOOOOOOOOOO0", true),
            ("Ǿ۳F", true),
            ("///////////////////////////////////////////////////////////////////////", false),
            ("X ---t", true),
            ("A0 I", false),
            ("A 5", false),
            ("A nnnn", false),
            ("d
            PB
            ", false),
            ("A B", false),
            ("A  fwBfwB", false),
            ("A  	BB", false),
            ("
            1 555", false),
            ("A	BB
            ", false),
            ("GGGGGGGGGGGGGGGGGGGGGGGB
            8B
             ", false),
            ("A B
            ", false),
            ("1 555A5J55", false),
            ("A
            AAAAAAAAAAAAAAAAAAAAAAMAJAAAAAAAAMAb", false),
            ("P A + B M ", false),
            ("ՇBBBBBB BB", false),
            ("ՇB	2C -J	 C ", false),
            ("ՇB	 C -B	 C -", false),
            ("Շ4	 CIIIII/III CIIIIIIIII", false),
            ("ՇB	 Շppppp", true),
            ("	ևB	 CC3/B
            C * /B
            C *", false),
            ("D O	* A J ", false),
            ("wABBB BBA쀁BBBj", false),
            ("w

            w
            ", false),
            ("A 333333H33333H3333333033333H333333333333", false),
            ("333              3                                             ", false),
            ("٢J	5C ", false),
            ("w   BIB3333", false),
            ("A / B/+ C -", false),
            ("A / B + / D - ", false),
            ("A / B + -A", true),
            ("A / B + -6", true),
            ("AA /C - /C - ", false),
            ("A + C -+ C - ", false),
            ("A / B + + C      -", false),
            ("A/ B +  + C", true),
            ("A / B *-C - ", false),
            ("A / B +   /  C -
            ", false),
            ("A+ C - / B ", false),
            ("A /	 B +  + C - ", false),
            ("A / B + -BBBBBBBBBBBBBBBBBBBBBB4BBBBUBB ", true),
            ("A / B ++C - / B ++C - ", false),
            ("A / B +                                   *    C ", true),
            ("a + b * c * d + e", true),
            ("a = b + c", true),
            (include_str!("examples/for"), true),
            (include_str!("examples/invalid-for"), false),
            ("forA = B +AB +A = B +A =OOOOO* EO=OOOAAAAAAAAAAAAAAAAAAAAAAAA=nOOOO* E", false),
            ("forA = +AOOO* E", false),
            ("forA = B +ABE", false),
            ("forӷT", false),
            ("A++++++++++++++++++++++++++ B", true),
            ("	for++++++++++++++++++++++++++ B", false),
            ("for𖥀o", false),
            ("forf3ror𖥇A + B", false),
            ("a = 1 + 2", true),
            ("2 = \"", false),
            ("\"+ + C - C - L =5", false),
            ("A0= \"", false),
            ("\" ", false),
            ("\" / B
            ", false),
            ("for=e = A ", false),
            ("for", false),
            ("for*i = c ", false),
            (include_str!("examples/invalid-for2"), false),
            (include_str!("examples/invalid-for3"), false),
            (include_str!("examples/invalid-for4"), false),
            (include_str!("examples/invalid-for5"), false),
            (include_str!("examples/if"), true),
            ("a + 1", true),
            (include_str!("examples/while"), true),
            (include_str!("examples/invalid-if"), false),
            ("a = True", true),
            (include_str!("examples/func"), true),
            (include_str!("examples/invalid-func"), false),
            (include_str!("examples/invalid-if-for"), false),
            (include_str!("examples/invalid-return"), false),
            (include_str!("examples/invalid-if2"), false),
            (include_str!("examples/invalid-while"), false),
            ("--------------------------------------------------------A", true),
            ("call_function(a + b)", true),
            ("f(x + 2 + 3) + 4", true),
            (include_str!("examples/invalid-for6"), false),
            (include_str!("examples/invalid-if3"), false),
            (include_str!("examples/invalid-if4"), false),
            ("return+ 1", false),
            (include_str!("examples/invalid-func2"), false),
            ("U", true),
            (include_str!("examples/multiple"), true),
            (include_str!("examples/nested"), true),
            (include_str!("examples/fuzzcheck/complex-expr"), true),
            (include_str!("examples/fuzzcheck/floats"), true)
            ("(1 + 2) * 3", true),
            (include_str!("examples/fuzzcheck/for"), true),
            (include_str!("examples/fuzzcheck/func-call"), false),
            (include_str!("examples/fuzzcheck/for2"), true),
            (include_str!("examples/valid-func"), true),
            (include_str!("examples/fuzzcheck/if"), true),
            (include_str!("examples/fuzzcheck/complex-if"), true),
            (include_str!("examples/fuzzcheck/diverse-cov"), true),
            (include_str!("examples/fuzzcheck/record"), true),
            (include_str!("examples/fuzzcheck/simple-constructor"), true),
            (include_str!("examples/fuzzcheck/high-cov"), true),
            (include_str!("examples/fuzzcheck/diverse-cov"), true),
        ]
        // note: examples/invalid-expr and examples/invalid-expr2 are not
        // included here because I haven't figured out what is causing the stack
        // overflow
    }
}
