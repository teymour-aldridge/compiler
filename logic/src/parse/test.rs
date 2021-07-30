mod parse_reparse {
    use crate::parse::{
        utils::{Input, Parse},
        Ast,
    };

    fn inner(input: &str) {
        let ast = match Ast::parse(&mut Input::new(input)) {
            Ok(t) => t,
            Err(_) => return,
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
                panic!("failed to parse, with error: {:#?}", e);
            }
        };
    }

    codegen::regressions! {
        inner,
        [
            "-͓", "x7=27=", "ۆ", "-[YYYYY
            0YYYYY",
            "(-٫55580,",
            "(X~Y",
            "=[",
            "- Y",
            "([385",
            "(-[=18J/3",
            "49",
            "޶-W-",
            "z -",
            "XY=-[Y",
            "(ȧ0a782(",
            "YY=-ͪYZYY=",
            "Z=ˀY",
            "[7-",
            " V",
            "A0",
            "A ",
            "A + A + - * C",
            "AA09AA090O09",
            " + B******************** * C",
            "AA+++o++++/",
            "ⲲCCCCCCC",
            "OOOOOOOOOOOOOOOOOOOO0",
            "Ǿ۳F",
            "///////////////////////////////////////////////////////////////////////"
        ]
    }
}
