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
        let reparsed = Ast::parse(&mut Input::new(&output)).expect("failed to reparse ast");
        assert_eq!(ast, reparsed);
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
            "[7-"
        ]
    }
}
