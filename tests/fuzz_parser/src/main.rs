use logic::parse::parse;

fn main() {
    afl::fuzz!(|string: &[u8]| {
        if let Ok(string) = String::from_utf8(string.to_vec()) {
            if let Ok(ast) = parse(&string) {
                let out = ast.to_string();
                let reconstructed_ast = parse(&out).expect("failed to parse");
                assert_eq!(ast, reconstructed_ast);
            }
        }
    });
}
