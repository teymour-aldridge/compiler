use proc_macro::{TokenStream, TokenTree};

#[proc_macro]
pub fn regressions(input: TokenStream) -> TokenStream {
    let mut iter = input.into_iter();
    let func_ident = iter.next().expect("expected the token `function` to start");

    let func_ident = match func_ident {
        TokenTree::Ident(ident) => ident.to_string(),
        _ => panic!(),
    };

    let mut literals = vec![];

    match iter.next().unwrap() {
        TokenTree::Group(group) => {
            for tree in group.stream() {
                match tree {
                    TokenTree::Literal(lit) => literals.push(lit),
                    TokenTree::Punct(p) if p.as_char() == ',' => continue,
                    _ => {
                        panic!("Expected a literal")
                    }
                }
            }
        }
        _ => panic!("Expected a literal here"),
    };

    println!("step 4");

    literals
        .into_iter()
        .enumerate()
        .map(|(index, lit)| {
            format!(
                "
        #[test]
        fn regression_{}() {{
            {}({});
        }}
        ",
                index, func_ident, lit
            )
        })
        .collect::<Vec<_>>()
        .join("")
        .parse()
        .unwrap()
}
