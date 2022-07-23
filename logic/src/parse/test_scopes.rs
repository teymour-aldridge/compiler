use super::parse;

#[test]
fn simple_scopes() {
    let tree = parse(include_str!("examples/simple-scope")).unwrap();
    // `main`, `y`, `x`, `print` and `i`
    assert_eq!(tree.ident.len(), 5);
    assert_eq!(
        tree.ident
            .iter()
            .filter(|(_, ident)| { ident.inner() == "y" })
            .count(),
        1
    );
    assert_eq!(
        tree.ident
            .iter()
            .filter(|(_, ident)| { ident.inner() == "i" })
            .count(),
        1
    );
}

#[test]
fn for_scope() {
    let tree = parse(include_str!("examples/simple-for-scope")).unwrap();
    // `i`, and `print_int`
    assert_eq!(tree.ident.len(), 2);
    assert_eq!(
        tree.ident
            .iter()
            .filter(|(_, ident)| { ident.inner() == "i" })
            .count(),
        1
    );
}

#[test]
fn function_scopes() {
    let tree = parse(include_str!("examples/scopes-with-functions")).unwrap();
    // three functions - each one has two identifiers (its name and one variable inside the
    // function)
    assert_eq!(tree.ident.len(), 3 * 2);
    assert_eq!(
        tree.ident
            .iter()
            .filter(|(_, ident)| { ident.inner() == "f" })
            .count(),
        1
    );
    // here we check that names declared inside multiple functions are treated seperately
    assert_eq!(
        tree.ident
            .iter()
            .filter(|(_, ident)| { ident.inner() == "y" })
            .count(),
        2
    );
}
