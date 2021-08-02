use insta::assert_debug_snapshot;

use crate::{id::tag, parse::parse};

#[test]
fn test_scopes() {
    let ast = parse(include_str!("examples/one")).expect("failed to parse");
    let tagged = tag(ast);
    assert_debug_snapshot!(tagged)
}
