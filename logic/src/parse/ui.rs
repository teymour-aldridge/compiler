/// Tests that the parser emits appropriate errors.
use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor::Buffer, Config},
};

use super::parse;

fn setup_file(file: String) -> (usize, SimpleFiles<String, String>) {
    let mut files = SimpleFiles::new();
    let id = files.add("file".to_string(), file);
    (id, files)
}

/// The generic test function (concrete values are provided in the tests lower down).
fn ui_test(string: String) {
    let err = parse(&string).unwrap_err();
    let (file_id, files) = setup_file(string);
    let report = err.report(file_id);

    let mut writer = Buffer::no_color();

    let config = Config::default();

    term::emit(&mut writer, &config, &files, &report).unwrap();

    let vec = writer.into_inner();
    let string = String::from_utf8(vec).unwrap();

    insta::assert_snapshot!(string);
}

#[test]
/// I'm reasonably happy with this error message
fn test_invalid_operator() {
    ui_test("(a b c)".to_string())
}

#[test]
/// I'm also reasonably happy with this error message
fn test_invalid_brackets_1() {
    ui_test("( ( ) a".to_string())
}

#[test]
fn test_invalid_brackets2() {
    // this ideally should also mention what token it expects (rather than just saying "unexpected
    // end of input")
    ui_test("f(x + a * z (( zz".to_string())
}

#[test]
fn test_non_alphabetic_ident() {
    // i'm not sure if this is the best possible error message tbh
    ui_test("0x = 15".to_string());
}

#[test]
fn test_missing_endfor() {
    // todo: make this highlight the *whole* token (see the debug snapshot for specifics)
    ui_test(include_str!("ui-examples/missing-next").to_string())
}
