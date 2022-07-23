use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor::Buffer, Config},
};

use crate::{parse::parse, ty::type_check};

fn setup_file(file: String) -> (usize, SimpleFiles<String, String>) {
    let mut files = SimpleFiles::new();
    let id = files.add("file".to_string(), file);
    (id, files)
}

fn ui_test(string: String) -> String {
    let tree = parse(&string).unwrap();

    let err = type_check(&tree).unwrap_err();
    let (file_id, files) = setup_file(string.clone());
    let report = err.report(file_id, &tree);

    let mut writer = Buffer::no_color();

    let config = Config::default();

    term::emit(&mut writer, &config, &files, &report).unwrap();

    let vec = writer.into_inner();
    String::from_utf8(vec).unwrap()
}

#[test]
fn test_simple() {
    insta::assert_snapshot!(ui_test(include_str!("examples/failing").to_string()));
}
