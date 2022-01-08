use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor::Buffer, Config},
};

use crate::{id::tag, parse::parse, ty::type_check};

fn setup_file(file: String) -> (usize, SimpleFiles<String, String>) {
    let mut files = SimpleFiles::new();
    let id = files.add("file".to_string(), file);
    (id, files)
}

fn ui_test(string: String) {
    let tree = parse(&string).unwrap();
    let tagged = tag(tree);
    let err = type_check(&tagged).unwrap_err();
    let (file_id, files) = setup_file(string.clone());
    let report = err.report(file_id);

    let mut writer = Buffer::no_color();

    let config = Config::default();

    term::emit(&mut writer, &config, &files, &report).unwrap();

    let vec = writer.into_inner();
    let string = String::from_utf8(vec).unwrap();

    insta::assert_snapshot!(string);
}

#[test]
fn test_simple() {
    ui_test(include_str!("examples/failing").to_string());
}
