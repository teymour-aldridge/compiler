use std::{env, fs, process};

use codespan_reporting::{
    files::SimpleFiles,
    term::{
        emit,
        termcolor::{ColorChoice, StandardStream},
    },
};
use logic::{codegen::compile, parse, ty::type_check};

/// Runs the compiler.
///
/// todo: some sort of incremental computation
/// todo: emit multiple errors
fn main() {
    let args = env::args().collect::<Vec<_>>();

    if args.is_empty() {
        println!("You must provide the name of the file you would like to compile!");
        process::exit(1);
    }

    let file_name = args.get(1).unwrap();

    let input = fs::read_to_string(file_name)
        .expect("the file in question could not be opened; are you sure it exists");

    let mut files = SimpleFiles::new();
    let file_id = files.add(file_name, input.clone());

    let mut writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let ast = match parse::parse(&input) {
        Ok(ast) => ast,
        Err(error) => {
            let report = error.report(file_id);
            emit(&mut writer, &config, &files, &report).unwrap();
            process::exit(1);
        }
    };

    let env = match type_check(&ast) {
        Ok(env) => env,
        Err(error) => {
            let report = error.report(file_id, &ast);
            emit(&mut writer, &config, &files, &report).unwrap();
            process::exit(1);
        }
    };

    match compile(&ast, &env) {
        Ok(env) => env,
        Err(error) => {
            let report = error.report(file_id);
            emit(&mut writer, &config, &files, &report).unwrap();
            process::exit(1);
        }
    };
}
