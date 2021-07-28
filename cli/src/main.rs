use std::{
    env::{self},
    fs, process,
};

use logic::parse;

fn main() {
    let args = env::args().collect::<Vec<_>>();

    if args.is_empty() {
        println!("You must provide the name of the file you would like to compile!");
        process::abort();
    }

    let file_name = args.get(1).unwrap();

    let input = fs::read_to_string(file_name)
        .expect("the file in question could not be opened; are you sure it exists");

    parse::parse(&input).unwrap();
}
