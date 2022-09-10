#[cfg(test)]
use std::{fs::read_to_string, process::Command};

#[cfg(test)]
mod fuzzcheck_finds;

#[cfg(test)]
use lang_tester::LangTester;
#[cfg(test)]
use run_script::{IoOptions, ScriptOptions};

#[cfg(test)]
#[test]
fn integration_tests() {
    static COMMENT_PREFIX: &str = ";;";
    let options = ScriptOptions {
        output_redirection: IoOptions::Inherit,
        ..ScriptOptions::new()
    };
    let args = vec![];
    run_script::run(
        r#"
            (cd ../../cli && cargo build)
            mv ../../target/debug/pseudo ${CARGO_HOME}/bin
            "#,
        &args,
        &options,
    )
    .unwrap();

    LangTester::new()
        .test_dir("../../filetests")
        .test_file_filter(|p| p.extension().unwrap().to_str().unwrap() == "pseudo")
        .test_extract(|p| {
            read_to_string(p)
                .unwrap()
                .lines()
                .skip_while(|l| !l.starts_with(COMMENT_PREFIX))
                .take_while(|l| l.starts_with(COMMENT_PREFIX))
                .map(|l| &l[COMMENT_PREFIX.len()..])
                .collect::<Vec<_>>()
                .join("\n")
        })
        .test_cmds(move |p| {
            let mut compiler = Command::new("pseudo");
            compiler.args(&[p.to_str().unwrap()]);

            vec![("compiler", compiler)]
        })
        .run();
}
