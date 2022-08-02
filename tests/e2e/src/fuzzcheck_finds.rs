use logic::{
    diagnostics::reportable_error::ReportableError, parse::utils::ParseError,
    ty::error::TyCheckError,
};

fn compile_for_fuzzing(input: &str) {
    let table = logic::parse::parse(input).unwrap();
    if let Ok(ty_checked) = logic::ty::type_check(&table) {
        let _ = logic::codegen::compile(&table, &ty_checked);
    }
}

/// Describes the status of a program we attempted to run.
enum ExecutionStatus {
    /// All stages ran successfully.
    Ok(i32),
    /// The program could not be parsed.
    FailedParsing(ParseError),
    /// The program could not be type checked.
    FailedTypeChecking(TyCheckError),
    /// The relevant machine code could not be generated for the program.
    FailedCodeGeneration(ReportableError),
}

impl ExecutionStatus {
    fn as_failed_code_generation(&self) -> Option<&ReportableError> {
        if let Self::FailedCodeGeneration(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// Runs the provided code fragment, returning the status of the execution.
fn run_test(input: &str) -> ExecutionStatus {
    let tree = match logic::parse::parse(input) {
        Ok(tree) => tree,
        Err(error) => return ExecutionStatus::FailedParsing(error),
    };

    let ty_env = match logic::ty::type_check(&tree) {
        Ok(env) => env,
        Err(err) => return ExecutionStatus::FailedTypeChecking(err),
    };

    let codegen = match logic::codegen::compile(&tree, &ty_env) {
        Ok(res) => res,
        Err(err) => return ExecutionStatus::FailedCodeGeneration(err),
    };

    ExecutionStatus::Ok(codegen)
}

#[test]
fn empty_function_with_one_parameter() {
    let status = run_test("function P (t,)\nendfunction\nP = False\n");
    let error = status.as_failed_code_generation().unwrap();
    dbg!(&error);
    assert!(error
        .explanation()
        // todo: this message could be improved
        .contains("A type of variable could not be established for this function parameter."));
}

#[test]
#[ignore = "todo: fix a bug in struct stack layouts"]
fn function_and_record() {
    compile_for_fuzzing("function P ()\n  +s { k: False,}\nendfunction\nP = False\n");
}

#[test]
#[ignore = "todo: implement boolean literals"]
fn empty_function_with_boolean_literal() {
    compile_for_fuzzing("function P ()\n  False\nendfunction\nP = False\n");
}

#[test]
fn boolean_field_access_inside_function() {
    let result = run_test("function P ()\n  True.H\nendfunction");
    let error = result.as_failed_code_generation().unwrap();
    assert!(error.explanation().contains("return type"));
    assert!(error.explanation().contains("could not"));
}

#[test]
#[ignore = "todo: implement support for string literals in expression positions"]
fn string_literal() {
    compile_for_fuzzing("function P ()\n  N = \"i\"\nendfunction\nP = False\n");
}

#[test]
#[ignore = "todo: fix incorrect use of Cranelift"]
fn bool_inside_func() {
    compile_for_fuzzing("function P ()\n  record U\n  endrecord\nendfunction\nP = False");
}

#[test]
#[ignore = "todo: add an error message for this program"]
fn constructor_without_record_definition() {
    compile_for_fuzzing("k { }");
}

#[test]
#[ignore = "todo: implement support for methods"]
fn boolean_method_call() {
    compile_for_fuzzing("True.m()\n");
}
