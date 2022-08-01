fn compile_for_fuzzing(input: &str) {
    let table = logic::parse::parse(input).unwrap();
    if let Ok(ty_checked) = logic::ty::type_check(&table) {
        let _ = logic::codegen::compile(&table, &ty_checked);
    }
}

#[test]
#[ignore = "todo: add an error message for this program"]
fn empty_function_with_one_parameter() {
    compile_for_fuzzing("function P (t,)\nendfunction\nP = False\n");
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
#[ignore = "todo: add an error message for this program"]
fn boolean_field_access_inside_function() {
    compile_for_fuzzing("function P ()\n  True.H\nendfunction");
}

#[test]
#[ignore = "todo: implement support for string literals in expression positions"]
fn string_literal() {
    compile_for_fuzzing("function P ()\n  N = \"i\"\nendfunction\nP = False\n");
}

#[test]
#[ignore = "todo: add an error message for this program"]
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
