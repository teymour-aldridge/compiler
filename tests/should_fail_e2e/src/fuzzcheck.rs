fn compile_for_fuzzing(input: &str) {
    let table = logic::parse::parse(input).unwrap();
    if let Ok(ty_checked) = logic::ty::type_check(&table) {
        let _ = logic::codegen::compile(&table, &ty_checked);
    }
}

#[test]
fn invalid_dot_expr() {
    compile_for_fuzzing("17913108076166669831.False")
}
