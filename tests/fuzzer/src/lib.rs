#[cfg(test)]
#[cfg(feature = "fuzzcheck")]
use fuzzcheck::fuzz_test;

#[test]
#[cfg(feature = "fuzzcheck")]
pub fn main() {
    let result = fuzz_test(|input: &(String, generator::Block)| compile_for_fuzzing(&input.0))
        .mutator(generator::block_with_string_mutator())
        .serde_serializer()
        .default_sensor_and_pool()
        .arguments_from_cargo_fuzzcheck()
        .launch();
    assert!(!result.found_test_failure);
}

#[cfg(feature = "fuzzcheck")]
#[cfg(test)]
fn compile_for_fuzzing(input: &str) {
    let table = logic::parse::parse(input).unwrap();
    if let Ok(ty_checked) = logic::ty::type_check(&table) {
        let _ = logic::codegen::compile(&table, &ty_checked, None);
    }
}
