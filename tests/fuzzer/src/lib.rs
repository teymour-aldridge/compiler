#[cfg(all(test, fuzzing))]
use fuzzcheck::fuzz_test;

#[test]
#[cfg(fuzzing)]
pub fn run_fuzzer() {
    if !std::env::var("FUZZCHECK_ARGS").is_ok() {
        return;
    }

    use fuzzcheck::{builder::basic_sensor_and_pool, mutators::unique::UniqueMutator};

    // as per suggestions in
    // https://github.com/loiclec/fuzzcheck-rs/issues/31#issuecomment-1200470065
    let (sensor, pool) = basic_sensor_and_pool()
        .find_most_diverse_set_of_test_cases(20)
        .finish();

    let mutator = generator::block_with_string_mutator();
    // also as per the suggestions from the aforementioned issue
    let mutator = UniqueMutator::new(mutator, |(string, _block)| string);

    let result = fuzz_test(|input: &(String, generator::Block)| compile_for_fuzzing(&input.0))
        .mutator(mutator)
        .serde_serializer()
        .sensor_and_pool(sensor, pool)
        .arguments_from_cargo_fuzzcheck()
        .launch();
    assert!(!result.found_test_failure);
}

#[cfg(test)]
#[cfg(fuzzing)]
fn compile_for_fuzzing(input: &str) {
    let table = logic::parse::parse(input).unwrap();
    if let Ok(ty_checked) = logic::ty::type_check(&table) {
        let _ = logic::codegen::codegen(&table, &ty_checked);
    }
}
