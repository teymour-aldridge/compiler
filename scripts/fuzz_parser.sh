(
    cd tests/fuzz_parser &&
    cargo afl build --release &&
    cargo afl fuzz -i in -o results ../../target/release/fuzz_parser
)