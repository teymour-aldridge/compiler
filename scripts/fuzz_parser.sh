(
    cd tests/fuzz_parser &&
    cargo afl fuzz -i in -o results ../../target/release/fuzz_parser
)