#!/bin/bash -e
# tests that the files in `examples` provide the correct output when run

# should be run from the root directory
cargo build --workspace
mkdir runin
cd runin

function cleanup() {
    cd ..
    rm -rf runin
}

trap cleanup EXIT

# copy the runtime and compiler artefacts into this directory
cp ../target/debug/libruntime.a libruntime.a
cp ../target/debug/cli cli
# copy the examples into this directory
mkdir examples
cp ../examples/*.pseudo examples

for f in examples/alloc.pseudo; do
    ./cli "$f"
    cc program.o libruntime.a
    echo $(./a.out) >> output
    echo $(cat "../examples/expected/$f") >> oracle
    echo $output
    cmp output oracle
done
