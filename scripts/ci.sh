cargo nextest run
echo "------------------------------------------------------------------------"
echo "TESTING COMPILED PROGRAMS"
(cd tests/oracle && cargo run)
