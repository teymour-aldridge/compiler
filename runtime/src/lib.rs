//! When I say "runtime", I mean "helpful Rust package that is linked to the compiled program"

#[no_mangle]
/// Prints the provided string to the standard output.
///
/// This function will at some point be replaced with a unified API.
pub extern "C" fn print(len: i32, ptr: *const u8) {
    let mut string = vec![];
    for i in 0isize..len as isize {
        string.push(unsafe { *ptr.offset(i) })
    }
    println!("{}", String::from_utf8(string).unwrap())
}

#[no_mangle]
/// Prints the provided integer to the standard output.
///
/// This function will at some point be replaced with a unified API.
pub extern "C" fn print_int(int: i32) {
    println!("{}", int)
}
