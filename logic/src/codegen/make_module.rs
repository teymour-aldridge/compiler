use cranelift_codegen::settings::{self, Configurable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::default_libcall_names;

#[no_mangle]
unsafe fn print(len: i64, ptr: *const u8) {
    let slice = std::str::from_utf8(std::slice::from_raw_parts(ptr, len as usize)).unwrap();
    println!("{}", slice)
}

#[no_mangle]
fn print_int(int: i32) {
    println!("{}", int)
}

#[no_mangle]
fn print_bool(boolean: i32) {
    println!(
        "{}",
        if boolean == 1 {
            "True"
        } else if boolean == 0 {
            "False"
        } else {
            panic!("invalid boolean value {boolean}")
        }
    )
}

pub(crate) fn make_module_for_compiler_host_architecture() -> JITModule {
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();

    // workaround for https://github.com/bytecodealliance/wasmtime/issues/2735
    if cfg!(target_arch = "aarch64") || cfg!(target_arch = "arm") {
        flag_builder.set("is_pic", "false").unwrap();
    } else {
        flag_builder.set("is_pic", "true").unwrap();
    }

    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {}", msg);
    });
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap();
    let mut builder = JITBuilder::with_isa(isa, default_libcall_names());

    // define some standard library items
    builder.symbol("print", print as *const u8);
    builder.symbol("print_int", print_int as *const u8);
    builder.symbol("print_bool", print_bool as *const u8);

    JITModule::new(builder)
}
