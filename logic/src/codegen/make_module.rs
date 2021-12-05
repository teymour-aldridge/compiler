use cranelift_codegen::settings;
use cranelift_module::default_libcall_names;
use cranelift_native::builder_with_options;
use cranelift_object::{ObjectBuilder, ObjectModule};

/// Instantiates a module configured for the Instruction Set Architecture (ISA) of the machine that
/// is currently running the compiler.
///
/// Cross-compiling is planned, but not yet supported.
pub(crate) fn make_module_for_compiler_host_architecture() -> ObjectModule {
    let flags = settings::Flags::new(settings::builder());

    let target = builder_with_options(true).unwrap().finish(flags);

    let builder = ObjectBuilder::new(target, vec![], default_libcall_names()).unwrap();

    ObjectModule::new(builder)
}
