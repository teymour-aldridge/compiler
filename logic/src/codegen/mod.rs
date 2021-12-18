use crate::{id::TaggedAst, ty::TyEnv};

use self::compile::Compiler;

/// Performs the actual AST -> Cranelift IR pass
mod compile;
/// Layouts of objects in memory.
pub(self) mod layout;
/// Produces the `ObjectModule` necessary for the compiler target in question.
pub(self) mod make_module;

/// Compiles the AST to machine code and writes it to an object file called `program.o`.
///
/// todo: automatically link
/// todo: allow custom file outputs
pub fn compile<'compiler>(ast: &'compiler TaggedAst, env: &'compiler TyEnv) {
    let mut compiler = Compiler::new(env);

    compiler.compile(ast);

    let output = compiler.finish();

    let res = output.emit().unwrap();

    // todo: store the output in the correct location
    std::fs::write("program.o", res).expect("failed to write code to file");
}
