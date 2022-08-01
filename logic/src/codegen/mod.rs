use crate::{parse::table::ParseTable, ty::TyEnv};

use self::compile::Compiler;

/// Performs the actual AST -> Cranelift IR pass
mod compile;
/// Translation of expressions into Cranelift IR.
mod expr;
/// Translation of individual functions into Cranelift IR.
mod func;
/// Layouts of objects in memory.
pub(self) mod layout;
/// Produces the `ObjectModule` necessary for the compiler target in question.
pub(self) mod make_module;

/// Compiles the AST to machine code and writes it to an object file called `program.o`.
///
/// todo: automatically link
/// todo: allow custom file outputs
pub fn compile<'compiler>(ast: &'compiler ParseTable<'compiler>, env: &'compiler TyEnv) -> i32 {
    let mut compiler = Compiler::new(env);

    compiler.compile(ast);

    let output = compiler.finish();

    let code_fn = unsafe { std::mem::transmute::<_, fn(()) -> i32>(output) };
    code_fn(())
}
