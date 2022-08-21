use crate::{diagnostics::reportable_error::ReportableError, parse::table::ParseTable, ty::TyEnv};

use self::compile::Codegen;

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
pub fn codegen<'compiler>(
    ast: &'compiler ParseTable<'compiler>,
    env: &'compiler TyEnv,
) -> Result<i32, ReportableError> {
    let mut compiler = Codegen::new(env, ast);

    compiler.compile(ast)?;

    let output = compiler.finish()?;

    let code_fn = unsafe { std::mem::transmute::<_, fn(()) -> i32>(output) };
    Ok(code_fn(()))
}
