use crate::{id::TaggedAst, ty::TyEnv};

use self::compile::Compiler;

/// Performs the actual AST -> Cranelift IR pass
mod compile;
mod make_module;

pub fn compile<'compiler>(ast: &'compiler TaggedAst, env: &'compiler TyEnv) {
    let mut compiler = Compiler::new(env);

    compiler.compile(ast);

    let output = compiler.finish();

    let res = output.emit().unwrap();

    // todo: store the output in the correct location
    std::fs::write("program.o", res).expect("failed to write code to file");
}
