use cranelift_codegen::ir::InstBuilder;
use cranelift_frontend::FunctionBuilder;
use cranelift_jit::JITModule;

use crate::{
    diagnostics::{
        reportable_error::{ReportableError, ReportableResult},
        span::HasSpan,
    },
    parse::{
        func::Return,
        r#for::ForLoop,
        r#if::If,
        r#while::While,
        table::{Item, ParseTable, WithId},
    },
    ty::TyEnv,
};

/// Translates an individual function into Cranelift IR.
pub(crate) struct FunctionCompiler<'i, 'builder> {
    pub(crate) builder: &'builder mut FunctionBuilder<'i>,
    pub(crate) ty_env: &'i TyEnv,
    pub(crate) module: &'builder mut JITModule,
}

impl<'i, 'builder> FunctionCompiler<'i, 'builder> {
    pub(crate) fn new(
        function: &'builder mut FunctionBuilder<'i>,
        ty_env: &'i TyEnv,
        module: &'builder mut JITModule,
    ) -> Self {
        Self {
            builder: function,
            ty_env,
            module,
        }
    }

    /// Transforms the provided series of instructions into Cranelift IR.
    pub(crate) fn compile_block(
        &mut self,
        block: &crate::parse::block::Block,
        table: &ParseTable,
    ) -> ReportableResult {
        for block in &block.inner {
            match table.get(block).unwrap() {
                Item::Expr(e) => {
                    if !self.builder.is_filled() {
                        self.compile_expr(
                            WithId {
                                inner: e,
                                id: block.id,
                            },
                            table,
                        )?;
                    } else {
                        // todo: report this properly
                        if std::env::var("FUZZCHECK").is_err() {
                            println!("warning: anny statements added after a return instruction have no effect")
                        }
                    }
                }
                Item::For(f) => self.compile_for(f, table),
                Item::If(i) => self.compile_if(i, table)?,
                Item::While(w) => self.compile_while(w, table)?,
                Item::Return(r) => self.compile_return(r, table)?,
                Item::Func(_) => {
                    panic!("should have checked this error before now!");
                }
                Item::Record(rec) => {
                    // todo: this restriction may be lifted in the future
                    return Err(ReportableError::new(
                        table.get_ident(rec.name).span(table),
                        "Record definitions are not allowed inside functions.".to_owned(),
                    ));
                }
                Item::Ident(_) => unreachable!(),
                Item::Block(b) => self.compile_block(b, table)?,
            };
        }

        Ok(())
    }

    /// Compiles a return statement.
    pub(crate) fn compile_return(&mut self, ret: &Return, table: &ParseTable) -> ReportableResult {
        let return_value = self.compile_expr(table.get_expr_with_id(ret.expr), table)?;
        self.builder.ins().return_(&[return_value]);
        Ok(())
    }

    pub(crate) fn compile_if(&mut self, stmt: &If, table: &ParseTable) -> ReportableResult {
        let condition_value =
            self.compile_expr(table.get_expr_with_id(stmt.r#if.condition), table)?;

        let if_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        // all instructions exit through this block
        let exit_block = self.builder.create_block();

        if !stmt.else_ifs.is_empty() && std::env::var("FUZZCHECK").is_err() {
            // todo: proper warning API
            println!("WARNING: else if is not yet supported!");
        }

        self.builder.ins().brz(condition_value, else_block, &[]);
        self.builder.ins().jump(if_block, &[]);

        self.builder.switch_to_block(if_block);
        self.builder.seal_block(if_block);

        self.compile_block(table.get_block(&stmt.r#if.block), table)?;

        // the block we just compiled might have included a terminator instruction; if it didn't
        // then we jump to the exit block
        if !self.builder.is_filled() {
            self.builder.ins().jump(exit_block, &[]);
        }

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        if let Some(else_block) = &stmt.r#else {
            self.compile_block(table.get_block(else_block), table)?;
        }

        // the block we just compiled might have included a terminator instruction; if it didn't
        // then we jump to the exit block
        if !self.builder.is_filled() {
            self.builder.ins().jump(exit_block, &[]);
        }

        self.builder.switch_to_block(exit_block);
        self.builder.seal_block(exit_block);

        assert!(!self.builder.is_filled());

        Ok(())
    }

    pub(crate) fn compile_for(&self, _: &ForLoop, _: &ParseTable) {
        panic!("compilation of for loops is not yet implemented")
    }

    pub(crate) fn compile_while(&mut self, stmt: &While, table: &ParseTable) -> ReportableResult {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.compile_expr(table.get_expr_with_id(stmt.condition), table)?;

        self.builder.ins().brz(condition_value, exit_block, &[]);
        self.builder.ins().jump(body_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        self.compile_block(table.get_block(&stmt.block), table)?;

        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);
        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);

        assert!(!self.builder.is_filled());

        Ok(())
    }
}
