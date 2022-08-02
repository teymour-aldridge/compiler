//! Emits machine code from the provided instructions.
//!
//! This uses the Cranelift code generator to produce the code.
//!
//! todo: report errors properly

use std::env;

use cranelift_codegen::{
    entity::EntityRef,
    ir::{self, AbiParam},
    Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};

use crate::{
    codegen::make_module::make_module_for_compiler_host_architecture,
    diagnostics::{reportable_error::ReportableError, span::HasSpan},
    parse::table::{ItemKind, ParseTable},
    ty::PrimitiveType,
};
use crate::{
    diagnostics::reportable_error::ReportableResult,
    ty::{Ty, TyEnv},
};

use super::func::FunctionCompiler;

/// The core compiler struct.
pub struct Compiler<'i> {
    context: Context,
    ty_env: &'i TyEnv,
    table: &'i ParseTable<'i>,
    module: JITModule,
}

// todo: pointer types
pub fn cranelift_of_ty_module(module: &JITModule, ty: Ty) -> ir::Type {
    match ty {
        Ty::PrimitiveType(PrimitiveType::Int) => ir::Type::int(64).unwrap(),
        Ty::PrimitiveType(PrimitiveType::Bool) => ir::Type::int(8).unwrap().as_bool(),
        Ty::PrimitiveType(PrimitiveType::StrSlice) => module.target_config().pointer_type(),
        // todo: compile structs (using stack slots)
        Ty::Record { ref_: _ } => todo!(),
        Ty::PrimitiveType(PrimitiveType::Pointer) => module.target_config().pointer_type(),
    }
}

impl<'i> Compiler<'i> {
    /// Create a new instance of the compiler.
    pub fn new(ty_env: &'i TyEnv, table: &'i ParseTable<'i>) -> Self {
        let module = make_module_for_compiler_host_architecture();
        Self {
            context: module.make_context(),
            ty_env,
            table,
            module,
        }
    }

    /// Convert the given type into the corresponding Cranelift type.
    ///
    /// TODO: add support for more complex objects
    fn cranelift_of_ty(&self, ty: Ty) -> ir::Type {
        cranelift_of_ty_module(&self.module, ty)
    }

    /// Transforms the provided AST into Cranelift IR.
    pub fn compile(&mut self, table: &ParseTable<'i>) -> ReportableResult {
        let functions = table.root.1.inner.iter().filter_map(|item| {
            if let ItemKind::Func = item.item_kind {
                table.func.get(&item.id)
            } else {
                None
            }
        });

        let mut function_builder_context = FunctionBuilderContext::new();

        for func in functions {
            // set up the signature
            let returns = if table.get_ident(func.name).inner() == "print_int"
                || table.get_ident(func.name).inner() == "print"
            {
                self.cranelift_of_ty(Ty::PrimitiveType(PrimitiveType::Int))
            } else if let Some(ty) = self
                .ty_env
                .ty_of(func.name.id)
                .map(|x| self.cranelift_of_ty(x))
            {
                ty
            } else {
                return Err(ReportableError::new(
                    table.get_ident(func.name).span(table),
                    "The return type of this function could not be deduced.".to_owned(),
                ));
            };
            let returns = AbiParam::new(returns);
            self.context.func.signature.returns = vec![returns];

            let mut parameters = Vec::with_capacity(func.parameters.len());

            for ident in &func.parameters {
                if let Some(ty) = self.ty_env.ty_of(ident.id) {
                    parameters.push(AbiParam::new(self.cranelift_of_ty(ty)));
                } else {
                    return Err(ReportableError::new(
                        self.table.get_ident(*ident).span(table),
                        "A type of variable could not be established for this function parameter."
                            .to_owned(),
                    ));
                }
            }

            let parameters = func
                .parameters
                .iter()
                .map(|ident| self.ty_env.ty_of(ident.id).unwrap())
                .map(|x| self.cranelift_of_ty(x))
                .map(AbiParam::new)
                .collect::<Vec<_>>();
            for param in parameters {
                self.context.func.signature.params.push(param);
            }

            let func_id = self
                .module
                .declare_function(
                    table.get_ident(func.name).inner(),
                    Linkage::Export,
                    &self.context.func.signature,
                )
                .unwrap();

            let mut function_builder =
                FunctionBuilder::new(&mut self.context.func, &mut function_builder_context);

            let entry_block = function_builder.create_block();

            function_builder.append_block_params_for_function_params(entry_block);

            function_builder.switch_to_block(entry_block);

            function_builder.seal_block(entry_block);

            for (i, param) in func.parameters.iter().enumerate() {
                let var = Variable::new(param.id.as_u32() as usize);
                function_builder.declare_var(
                    var,
                    self.ty_env
                        .ty_of(param.id)
                        .map(|x| cranelift_of_ty_module(&self.module, x))
                        .unwrap(),
                );

                let val = function_builder.block_params(entry_block)[i];

                function_builder.def_var(var, val);
            }

            let mut function_compiler =
                FunctionCompiler::new(&mut function_builder, self.ty_env, &mut self.module);

            function_compiler.compile_block(table.get_block(&func.block), table)?;

            function_compiler.builder.finalize();

            if env::var("PRINT_IR").is_ok() {
                println!("{}", function_compiler.builder.func);
            }

            self.module
                .define_function(func_id, &mut self.context)
                .unwrap();

            self.module.clear_context(&mut self.context);
        }

        Ok(())
    }

    pub fn finish(mut self) -> *const u8 {
        self.module.finalize_definitions();
        let main_func = match self.module.get_name("main").unwrap() {
            cranelift_module::FuncOrDataId::Func(func) => func,
            cranelift_module::FuncOrDataId::Data(_) => {
                panic!("should not have data with name `main`")
            }
        };
        self.module.get_finalized_function(main_func)
    }
}
