use std::convert::TryInto;

/// Emits machine code from the provided instructions.
///
/// This uses the Cranelift code generator to produce the code. Unfortunately the Cranelift docs
/// are not very extensive. Be warned; a lot of this is guesswork.
use cranelift_codegen::{
    binemit::{NullStackMapSink, NullTrapSink},
    entity::EntityRef,
    ir::{self, condcodes::IntCC, AbiParam, InstBuilder},
    Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectModule, ObjectProduct};

use crate::{
    codegen::make_module::make_module_for_compiler_host_architecture, parse::lit::Literal,
};
use crate::{
    id::{TaggedAst, TaggedBlock, TaggedExpr, TaggedFor, TaggedIf, TaggedReturn, TaggedWhile},
    parse::expr::BinOp,
    ty::{Ty, TyEnv},
};

/// The compiler.
///
/// Note that the callee must satisfy the lifetime `'ctx`.
pub struct Compiler<'ctx> {
    context: Context,
    ty_env: &'ctx TyEnv,
    module: ObjectModule,
}

fn cranelift_of_ty_module(module: &ObjectModule, ty: Ty) -> ir::Type {
    match ty {
        Ty::Int => ir::Type::int(32).unwrap(),
        Ty::Bool => ir::Type::int(8).unwrap().as_bool(),
        Ty::String => module.target_config().pointer_type(),
    }
}

impl<'ctx> Compiler<'ctx> {
    /// Create a new instance of the compiler.
    pub fn new(ty_env: &'ctx TyEnv) -> Self {
        let module = make_module_for_compiler_host_architecture();
        Self {
            context: module.make_context(),
            ty_env,
            module,
        }
    }

    /// Convert the given type into the corresponding Cranelift type.
    ///
    /// TODO: add support for more complex objects
    fn cranelift_of_ty(&self, ty: Ty) -> ir::Type {
        cranelift_of_ty_module(&self.module, ty)
    }

    /// Transforms the provided AST into Cranelift IR, and then returns a finished module.
    pub fn compile(&mut self, ast: &TaggedAst) {
        let functions = ast.nodes.iter().filter_map(|item| match item {
            crate::parse::Node::Func(func) => Some(func),
            _ => None,
        });

        let mut function_builder_context = FunctionBuilderContext::new();

        for func in functions {
            // set up the signature
            let returns = if *func.name.token == "print_int" {
                self.cranelift_of_ty(Ty::Int)
            } else if *func.name.token == "print" {
                self.cranelift_of_ty(Ty::Int)
            } else {
                self.ty_env
                    .ty_of(func.name.id)
                    .map(|x| self.cranelift_of_ty(x))
                    .unwrap()
            };
            let returns = AbiParam::new(returns);
            self.context.func.signature.returns = vec![returns];

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
                    *func.name.token,
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
                let var = Variable::new(param.id.raw_id());
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

            function_compiler.compile_block(&func.block);

            function_compiler.builder.finalize();

            self.module
                .define_function(
                    func_id,
                    &mut self.context,
                    &mut NullTrapSink {},
                    &mut NullStackMapSink {},
                )
                .unwrap();

            self.module.clear_context(&mut self.context);
        }
    }

    pub fn finish(self) -> ObjectProduct {
        self.module.finish()
    }
}

/// Compiles one specific function.
struct FunctionCompiler<'ctx, 'builder> {
    builder: &'builder mut FunctionBuilder<'ctx>,
    ty_env: &'ctx TyEnv,
    module: &'builder mut ObjectModule,
}

impl<'ctx, 'builder> FunctionCompiler<'ctx, 'builder> {
    fn new(
        function: &'builder mut FunctionBuilder<'ctx>,
        ty_env: &'ctx TyEnv,
        module: &'builder mut ObjectModule,
    ) -> Self {
        Self {
            builder: function,
            ty_env,
            module,
        }
    }

    /// Transforms the provided series of instructions into Cranelift IR.
    fn compile_block(&mut self, block: &TaggedBlock<'_>) {
        for block in &block.inner.nodes {
            match block {
                crate::parse::Node::Expr(e) => {
                    self.compile_expr(e);
                }
                crate::parse::Node::For(f) => self.compile_for(f),
                crate::parse::Node::If(i) => self.compile_if(i),
                crate::parse::Node::While(w) => self.compile_while(w),
                crate::parse::Node::Return(r) => self.compile_return(r),
                crate::parse::Node::Func(_) => {
                    panic!("should have checked this error before now!");
                }
            };
        }
    }

    fn compile_return(&mut self, ret: &TaggedReturn<'_>) {
        let return_value = self.compile_expr(&ret.expr);
        self.builder.ins().return_(&[return_value]);
    }

    /// Compiles an expression into the appropriate (that's my hope, at least) Cranelift IR.
    fn compile_expr(&mut self, expr: &TaggedExpr) -> ir::Value {
        match &expr.token {
            crate::id::TaggedExprInner::Ident(ident) => self
                .builder
                .use_var(Variable::with_u32(ident.id.raw_id().try_into().unwrap())),
            crate::id::TaggedExprInner::Literal(lit) => match &lit.token {
                crate::parse::lit::Literal::String(lit) => {
                    let mut data_ctx = DataContext::new();
                    data_ctx.define(lit.as_bytes().to_vec().into_boxed_slice());
                    let id = self
                        .module
                        .declare_data(&format!("__data_{}", expr.id), Linkage::Local, true, false)
                        .unwrap();
                    self.module.define_data(id, &data_ctx).unwrap();
                    drop(data_ctx);
                    let local_id = self.module.declare_data_in_func(id, self.builder.func);
                    let pointer = self.module.target_config().pointer_type();
                    self.builder.ins().symbol_value(pointer, local_id)
                }
                crate::parse::lit::Literal::Number(number) => {
                    if number.float.is_some() {
                        panic!("Floats are not yet supported.")
                    }
                    if number.exp.is_some() {
                        panic!("Exponents are not yet supported!")
                    }

                    self.builder
                        .ins()
                        .iconst(ir::types::I32, number.as_int() as i64)
                }
                crate::parse::lit::Literal::Bool(_) => todo!(),
            },
            crate::id::TaggedExprInner::BinOp(op, left, right) if op.token == BinOp::SetEquals => {
                let id = match &left.token {
                    crate::id::TaggedExprInner::Ident(ident) => ident.id,
                    _ => unreachable!(),
                };

                let new_value = self.compile_expr(right);
                self.builder
                    .def_var(Variable::with_u32(id.raw_id() as u32), new_value);
                new_value
            }
            /*
             * compile standard operators
             * TODO: at some point add object orientation (for operator overloading)
             */
            crate::id::TaggedExprInner::BinOp(op, left, right) => match op.token {
                BinOp::Add => {
                    let lhs = self.compile_expr(left);
                    let rhs = self.compile_expr(right);
                    self.builder.ins().iadd(lhs, rhs)
                }
                BinOp::Subtract => {
                    let lhs = self.compile_expr(left);
                    let rhs = self.compile_expr(right);
                    self.builder.ins().isub(lhs, rhs)
                }
                BinOp::Divide => {
                    let lhs = self.compile_expr(left);
                    let rhs = self.compile_expr(right);
                    self.builder.ins().udiv(lhs, rhs)
                }
                BinOp::Multiply => {
                    let lhs = self.compile_expr(left);
                    let rhs = self.compile_expr(right);
                    self.builder.ins().imul(lhs, rhs)
                }
                BinOp::IsEqual => {
                    let lhs = self.compile_expr(left);
                    let rhs = self.compile_expr(right);
                    let left_ty = self.ty_env.ty_of(left.id).unwrap();
                    let right_ty = self.ty_env.ty_of(right.id).unwrap();
                    match (left_ty, right_ty) {
                        (Ty::Int, Ty::Int) => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        _ => unimplemented!(),
                    }
                }
                BinOp::SetEquals => unreachable!(),
            },
            crate::id::TaggedExprInner::UnOp(_, _) => todo!(),
            crate::id::TaggedExprInner::FunctionCall(name, params) => {
                let local_callee = match *name.token {
                    "print_int" => {
                        let mut sig = self.module.make_signature();
                        sig.params
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Int)));
                        sig.returns
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Int)));
                        let func_id = self
                            .module
                            .declare_function("print_int", Linkage::Import, &sig)
                            .unwrap();

                        self.module.declare_func_in_func(func_id, self.builder.func)
                    }
                    "print" => {
                        let mut sig = self.module.make_signature();
                        sig.params
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Int)));
                        sig.params.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::String,
                        )));
                        sig.returns
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Int)));

                        let func_id = self
                            .module
                            .declare_function("print", Linkage::Import, &sig)
                            .unwrap();
                        let local_callee =
                            self.module.declare_func_in_func(func_id, self.builder.func);

                        let string = params.iter().next().unwrap();

                        let lit = match string.token {
                            crate::id::TaggedExprInner::Literal(ref x) => match x.token {
                                Literal::String(ref string) => string,
                                _ => panic!("Only string literals can be printed (for now)."),
                            },
                            _ => panic!("Only string literals can be printed (for now)."),
                        };

                        let string = self.compile_expr(string);
                        let len = self.builder.ins().iconst(ir::types::I32, lit.len() as i64);

                        let call = self.builder.ins().call(local_callee, &[len, string]);
                        return self.builder.inst_results(call)[0];
                    }
                    _ => {
                        let mut sig = self.module.make_signature();

                        for param in params {
                            sig.params.push(AbiParam::new(
                                self.ty_env
                                    .ty_of(param.id)
                                    .map(|x| cranelift_of_ty_module(self.module, x))
                                    .unwrap(),
                            ))
                        }

                        sig.returns.push(AbiParam::new(
                            self.ty_env
                                .ty_of(name.id)
                                .map(|x| cranelift_of_ty_module(self.module, x))
                                .unwrap(),
                        ));

                        let callee = self
                            .module
                            .declare_function(*name.token, Linkage::Import, &sig)
                            .expect("problem declaring function");

                        self.module.declare_func_in_func(callee, self.builder.func)
                    }
                };
                let arg_values = params
                    .iter()
                    .map(|param| self.compile_expr(param))
                    .collect::<Vec<_>>();

                let call = self.builder.ins().call(local_callee, &arg_values);
                self.builder.inst_results(call)[0]
            }
        }
    }

    fn compile_if(&mut self, stmt: &TaggedIf) {
        let condition_value = self.compile_expr(&stmt.r#if.condition);

        let if_block = self.builder.create_block();

        if !stmt.else_ifs.is_empty() {
            println!("WARNING: else if is not yet supported!");
        }

        let else_block = self.builder.create_block();

        self.builder.ins().brz(condition_value, else_block, &[]);
        self.builder.ins().jump(if_block, &[]);

        self.builder.switch_to_block(if_block);
        self.builder.seal_block(if_block);

        self.compile_block(&stmt.r#if.block);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        if let Some(else_block) = &stmt.r#else {
            self.compile_block(else_block);
        }
    }

    fn compile_for(&self, _: &TaggedFor) {
        panic!("compilation of for loops is not yet implemented")
    }

    fn compile_while(&mut self, stmt: &TaggedWhile) {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.compile_expr(&stmt.condition);

        self.builder.ins().brz(condition_value, exit_block, &[]);
        self.builder.ins().jump(body_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        self.compile_block(&stmt.block);

        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);
        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);
    }
}
