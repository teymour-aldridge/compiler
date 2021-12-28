//! Emits machine code from the provided instructions.
//!
//! This uses the Cranelift code generator to produce the code.
//!
//! todo: report errors properly

use std::{convert::TryInto, env};

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

/// The core compiler struct.
pub struct Compiler<'ctx> {
    context: Context,
    ty_env: &'ctx TyEnv<'ctx>,
    module: ObjectModule,
}

// todo: pointer types
fn cranelift_of_ty_module(module: &ObjectModule, ty: Ty) -> ir::Type {
    match ty {
        Ty::Int => ir::Type::int(64).unwrap(),
        Ty::Bool => ir::Type::int(8).unwrap().as_bool(),
        Ty::String => module.target_config().pointer_type(),
        // todo: compile structs (using stack slots)
        Ty::Record(_) => todo!(),
        Ty::Pointer => module.target_config().pointer_type(),
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

    /// Transforms the provided AST into Cranelift IR.
    pub fn compile(&mut self, ast: &TaggedAst) {
        let functions = ast.nodes.iter().filter_map(|item| match item {
            crate::parse::Node::Func(func) => Some(func),
            _ => None,
        });

        let mut function_builder_context = FunctionBuilderContext::new();

        for func in functions {
            // set up the signature
            let returns = if *func.name.token == "print_int" || *func.name.token == "print" {
                self.cranelift_of_ty(Ty::Int)
            } else {
                self.ty_env
                    .ty_of(func.name.id.into())
                    .map(|x| self.cranelift_of_ty(x))
                    .unwrap()
            };
            let returns = AbiParam::new(returns);
            self.context.func.signature.returns = vec![returns];

            let parameters = func
                .parameters
                .iter()
                .map(|ident| self.ty_env.ty_of(ident.id.into()).unwrap())
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
                        .ty_of(param.id.into())
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

            if env::var("PRINT_IR").is_ok() {
                println!("{}", function_compiler.builder.func);
            }

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
pub(crate) struct FunctionCompiler<'ctx, 'builder> {
    pub(crate) builder: &'builder mut FunctionBuilder<'ctx>,
    pub(crate) ty_env: &'ctx TyEnv<'ctx>,
    pub(crate) module: &'builder mut ObjectModule,
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
                crate::parse::Node::Record(_) => todo!(),
            };
        }
    }

    /// Compiles a return statement.
    fn compile_return(&mut self, ret: &TaggedReturn<'_>) {
        let return_value = self.compile_expr(&ret.expr);
        self.builder.ins().return_(&[return_value]);
    }

    /// Lowers an expression to the corresponding Cranelift IR.Ï
    pub(crate) fn compile_expr(&mut self, expr: &TaggedExpr) -> ir::Value {
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
                        .iconst(ir::types::I64, number.as_int() as i64)
                }
                crate::parse::lit::Literal::Bool(_) => todo!(),
            },
            crate::id::TaggedExprInner::BinOp(op, left, right)
                if op.token == BinOp::SetEquals && left.is_ident() =>
            {
                let id = match &left.token {
                    crate::id::TaggedExprInner::Ident(ident) => ident.id,
                    _ => unreachable!(),
                };

                let new_value = self.compile_expr(right);

                let cranelift_ty = match self.ty_env.ty_of(right.id.into()).unwrap() {
                    Ty::Int => cranelift_of_ty_module(&self.module, Ty::Int),
                    Ty::Bool | Ty::String => todo!(),
                    Ty::Record(_) | Ty::Pointer => self.module.target_config().pointer_type(),
                };

                let var = Variable::with_u32(id.raw_id() as u32);

                self.builder.declare_var(var, cranelift_ty);
                self.builder.def_var(var, new_value);
                new_value
            }
            crate::id::TaggedExprInner::BinOp(op, ref left, ref right)
                if op.token == BinOp::SetEquals
                    && left
                        .as_un_op()
                        .map(|(op, _)| op.is_deref())
                        .unwrap_or(false) =>
            {
                let val = self.compile_expr(right);
                let addr = self.compile_expr(left.as_un_op().unwrap().1);
                self.builder.ins().store(ir::MemFlags::new(), val, addr, 0);
                self.builder.ins().load(
                    cranelift_of_ty_module(
                        &self.module,
                        self.ty_env.ty_of(expr.id.into()).unwrap(),
                    ),
                    ir::MemFlags::new(),
                    addr,
                    0,
                )
            }
            /*
             * compile standard operators
             * TODO: at some point add operator overloading with protocol classes
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
                    let left_ty = self.ty_env.ty_of(left.id.into()).unwrap();
                    let right_ty = self.ty_env.ty_of(right.id.into()).unwrap();
                    match (left_ty, right_ty) {
                        (Ty::Int, Ty::Int) => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        _ => unimplemented!(),
                    }
                }
                BinOp::SetEquals => unreachable!(),
                BinOp::Dot => {
                    if let (Some(record), Some(field)) =
                        (left.token.as_ident(), right.token.as_ident())
                    {
                        let record_ty = match self.ty_env.ty_of(record.id.into()).unwrap() {
                            Ty::Record(rec) => rec,
                            _ => {
                                // todo: report the correct error
                                todo!()
                            }
                        };

                        let mut offset = 0;
                        for (key, value) in &record_ty {
                            if key != &field.token().inner() {
                                offset += super::layout::type_size(value.clone());
                            } else {
                                break;
                            }
                        }

                        let field_type = {
                            // todo: resolve fields properly
                            let ty = self.ty_env.ty_of(field.id.into()).unwrap();
                            cranelift_of_ty_module(&self.module, ty)
                        };

                        let pointer = self
                            .builder
                            .use_var(Variable::with_u32(record.id.raw_id() as u32));
                        self.builder.ins().load(
                            field_type,
                            ir::MemFlags::new(),
                            pointer,
                            offset as i32,
                        )
                    } else {
                        // report a proper error
                        todo!()
                    }
                }
                BinOp::Index => match self.ty_env.ty_of(expr.id.into()).unwrap() {
                    Ty::Pointer => {
                        let lhs = self.compile_expr(left);
                        let rhs = self.compile_expr(right);
                        return self.builder.ins().iadd(lhs, rhs);
                    }
                    _ => todo!(),
                },
            },
            crate::id::TaggedExprInner::UnOp(op, arg) if op.token.is_deref() => {
                let arg_value = self.compile_expr(arg);
                dbg!(&expr);
                self.builder.ins().load(
                    cranelift_of_ty_module(self.module, self.ty_env.ty_of(expr.id.into()).unwrap()),
                    ir::MemFlags::new(),
                    arg_value,
                    0,
                )
            }
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
                    "malloc" => {
                        let mut sig = self.module.make_signature();
                        sig.params
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Int)));
                        sig.returns
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Pointer)));
                        let func_id = self
                            .module
                            .declare_function("malloc", Linkage::Import, &sig)
                            .unwrap();

                        self.module.declare_func_in_func(func_id, self.builder.func)
                    }
                    "realloc" => {
                        let mut sig = self.module.make_signature();
                        sig.params
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Pointer)));
                        sig.params
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Int)));

                        sig.returns
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Pointer)));
                        let func_id = self
                            .module
                            .declare_function("realloc", Linkage::Import, &sig)
                            .unwrap();

                        self.module.declare_func_in_func(func_id, self.builder.func)
                    }
                    "free" => {
                        let mut sig = self.module.make_signature();
                        sig.params
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Pointer)));
                        sig.returns
                            .push(AbiParam::new(cranelift_of_ty_module(self.module, Ty::Int)));
                        let func_id = self
                            .module
                            .declare_function("free", Linkage::Import, &sig)
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
                        let len = self.builder.ins().iconst(ir::types::I64, lit.len() as i64);

                        let call = self.builder.ins().call(local_callee, &[len, string]);
                        return self.builder.inst_results(call)[0];
                    }
                    _ => {
                        let mut sig = self.module.make_signature();

                        for param in params {
                            sig.params.push(AbiParam::new(
                                self.ty_env
                                    .ty_of(param.id.into())
                                    .map(|x| cranelift_of_ty_module(self.module, x))
                                    .unwrap(),
                            ))
                        }

                        sig.returns.push(AbiParam::new(
                            self.ty_env
                                .ty_of(name.id.into())
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
            crate::id::TaggedExprInner::Constructor(con) => self.compile_constructor(con),
        }
    }

    fn compile_if(&mut self, stmt: &TaggedIf) {
        let condition_value = self.compile_expr(&stmt.r#if.condition);

        let if_block = self.builder.create_block();

        if !stmt.else_ifs.is_empty() {
            // todo: proper warning API
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
