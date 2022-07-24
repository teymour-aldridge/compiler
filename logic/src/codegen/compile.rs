//! Emits machine code from the provided instructions.
//!
//! This uses the Cranelift code generator to produce the code.
//!
//! todo: report errors properly

use std::env;

use cranelift_codegen::{
    entity::EntityRef,
    ir::{self, condcodes::IntCC, AbiParam, InstBuilder},
    Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectModule, ObjectProduct};

use crate::{
    codegen::make_module::make_module_for_compiler_host_architecture,
    parse::{
        expr::Expr,
        func::Return,
        lit::Literal,
        r#for::ForLoop,
        r#if::If,
        r#while::While,
        table::{Item, ItemKind, ParseTable, WithId},
    },
    ty::PrimitiveType,
};
use crate::{
    parse::expr::BinOp,
    ty::{Ty, TyEnv},
};

/// The core compiler struct.
pub struct Compiler<'i> {
    context: Context,
    ty_env: &'i TyEnv,
    module: ObjectModule,
}

// todo: pointer types
fn cranelift_of_ty_module(module: &ObjectModule, ty: Ty) -> ir::Type {
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
    pub fn new(ty_env: &'i TyEnv) -> Self {
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
    pub fn compile(&mut self, table: &ParseTable<'i>) {
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

            function_compiler.compile_block(table.get_block(&func.block), table);

            function_compiler.builder.finalize();

            if env::var("PRINT_IR").is_ok() {
                println!("{}", function_compiler.builder.func);
            }

            self.module
                .define_function(func_id, &mut self.context)
                .unwrap();

            self.module.clear_context(&mut self.context);
        }
    }

    pub fn finish(self) -> ObjectProduct {
        self.module.finish()
    }
}

/// Compiles one specific function.
pub(crate) struct FunctionCompiler<'i, 'builder> {
    pub(crate) builder: &'builder mut FunctionBuilder<'i>,
    pub(crate) ty_env: &'i TyEnv,
    pub(crate) module: &'builder mut ObjectModule,
}

impl<'i, 'builder> FunctionCompiler<'i, 'builder> {
    fn new(
        function: &'builder mut FunctionBuilder<'i>,
        ty_env: &'i TyEnv,
        module: &'builder mut ObjectModule,
    ) -> Self {
        Self {
            builder: function,
            ty_env,
            module,
        }
    }

    /// Transforms the provided series of instructions into Cranelift IR.
    fn compile_block(&mut self, block: &crate::parse::block::Block, table: &ParseTable) {
        for block in &block.inner {
            match table.get(block).unwrap() {
                Item::Expr(e) => {
                    self.compile_expr(
                        WithId {
                            inner: e,
                            id: block.id,
                        },
                        table,
                    );
                }
                Item::For(f) => self.compile_for(f, table),
                Item::If(i) => self.compile_if(i, table),
                Item::While(w) => self.compile_while(w, table),
                Item::Return(r) => self.compile_return(r, table),
                Item::Func(_) => {
                    panic!("should have checked this error before now!");
                }
                Item::Record(_) => todo!(),
                Item::Ident(_) => unreachable!(),
                Item::Block(b) => self.compile_block(b, table),
            };
        }
    }

    /// Compiles a return statement.
    fn compile_return(&mut self, ret: &Return, table: &ParseTable) {
        let return_value = self.compile_expr(table.get_expr_with_id(ret.expr), table);
        self.builder.ins().return_(&[return_value]);
    }

    /// Lowers an expression to the corresponding Cranelift IR.
    pub(crate) fn compile_expr(&mut self, expr: WithId<&Expr>, table: &ParseTable) -> ir::Value {
        match &expr.inner() {
            Expr::Ident(ident) => self.builder.use_var(Variable::with_u32(ident.id.as_u32())),
            Expr::Literal(lit) => match &lit.token {
                crate::parse::lit::Literal::String(lit) => {
                    let mut data_ctx = DataContext::new();
                    data_ctx.define(lit.as_bytes().to_vec().into_boxed_slice());
                    let id = self
                        .module
                        .declare_data(
                            &format!("__data_{}", expr.id()),
                            Linkage::Local,
                            true,
                            false,
                        )
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
            Expr::BinOp(op, left, right)
                if op.token == BinOp::SetEquals && table.get_expr(left).is_ident() =>
            {
                let id = table.get_ident_with_id(*table.get_expr(left).as_ident().unwrap());

                let new_value = self.compile_expr(table.get_expr_with_id(*right), table);

                let cranelift_ty = match self.ty_env.ty_of(right.id).unwrap() {
                    Ty::PrimitiveType(PrimitiveType::Int) => {
                        cranelift_of_ty_module(self.module, Ty::PrimitiveType(PrimitiveType::Int))
                    }
                    Ty::PrimitiveType(PrimitiveType::Bool | PrimitiveType::StrSlice) => todo!(),
                    Ty::Record { .. } | Ty::PrimitiveType(PrimitiveType::Pointer) => {
                        self.module.target_config().pointer_type()
                    }
                };

                let var = Variable::with_u32(id.id().as_u32());

                self.builder.declare_var(var, cranelift_ty);
                self.builder.def_var(var, new_value);
                new_value
            }
            Expr::BinOp(op, ref left, ref right)
                if op.token == BinOp::SetEquals
                    && table
                        .get_expr(left)
                        .as_un_op()
                        .map(|(op, _)| op.is_deref())
                        .unwrap_or(false) =>
            {
                let val = self.compile_expr(table.get_expr_with_id(*right), table);
                let addr = self.compile_expr(
                    table.get_expr_with_id(*table.get_expr(left).as_un_op().unwrap().1),
                    table,
                );
                self.builder.ins().store(ir::MemFlags::new(), val, addr, 0);
                self.builder.ins().load(
                    cranelift_of_ty_module(self.module, self.ty_env.ty_of(expr.id()).unwrap()),
                    ir::MemFlags::new(),
                    addr,
                    0,
                )
            }
            /*
             * compile standard operators
             * TODO: at some point add operator overloading with protocol classes
             */
            Expr::BinOp(op, left, right) => match op.token {
                BinOp::Add => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table);
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table);
                    self.builder.ins().iadd(lhs, rhs)
                }
                BinOp::Subtract => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table);
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table);
                    self.builder.ins().isub(lhs, rhs)
                }
                BinOp::Divide => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table);
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table);
                    self.builder.ins().udiv(lhs, rhs)
                }
                BinOp::Multiply => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table);
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table);
                    self.builder.ins().imul(lhs, rhs)
                }
                BinOp::IsEqual => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table);
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table);
                    let left_ty = self.ty_env.ty_of(left.id).unwrap();
                    let right_ty = self.ty_env.ty_of(right.id).unwrap();
                    match (left_ty, right_ty) {
                        (
                            Ty::PrimitiveType(PrimitiveType::Int),
                            Ty::PrimitiveType(PrimitiveType::Int),
                        ) => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        _ => todo!(),
                    }
                }
                BinOp::IsNotEqual => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table);
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table);
                    let left_ty = self.ty_env.ty_of(left.id).unwrap();
                    let right_ty = self.ty_env.ty_of(right.id).unwrap();
                    match (left_ty, right_ty) {
                        (
                            Ty::PrimitiveType(PrimitiveType::Int),
                            Ty::PrimitiveType(PrimitiveType::Int),
                        ) => self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        _ => todo!(),
                    }
                }
                BinOp::SetEquals => unreachable!(),
                BinOp::Dot => {
                    let record = table.get_expr_with_id(*left);
                    let record_id = record.inner().as_ident().unwrap().id;
                    let key = table.get_expr_with_id(*right).inner().as_ident().unwrap();

                    let record_ty = match self.ty_env.ty_of(record_id).unwrap() {
                        Ty::Record { ref_ } => ref_,
                        _ => {
                            // todo: report the correct error
                            todo!()
                        }
                    };

                    let mut offset = 0;
                    for field in &table.get_record(record_ty).fields {
                        if table.get_ident(*key).inner() != table.get_ident(field.name).inner() {
                            offset +=
                                super::layout::type_size(self.ty_env.ty_of(field.name.id).unwrap());
                        } else {
                            break;
                        }
                    }

                    let field_type = {
                        // todo: resolve fields properly
                        let ty = self.ty_env.ty_of(key.id).unwrap();
                        cranelift_of_ty_module(self.module, ty)
                    };

                    let pointer = self.builder.use_var(Variable::with_u32(record_id.as_u32()));
                    self.builder
                        .ins()
                        .load(field_type, ir::MemFlags::new(), pointer, offset as i32)
                }
                BinOp::Index => match self.ty_env.ty_of(expr.id()).unwrap() {
                    Ty::PrimitiveType(PrimitiveType::Pointer) => {
                        let lhs = self.compile_expr(table.get_expr_with_id(*left), table);
                        let rhs = self.compile_expr(table.get_expr_with_id(*right), table);
                        return self.builder.ins().iadd(lhs, rhs);
                    }
                    _ => todo!(),
                },
            },
            Expr::UnOp(op, arg) if op.token.is_deref() => {
                let arg_value = self.compile_expr(table.get_expr_with_id(*arg), table);
                self.builder.ins().load(
                    cranelift_of_ty_module(self.module, self.ty_env.ty_of(expr.id()).unwrap()),
                    ir::MemFlags::new(),
                    arg_value,
                    0,
                )
            }
            Expr::UnOp(_, _) => todo!(),
            Expr::FunctionCall(name, params) => {
                let local_callee = match table.get_ident(*name).inner() {
                    "print_int" => {
                        let mut sig = self.module.make_signature();
                        sig.params.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Int),
                        )));
                        sig.returns.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Int),
                        )));
                        let func_id = self
                            .module
                            .declare_function("print_int", Linkage::Import, &sig)
                            .unwrap();

                        self.module.declare_func_in_func(func_id, self.builder.func)
                    }
                    "malloc" => {
                        let mut sig = self.module.make_signature();
                        sig.params.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Int),
                        )));
                        sig.returns.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Pointer),
                        )));
                        let func_id = self
                            .module
                            .declare_function("malloc", Linkage::Import, &sig)
                            .unwrap();

                        self.module.declare_func_in_func(func_id, self.builder.func)
                    }
                    "realloc" => {
                        let mut sig = self.module.make_signature();
                        sig.params.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Pointer),
                        )));
                        sig.params.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Int),
                        )));

                        sig.returns.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Pointer),
                        )));
                        let func_id = self
                            .module
                            .declare_function("realloc", Linkage::Import, &sig)
                            .unwrap();

                        self.module.declare_func_in_func(func_id, self.builder.func)
                    }
                    "free" => {
                        let mut sig = self.module.make_signature();
                        sig.params.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Pointer),
                        )));
                        sig.returns.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Pointer),
                        )));
                        let func_id = self
                            .module
                            .declare_function("free", Linkage::Import, &sig)
                            .unwrap();

                        self.module.declare_func_in_func(func_id, self.builder.func)
                    }
                    "print" => {
                        let mut sig = self.module.make_signature();
                        sig.params.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Int),
                        )));
                        sig.params.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::StrSlice),
                        )));
                        sig.returns.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Int),
                        )));

                        let func_id = self
                            .module
                            .declare_function("print", Linkage::Import, &sig)
                            .unwrap();
                        let local_callee =
                            self.module.declare_func_in_func(func_id, self.builder.func);

                        let string = params.iter().next().unwrap();

                        let lit = match table.get_expr(string) {
                            Expr::Literal(lit) if matches!(lit.token, Literal::String(_)) => {
                                lit.as_string().unwrap()
                            }
                            _ => panic!("Only string literals can be printed (for now)."),
                        };

                        let string = self.compile_expr(table.get_expr_with_id(*string), table);
                        let len = self.builder.ins().iconst(ir::types::I64, lit.len() as i64);

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
                            .declare_function(table.get_ident(*name).inner(), Linkage::Import, &sig)
                            .expect("problem declaring function");

                        self.module.declare_func_in_func(callee, self.builder.func)
                    }
                };
                let arg_values = params
                    .iter()
                    .map(|param| self.compile_expr(table.get_expr_with_id(*param), table))
                    .collect::<Vec<_>>();

                let call = self.builder.ins().call(local_callee, &arg_values);
                self.builder.inst_results(call)[0]
            }
            Expr::Constructor(con) => self.compile_constructor(con, table),
        }
    }

    fn compile_if(&mut self, stmt: &If, table: &ParseTable) {
        let condition_value = self.compile_expr(table.get_expr_with_id(stmt.r#if.condition), table);

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

        self.compile_block(table.get_block(&stmt.r#if.block), table);

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        if let Some(else_block) = &stmt.r#else {
            self.compile_block(table.get_block(else_block), table);
        }
    }

    fn compile_for(&self, _: &ForLoop, _: &ParseTable) {
        panic!("compilation of for loops is not yet implemented")
    }

    fn compile_while(&mut self, stmt: &While, table: &ParseTable) {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);
        self.builder.switch_to_block(header_block);

        let condition_value = self.compile_expr(table.get_expr_with_id(stmt.condition), table);

        self.builder.ins().brz(condition_value, exit_block, &[]);
        self.builder.ins().jump(body_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);

        self.compile_block(table.get_block(&stmt.block), table);

        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(exit_block);
        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);
    }
}
