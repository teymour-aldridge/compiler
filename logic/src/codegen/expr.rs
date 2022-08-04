use cranelift_codegen::ir::{self, condcodes::IntCC, AbiParam, InstBuilder};
use cranelift_frontend::Variable;
use cranelift_module::{DataContext, Linkage, Module};

use crate::{
    diagnostics::{reportable_error::ReportableError, span::HasSpan},
    parse::{
        expr::{BinOp, Expr},
        lit::Literal,
        table::{ParseTable, WithId},
    },
    ty::{PrimitiveType, Ty},
};

use super::{compile::cranelift_of_ty_module, func::FunctionCompiler};

impl<'i, 'builder> FunctionCompiler<'i, 'builder> {
    /// Lowers an expression to the corresponding Cranelift IR.
    pub(crate) fn compile_expr(
        &mut self,
        expr: WithId<&Expr>,
        table: &ParseTable,
    ) -> Result<ir::Value, ReportableError> {
        Ok(match &expr.inner() {
            // todo: use https://github.com/bytecodealliance/wasmtime/pull/4588
            // to report better errors here
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

                    self.builder.ins().iconst(ir::types::I64, number.as_int())
                }
                crate::parse::lit::Literal::Bool(boolean) => {
                    self.builder.ins().bconst(ir::types::B1, *boolean)
                }
            },
            Expr::BinOp(op, left, right)
                if op.token == BinOp::SetEquals && table.get_expr(left).is_ident() =>
            {
                let id = table.get_ident_with_id(*table.get_expr(left).as_ident().unwrap());

                let new_value = self.compile_expr(table.get_expr_with_id(*right), table)?;

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
                let val = self.compile_expr(table.get_expr_with_id(*right), table)?;
                let addr = self.compile_expr(
                    table.get_expr_with_id(*table.get_expr(left).as_un_op().unwrap().1),
                    table,
                )?;
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
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table)?;
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table)?;
                    self.builder.ins().iadd(lhs, rhs)
                }
                BinOp::Subtract => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table)?;
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table)?;
                    self.builder.ins().isub(lhs, rhs)
                }
                BinOp::Divide => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table)?;
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table)?;
                    self.builder.ins().udiv(lhs, rhs)
                }
                BinOp::Multiply => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table)?;
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table)?;
                    self.builder.ins().imul(lhs, rhs)
                }
                BinOp::IsEqual => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table)?;
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table)?;
                    let left_ty = self.ty_env.ty_of(left.id).ok_or_else(|| {
                        ReportableError::could_not_infer_ty(table.get_expr(left).span(table))
                    })?;
                    let right_ty = self.ty_env.ty_of(right.id).ok_or_else(|| {
                        ReportableError::could_not_infer_ty(table.get_expr(left).span(table))
                    })?;
                    match (left_ty, right_ty) {
                        (
                            Ty::PrimitiveType(PrimitiveType::Int),
                            Ty::PrimitiveType(PrimitiveType::Int),
                        ) => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        (
                            Ty::PrimitiveType(PrimitiveType::Bool),
                            Ty::PrimitiveType(PrimitiveType::Bool),
                        ) => {
                            let truth = self.builder.ins().bconst(ir::types::B1, true);
                            // exclusive or
                            // A | B | Output
                            // T | T | F
                            // T | F | T
                            // F | T | T
                            // F | F | F
                            // essentially, if A == B is true then A XOR B is false (and if A==B is false,
                            // then A XOR B is true)
                            // to compare if two boolean values are equal we want to negate A XOR B
                            // which we can do by evaluating A XOR True
                            let xor_res = self.builder.ins().bxor(lhs, rhs);
                            self.builder.ins().bxor(xor_res, truth)
                        }
                        _ => todo!(),
                    }
                }
                BinOp::IsNotEqual => {
                    let lhs = self.compile_expr(table.get_expr_with_id(*left), table)?;
                    let rhs = self.compile_expr(table.get_expr_with_id(*right), table)?;
                    let left_ty = self.ty_env.ty_of(left.id).unwrap();
                    let right_ty = self.ty_env.ty_of(right.id).unwrap();
                    match (left_ty, right_ty) {
                        (
                            Ty::PrimitiveType(PrimitiveType::Int),
                            Ty::PrimitiveType(PrimitiveType::Int),
                        ) => self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        // see the implementation of `IsEqual` (above) for documentation
                        (
                            Ty::PrimitiveType(PrimitiveType::Bool),
                            Ty::PrimitiveType(PrimitiveType::Bool),
                        ) => self.builder.ins().bxor(lhs, rhs),
                        _ => todo!(),
                    }
                }
                BinOp::SetEquals => unreachable!(),
                BinOp::Dot => {
                    log::trace!("compiling dot expression with id {:?}", expr.id());

                    let record = table.get_expr_with_id(*left);
                    let record_id = match record.inner().as_ident() {
                        Some(r) => r.id,
                        None => {
                            return Err(ReportableError::new(
                                record.inner.span(table),
                                "This variable has been used, but it was never defined.".to_owned(),
                            ))
                        }
                    };
                    let key = table.get_expr_with_id(*right).inner().as_ident().unwrap();

                    log::trace!(
                        "field being accessed is `{}` (id: {})",
                        table.get_ident(*key).inner(),
                        key.id
                    );

                    let record = match self.ty_env.ty_of(record_id).unwrap() {
                        Ty::Record { ref_ } => table.get_record(ref_),
                        _ => {
                            // todo: report the correct error
                            todo!()
                        }
                    };

                    let mut offset = 0;
                    for field in &record.fields {
                        if table.get_ident(*key).inner() != table.get_ident(field.name).inner() {
                            offset +=
                                super::layout::type_size(self.ty_env.ty_of(field.name.id).unwrap());
                        } else {
                            break;
                        }
                    }

                    let field_pseudo_ty = self.ty_env.ty_of(key.id).unwrap();

                    let field_clif_type =
                        // todo: resolve fields properly
                        if matches!(field_pseudo_ty, Ty::PrimitiveType(PrimitiveType::Bool)) {
                            ir::types::I32
                        } else {
                            cranelift_of_ty_module(self.module, field_pseudo_ty)
                        };

                    let pointer = self.builder.use_var(Variable::with_u32(record_id.as_u32()));
                    let value = self.builder.ins().load(
                        field_clif_type,
                        ir::MemFlags::new(),
                        pointer,
                        offset as i32,
                    );
                    if matches!(field_pseudo_ty, Ty::PrimitiveType(PrimitiveType::Bool)) {
                        self.builder.ins().icmp_imm(IntCC::Equal, value, 1)
                    } else {
                        value
                    }
                }
                BinOp::Index => match self.ty_env.ty_of(expr.id()).unwrap() {
                    Ty::PrimitiveType(PrimitiveType::Pointer) => {
                        let lhs = self.compile_expr(table.get_expr_with_id(*left), table)?;
                        let rhs = self.compile_expr(table.get_expr_with_id(*right), table)?;
                        return Ok(self.builder.ins().iadd(lhs, rhs));
                    }
                    _ => todo!(),
                },
            },
            Expr::UnOp(op, arg) if op.token.is_deref() => {
                let arg_value = self.compile_expr(table.get_expr_with_id(*arg), table)?;
                self.builder.ins().load(
                    cranelift_of_ty_module(self.module, self.ty_env.ty_of(expr.id()).unwrap()),
                    ir::MemFlags::new(),
                    arg_value,
                    0,
                )
            }
            Expr::UnOp(op, arg) => match op.token {
                crate::parse::expr::UnOp::Positive => {
                    // todo: check that the integer is an integer
                    self.compile_expr(table.get_expr_with_id(*arg), table)?
                }
                crate::parse::expr::UnOp::Negative => {
                    // todo: check that the integer is an integer
                    let integer = self.compile_expr(table.get_expr_with_id(*arg), table)?;
                    self.builder.ins().ineg(integer)
                }
                crate::parse::expr::UnOp::Deref => {
                    unreachable!("Pointer dereferencing should have been handled separately.")
                }
            },
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
                    "print_bool" => {
                        let mut sig = self.module.make_signature();
                        sig.params.push(AbiParam::new(ir::types::I32));
                        sig.returns.push(AbiParam::new(cranelift_of_ty_module(
                            self.module,
                            Ty::PrimitiveType(PrimitiveType::Int),
                        )));
                        let func_id = self
                            .module
                            .declare_function("print_bool", Linkage::Import, &sig)
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

                        let string = self.compile_expr(table.get_expr_with_id(*string), table)?;
                        let len = self.builder.ins().iconst(ir::types::I64, lit.len() as i64);

                        let call = self.builder.ins().call(local_callee, &[len, string]);
                        return Ok(self.builder.inst_results(call)[0]);
                    }
                    _ => {
                        let mut sig = self.module.make_signature();

                        for param in params {
                            sig.params.push(AbiParam::new(
                                match self
                                    .ty_env
                                    .ty_of(param.id)
                                    .map(|x| cranelift_of_ty_module(self.module, x))
                                {
                                    Some(ty) => ty,
                                    None => {
                                        return Err(ReportableError::new(
                                            table.get_expr(param).span(table),
                                            "The type of this function parameter could not be inferred.".to_owned()
                                        ))
                                    }
                                },
                            ))
                        }

                        sig.returns.push(AbiParam::new(
                            match self
                                .ty_env
                                .ty_of(name.id)
                                .map(|x| cranelift_of_ty_module(self.module, x))
                            {
                                Some(ty) => ty,
                                None => {
                                    return Err(ReportableError::new(
                                        table.get_ident(*name).span(table),
                                        "The return type of this function could not be inferred."
                                            .to_owned(),
                                    ))
                                }
                            },
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
                    .map(|param| {
                        let value = self.compile_expr(table.get_expr_with_id(*param), table)?;
                        Ok(
                            if matches!(
                                self.ty_env.ty_of(param.id),
                                Some(Ty::PrimitiveType(PrimitiveType::Bool))
                            ) && table.get_ident(*name).inner() == "print_bool"
                            {
                                self.builder.ins().bint(ir::types::I32, value)
                            } else {
                                value
                            },
                        )
                    })
                    .collect::<Result<Vec<ir::Value>, ReportableError>>()?;

                let call = self.builder.ins().call(local_callee, &arg_values);
                self.builder.inst_results(call)[0]
            }
            Expr::Constructor(con) => self.compile_constructor(con, table)?,
        })
    }
}
