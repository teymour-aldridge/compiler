use std::collections::HashMap;

use crate::{
    id::{tag, Id, TaggedAst},
    parse::parse,
    ty::type_check,
    visitor::Visitor,
};

use super::{Ty, TyEnv};
#[cfg(not(disable_fuzzcheck))]
use fuzzcheck::{
    mutators::{integer_within_range::U8WithinRangeMutator, map::MapMutator, vector::VecMutator},
    DefaultMutator, Mutator,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(not(disable_fuzzcheck), derive(DefaultMutator))]
/// The starting case. This will be grown by using the transformations provided.
pub enum Start {
    Ty {
        name: VarName,
        ty: Ty,
        transforms: Vec<TyTransform>,
    },
}

impl Start {
    fn program(&self) -> String {
        match self {
            Self::Ty {
                name,
                ty,
                transforms,
            } => {
                let mut program = String::new();

                let mut prev = match ty {
                    Ty::Int => "1",
                    Ty::Bool => "False",
                    Ty::String => "\"some string\"",
                }
                .to_string();
                for tranform in transforms {
                    match tranform {
                        TyTransform::IntroduceVar { name } => {
                            let string = format!("{}={}\n", name.inner, prev);
                            program += &string;
                            prev = name.inner.to_string();
                        }
                    }
                }
                program += &format!("{}={}", name.inner, prev);
                program
            }
        }
    }

    fn check(&self, env: TyEnv, ast: &TaggedAst) -> bool {
        struct VariableVisitor {
            /// Maps the names of variables to their corresponding ids.
            map: HashMap<String, Id>,
        }

        impl<'a, 'ctx> Visitor<'a, 'ctx> for VariableVisitor {
            type Output = ();

            fn visit_for(&mut self, stmt: &crate::id::TaggedFor) -> Self::Output {
                self.visit_ident(&stmt.var);
                self.visit_block(&stmt.block);
            }

            fn visit_if(&mut self, stmt: &crate::id::TaggedIf) -> Self::Output {
                self.visit_expr(&stmt.r#if.condition);
                self.visit_block(&stmt.r#if.block);

                for else_if in &stmt.else_ifs {
                    self.visit_expr(&else_if.condition);
                    self.visit_block(&else_if.block);
                }

                stmt.r#else.as_ref().map(|x| self.visit_block(x));
            }

            fn visit_expr(&mut self, expr: &crate::id::TaggedExpr) -> Self::Output {
                match &expr.token {
                    crate::id::TaggedExprInner::Ident(ident) => self.visit_ident(ident),
                    crate::id::TaggedExprInner::BinOp(_, a, b) => {
                        self.visit_expr(a);
                        self.visit_expr(b);
                    }
                    crate::id::TaggedExprInner::UnOp(_, expr) => {
                        self.visit_expr(expr);
                    }
                    crate::id::TaggedExprInner::FunctionCall(name, params) => {
                        self.visit_ident(name);
                        for param in params {
                            self.visit_expr(param);
                        }
                    }
                    _ => (),
                }
            }

            fn visit_ret(&mut self, ret: &crate::id::TaggedReturn) -> Self::Output {
                self.visit_expr(&ret.expr)
            }

            fn visit_func(&mut self, func: &crate::id::TaggedFunc) -> Self::Output {
                self.visit_ident(&func.name);
                for each in &func.parameters {
                    self.visit_ident(each);
                }
            }

            fn visit_ident(&mut self, ident: &crate::id::TaggedIdent) -> Self::Output {
                self.map.insert(ident.token.to_string(), ident.id);
            }

            fn visit_while(&mut self, stmt: &crate::id::TaggedWhile) -> Self::Output {
                self.visit_expr(&stmt.condition);
                self.visit_block(&stmt.block);
            }

            fn visit_rec(&mut self, _rec: &'a crate::id::TaggedRecord<'ctx>) -> Self::Output {
                todo!()
            }
        }

        let mut visitor = VariableVisitor {
            map: Default::default(),
        };

        visitor.visit_ast(ast);

        let map = visitor.map;

        for (_, value) in map {
            let right = env.ty_of(value)
                == Some(*match self {
                    Start::Ty {
                        name: _,
                        ty,
                        transforms: _,
                    } => ty,
                });
            if !right {
                return false;
            }
        }
        true
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(not(disable_fuzzcheck), derive(DefaultMutator))]
pub enum TyTransform {
    /// Create a new variable.
    ///
    /// For example, the program
    /// ```ignore
    /// x = 1
    /// ```
    /// under the transformation `IntroduceVar("y")` would become
    /// ```
    /// y = 1
    /// x = y
    /// ```
    IntroduceVar { name: VarName },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarName {
    inner: String,
}

impl VarName {
    #[cfg(not(disable_fuzzcheck))]
    fn mutator() -> impl Mutator<VarName> {
        MapMutator::new(
            VecMutator::new(U8WithinRangeMutator::new(65..=90), 1..=100),
            |name: &VarName| Some(name.inner.as_bytes().to_vec()),
            |xs| VarName {
                inner: String::from_utf8_lossy(xs).to_string(),
            },
            |_, complexity| complexity,
        )
    }
}

#[cfg(not(disable_fuzzcheck))]
impl DefaultMutator for VarName {
    type Mutator = impl Mutator<VarName>;

    fn default_mutator() -> Self::Mutator {
        Self::mutator()
    }
}

fn perform_test(input: &Start) -> bool {
    let program = input.program();
    let ast = parse(&program).unwrap();
    let tagged = tag(ast);
    let env = type_check(&tagged).unwrap();
    input.check(env, &tagged)
}

#[test]
#[cfg(not(disable_fuzzcheck))]
fn test_type_checker() {
    let result = fuzzcheck::fuzz_test(perform_test)
        .default_mutator()
        .serde_serializer()
        .default_sensor_and_pool()
        .arguments_from_cargo_fuzzcheck()
        .stop_after_first_test_failure(false)
        .launch();
    assert!(!result.found_test_failure)
}

#[test]
fn regression_1() {
    // {"Ty":{"name":{"inner":"N"},"ty":"String","transforms":[{"IntroduceVar":{"name":{"inner":"R"}}}]}}
    assert!(perform_test(&Start::Ty {
        name: VarName {
            inner: "N".to_string(),
        },
        ty: Ty::String,
        transforms: vec![TyTransform::IntroduceVar {
            name: VarName {
                inner: "R".to_string(),
            },
        }],
    }))
}

#[test]
fn regression_2() {
    assert!(perform_test(&Start::Ty {
        name: VarName {
            inner: "B".to_string()
        },
        ty: Ty::Bool,
        transforms: vec![]
    }));
}
