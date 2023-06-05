//! The Cranelift backend for the Amp compiler.
//!
//! This phase converts the AIR indirectly to native machine code through the Cranelift code
//! generator.  In the future, there will be another intermediate pass in between for optimization
//! called the MIR (mid-level intermediate representation).  This pass will also resolve generic
//! function calls to separate functions, so it can easily be translated to Cranelift IR.

use std::collections::HashMap;

mod cranelift {
    pub use cranelift::prelude::*;
    pub use cranelift::*;
    pub use cranelift_module::*;
    pub use cranelift_native as native;
    pub use cranelift_object as object;
}

use ::cranelift::prelude::{Configurable, InstBuilder};
use target_lexicon::Triple;

use crate::{
    sema::air::{self, Air},
    types::{FuncSig, Type},
    value::{FuncId, Value},
};

/// Compiles the provided AIR into an object file for the host system.
pub fn compile_air_to_obj_file(air: Air) -> Vec<u8> {
    let mut flag_builder = cranelift::settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();
    let isa_builder = cranelift_native::builder()
        .unwrap_or_else(|_| {
            panic!(
                "Could not create an object module for the current target: {}",
                Triple::host()
            )
        })
        .finish(cranelift::settings::Flags::new(flag_builder))
        .unwrap();

    let module = cranelift::object::ObjectModule::new(
        cranelift::object::ObjectBuilder::new(
            isa_builder,
            [1, 2, 3, 4],
            cranelift::default_libcall_names(),
        )
        .unwrap(),
    );

    let mut clif_backend = ClifBackend::new(module);
    clif_backend.lower(air);
    clif_backend.module.finish().emit().unwrap()
}

/// The Cranelift function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClifFunc {
    /// The ID of the function.
    pub id: cranelift::FuncId,

    /// The Cranelift signature of the function.
    pub signature: cranelift::Signature,
}

/// The state of the Cranelift backend.
pub struct ClifBackend<Module: cranelift::Module> {
    /// The module to store functions in.
    pub module: Module,

    /// Maps Amp [FuncId]s to their associated Cranelift functions.
    pub clif_func_map: HashMap<FuncId, ClifFunc>,
}

impl<Module: cranelift_module::Module> ClifBackend<Module> {
    /// Creates a new [ClifBackend] for the provided module.
    #[inline]
    pub fn new(module: Module) -> Self {
        Self {
            module,
            clif_func_map: HashMap::new(),
        }
    }

    /// Attempts to convert a function signature from Amp into a Cranelift function signature.
    fn lower_func_sig(&mut self, sig: &FuncSig) -> Option<cranelift::Signature> {
        let mut clif_sig = self.module.make_signature();

        for param in &sig.params {
            clif_sig
                .params
                .push(cranelift::AbiParam::new(self.lower_type(param)?));
        }

        // TODO: check if return type is void
        clif_sig
            .returns
            .push(cranelift::AbiParam::new(self.lower_type(&sig.returns)?));

        Some(clif_sig)
    }

    /// Converts an Amp type to a Cranelift type, if possible.  Types such as [Type::Type] have no
    /// possible conversion.
    fn lower_type(&mut self, ty: &Type) -> Option<cranelift::Type> {
        match ty {
            Type::U8 => Some(cranelift::types::I8),
            Type::I32 => Some(cranelift::types::I32),
            Type::ThinPtr(_) => Some(self.module.target_config().pointer_type()),

            // Cannot map the Amp type to a Cranelift type.
            _ => None,
        }
    }

    /// Lowers the provided function call statement, returning the produced instruction.
    fn lower_func_call(
        &mut self,
        clif_builder: &mut cranelift::FunctionBuilder,
        call: &air::Call,
    ) -> cranelift::codegen::ir::Inst {
        let mut values = Vec::new();

        for value in &call.params {
            values.push(
                self.lower_expr(clif_builder, value)
                    .expect("should be a valid cranelift supported value"),
            );
        }

        match &call.callee {
            air::Expr::Const(_, value) => match value {
                Value::Func(id) => {
                    let clif_func_id = self.clif_func_map.get(id).unwrap().id;
                    let clif_func_ref = self
                        .module
                        .declare_func_in_func(clif_func_id, clif_builder.func);
                    clif_builder.ins().call(clif_func_ref, &values)
                }
                _ => unreachable!("no other constant values can be used as functions"),
            },
            _ => todo!("function as values (indirect function calls)"),
        }
    }

    /// Attempts to lower an expression into Cranelift IR.  Returns the value produced by the
    /// expression if it was successfully converted into Cranelift IR.
    fn lower_expr(
        &mut self,
        clif_builder: &mut cranelift::FunctionBuilder,
        expr: &air::Expr,
    ) -> Option<cranelift::Value> {
        match expr {
            air::Expr::Const(_, value) => match value {
                Value::U8(value) => Some(
                    clif_builder
                        .ins()
                        .iconst(cranelift::types::I8, *value as i64),
                ),
                Value::I32(value) => Some(
                    clif_builder
                        .ins()
                        .iconst(cranelift::types::I32, *value as i64),
                ),
                Value::Nullterm(value) => {
                    // TODO: move to separate function
                    let mut str = value.clone();
                    str.push('\0');

                    let mut clif_data_context = cranelift::DataContext::new();
                    clif_data_context.define(str.into_bytes().into_boxed_slice());

                    let data_id = self.module.declare_anonymous_data(true, false).unwrap();
                    self.module
                        .define_data(data_id, &clif_data_context)
                        .unwrap();

                    let clif_global_value =
                        self.module.declare_data_in_func(data_id, clif_builder.func);
                    Some(clif_builder.ins().global_value(
                        self.module.target_config().pointer_type(),
                        clif_global_value,
                    ))
                }
                // TODO: functions as values
                _ => None,
            },
            air::Expr::Call(_, call) => {
                let inst = self.lower_func_call(clif_builder, call);
                Some(clif_builder.inst_results(inst)[0])
            }
        }
    }

    /// Lowers an AIR statement into Cranelift IR.  Returns `true` if the instruction terminates
    /// and the compiler can skip the implicit return.
    fn lower_stmnt(
        &mut self,
        clif_builder: &mut cranelift::FunctionBuilder,
        stmnt: &air::Stmnt,
    ) -> bool {
        match stmnt {
            air::Stmnt::Return(ret) => {
                let mut values = Vec::new();
                match &ret.value {
                    Some(value) => values.push(
                        self.lower_expr(clif_builder, value)
                            .expect("trust that it's a valid cranelift supported value"),
                    ),
                    None => {}
                };

                clif_builder.ins().return_(&values);
                true
            }
            air::Stmnt::Call(call) => {
                self.lower_func_call(clif_builder, call);
                false
            }
        }
    }

    /// Lowers a code block into Cranelift IR.  Returns `true` if the block terminated by itself.
    fn lower_block(
        &mut self,
        clif_builder: &mut cranelift::FunctionBuilder,
        stmnts: &Vec<air::Stmnt>,
    ) -> bool {
        let mut terminates = false;
        for stmnt in stmnts {
            terminates = self.lower_stmnt(clif_builder, stmnt) || terminates;
        }
        terminates
    }

    /// Lowers the provided AIR unit into the Cranelift module that the [ClifBackend] was
    /// initialized with.
    pub fn lower(&mut self, air: Air) {
        // Declare all AIR functions in the Cranelift module.
        for (id, func) in air.funcs.iter() {
            let clif_sig = self
                .lower_func_sig(&func.signature)
                .expect("all possible types in Amp should be supported by Cranelift");

            let clif_id = if let Some(name) = &func.extern_name {
                self.module
                    .declare_function(&name, cranelift::Linkage::Export, &clif_sig)
            } else {
                self.module.declare_anonymous_function(&clif_sig)
            }
            .unwrap();

            self.clif_func_map.insert(
                FuncId(id.to_raw()),
                ClifFunc {
                    id: clif_id,
                    signature: clif_sig,
                },
            );
        }

        let mut clif_context = self.module.make_context();
        let mut clif_builder_context = cranelift::FunctionBuilderContext::new();

        // Convert the AIR functions to Cranelift IR.
        for (id, func) in air.funcs.iter() {
            if let Some(def) = &func.def {
                // the function is defined locally
                let mut clif_func = cranelift::codegen::ir::Function::new();
                clif_func.signature = self
                    .clif_func_map
                    .get(&FuncId(id.to_raw()))
                    .unwrap()
                    .signature
                    .clone();
                let mut clif_builder =
                    cranelift::FunctionBuilder::new(&mut clif_func, &mut clif_builder_context);

                // Set up the entry block for the function.
                {
                    let entry_block = clif_builder.create_block();
                    clif_builder.append_block_params_for_function_params(entry_block);
                    clif_builder.switch_to_block(entry_block);
                }

                if !self.lower_block(&mut clif_builder, &def.insts) {
                    // implicit return
                    match &func.signature.returns {
                        ty if ty.is_int() => {
                            let default = clif_builder.ins().iconst(
                                self.lower_type(ty)
                                    .expect("function must return cranelift compatible type"),
                                0,
                            );
                            clif_builder.ins().return_(&[default]);
                        }
                        Type::Func(_) | Type::ThinPtr(_) => {
                            let default = clif_builder
                                .ins()
                                .iconst(self.module.target_config().pointer_type(), 0);
                            clif_builder.ins().return_(&[default]);
                        }
                        _ => unreachable!("function cannot return invalid type"),
                    }
                }

                clif_builder.seal_all_blocks();
                clif_builder.finalize();

                // TODO: add toggle for printing Cranelift IR
                // Print the Cranelift IR of the function.
                // println!(
                //     "{}{}",
                //     if let Some(name) = &func.extern_name {
                //         format!("({}): ", name)
                //     } else {
                //         "".to_string()
                //     },
                //     clif_func.display()
                // );

                clif_context.func = clif_func;
                self.module
                    .define_function(
                        self.clif_func_map.get(&FuncId(id.to_raw())).unwrap().id,
                        &mut clif_context,
                    )
                    .unwrap();

                self.module.clear_context(&mut clif_context);
            }
        }
    }
}
