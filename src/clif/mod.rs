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
                // TODO: functions as values
                _ => None,
            },
        }
    }

    /// Lowers an AIR statement into Cranelift IR.
    fn lower_stmnt(&mut self, clif_builder: &mut cranelift::FunctionBuilder, stmnt: &air::Stmnt) {
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
            }
        }
    }

    /// Lowers a code block into Cranelift IR.
    fn lower_block(
        &mut self,
        clif_builder: &mut cranelift::FunctionBuilder,
        stmnts: &Vec<air::Stmnt>,
    ) {
        for stmnt in stmnts {
            self.lower_stmnt(clif_builder, stmnt);
        }
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

            // TODO: store signature information, etc.
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
                self.lower_block(&mut clif_builder, &def.insts);

                clif_builder.seal_all_blocks();
                clif_builder.finalize();

                // Print the Cranelift IR of the function.
                println!(
                    "{}{}",
                    if let Some(name) = &func.extern_name {
                        format!("({}): ", name)
                    } else {
                        "".to_string()
                    },
                    clif_func.display()
                );

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