//! The semantic analysis phase.
//!
//! This phase lowers Amp modules into AIR (Amp intermediate representation) which stores type
//! information.
//!
//! # Passes
//! Currently, `import`/`using` statements and `static` variables aren't implemented, so there
//! aren't as many passes now as there will be in the future.
//!
//! 1. Declare the names of root `const` bindings, ensuring there are no duplicate names.
//!    Disregard type/value information at this phase, as it's not important.
//! (note: there would be a pass here to evaluate `where` statements in the root of a module, which
//! would lazily evaluate any `const` values)
//! 2. Evaluate the types and values as of global `const` bindings.

pub mod air;
pub mod scope;

use slot_arena::{Ref, SlotArena};

use crate::{
    codemap::{Span, Spanned},
    diag::SemaDiagnostics,
    syntax::ast,
    types::{FuncSig, Mutable, Type},
    value::{FuncId, Value},
    Context,
};

use self::{
    air::{Air, Stmnt},
    scope::{Scope, ScopedRef},
};

/// The state of the semantic analysis unit.
///
/// A unit dissociates declarations from their modules, merging them all together.
#[derive(Debug)]
pub struct Unit {
    /// The constants declared in this [Unit].
    ///
    /// Past the semantic analysis phase, all uses of `const` declarations are inlined, for
    /// example:
    /// ```amp
    /// const MyValue = 42;
    /// var my_value = MyValue * 2;
    /// ```
    ///
    /// Becomes:
    /// ```amp
    /// var my_value = 42 * 2;
    /// ```
    ///
    /// This means that this list can be discarded as it is not used by the next phases.
    pub consts: SlotArena<Const>,

    /// The functions declared in this [Unit].
    pub funcs: SlotArena<air::Func>,

    /// The function currently being evaluated.
    ///
    /// Should not be changed outside of the [Module] implementation.
    pub current_func: Ref<air::Func>,
}

impl Unit {
    /// Creates an empty [Unit].
    #[inline]
    pub fn new() -> Self {
        Self {
            consts: SlotArena::new(),
            funcs: SlotArena::new(),
            current_func: Ref::from_raw(0),
        }
    }

    /// Returns the span of the provided item, if any.
    pub fn span_of_scoped_ref(&self, item: ScopedRef) -> Option<Span> {
        match item {
            ScopedRef::Const(item) => self.consts.get(item).span,
        }
    }

    /// Initializes the provided scope with Amp primitives, such as the `i32` and `u8` types.
    ///
    /// Should only be called once per [Unit], to prevent memory leaks.
    pub fn initialize_scope_with_primitives(&mut self, scope: &mut Scope) {
        {
            let type_ = self.consts.insert(Const {
                name: "type".into(),
                span: None,
                value: Some((Type::Type, Value::Type(Type::Type))),
            });
            scope.insert("type", ScopedRef::Const(type_));
        }

        {
            let u8 = self.consts.insert(Const {
                name: "u8".into(),
                span: None,
                value: Some((Type::Type, Value::Type(Type::U8))),
            });
            scope.insert("u8", ScopedRef::Const(u8));
        }

        {
            let i32 = self.consts.insert(Const {
                name: "i32".into(),
                span: None,
                value: Some((Type::Type, Value::Type(Type::I32))),
            });
            scope.insert("i32", ScopedRef::Const(i32));
        }
    }

    /// Analyzes the provided module, lowering it to AIR.
    ///
    /// Should only be called once per [Unit].
    pub fn analyze(mut self, cx: &mut Context, source: ast::Stmnts) -> Result<Air, ()> {
        let mut global_scope = Scope::new();
        self.initialize_scope_with_primitives(&mut global_scope);

        let mut module = Module::new(source);
        module.scope_mut().parent(Some(&global_scope));

        module.declare_root_const_names(cx, &mut self)?;
        module.define_root_const_values(cx, &mut self)?;

        Ok(Air { funcs: self.funcs })
    }

    /// Analyzes the provided statement, converting it into AIR.
    pub fn analyze_stmnt(
        &mut self,
        cx: &mut Context,
        scope: &mut Scope,
        stmnt: &ast::Expr,
    ) -> Result<air::Stmnt, ()> {
        match stmnt {
            ast::Expr::Return(expr) => Ok(air::Stmnt::Return(air::Return::from_ast(
                cx, self, scope, expr,
            )?)),
            ast::Expr::Call(expr) => Ok(air::Stmnt::Call(self.analyze_call(cx, scope, expr)?.1)),
            _ => todo!("report expressions that aren't supported as statements"),
        }
    }

    /// Analyzes a block of code.
    ///
    /// Creates a subscope from the provided scope.
    pub fn analyze_block(
        &mut self,
        cx: &mut Context,
        scope: &Scope,
        block: &ast::Block,
    ) -> Result<Vec<Stmnt>, ()> {
        let mut scope = scope.subscope();
        let mut stmnts = Vec::new();

        for stmnt in &block.stmnts.stmnts {
            stmnts.push(self.analyze_stmnt(cx, &mut scope, stmnt)?);
        }

        Ok(stmnts)
    }

    pub fn analyze_call(
        &mut self,
        cx: &mut Context,
        scope: &Scope,
        call: &ast::Call,
    ) -> Result<(Type, air::Call), ()> {
        let callee = IntermediateExpr::verify(cx, self, scope, &call.callee)?;
        let callee_type = callee
            .default_type()
            .expect("TODO: don't expect default type when values like `uninit` exist");

        match &callee_type {
            Type::Func(func_sig) => {
                let expected_len = func_sig.params.len();
                let got_len = call.args.items.len();
                if expected_len != got_len {
                    cx.function_call_argument_length_mismatch(
                        expected_len,
                        got_len,
                        &callee_type.name(),
                        call.span(),
                    );
                    return Err(());
                }

                let mut params = Vec::new();

                for (idx, arg) in call.args.items.iter().enumerate() {
                    let ty = &func_sig.params[idx];
                    let value = IntermediateExpr::verify(cx, self, scope, arg)?
                        .coerce(ty)
                        .expect("TODO: report argument type mismatch");

                    params.push(value);
                }

                Ok((
                    func_sig.returns.clone(),
                    air::Call {
                        callee: callee.infer().expect("should have a default type"),
                        params,
                    },
                ))
            }
            _ => {
                cx.calling_non_function_type(&callee_type.name(), call.span());
                Err(())
            }
        }
    }
}

/// A module of Amp source, to be lowered into AIR.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module<'root> {
    /// The source AST of the module.
    source: ast::Stmnts,

    /// The scope of items in this module.
    scope: Scope<'root>,
}

impl<'root> Module<'root> {
    /// Creates a new [Module] from the provided AST statements.
    #[inline]
    pub fn new(source: ast::Stmnts) -> Self {
        Self {
            source,
            scope: Scope::new(),
        }
    }

    /// Returns the scope of this [Module].
    #[inline]
    pub fn scope(&self) -> &Scope {
        &self.scope
    }

    /// Returns the scope of this [Module].
    #[inline]
    pub fn scope_mut(&mut self) -> &mut Scope<'root> {
        &mut self.scope
    }

    /// Declares the root `const` binding names in this module.  Also checks if there are any
    /// invalid items declared at the root of this module.
    pub fn declare_root_const_names(
        &mut self,
        cx: &mut Context,
        unit: &mut Unit,
    ) -> Result<(), ()> {
        let mut res = Ok(());

        for item in &self.source.stmnts {
            match item {
                ast::Expr::Const(decl) => {
                    if let Some(item) = self.scope.get(&decl.name.value) {
                        cx.duplicate_declaration(
                            &decl.name.value,
                            decl.span(),
                            unit.span_of_scoped_ref(item),
                        );
                        res = Err(());
                    } else {
                        let id = unit.consts.insert(Const {
                            span: Some(decl.span()),
                            name: decl.name.value.clone(),
                            value: None,
                        });
                        self.scope.insert(&decl.name.value, ScopedRef::Const(id));
                    }
                }
                _ => todo!("throw error when invalid items are found"),
            }
        }

        res
    }

    /// Defines the values of `const` declarations in the root.
    pub fn define_root_const_values(
        &mut self,
        cx: &mut Context,
        unit: &mut Unit,
    ) -> Result<(), ()> {
        for item in &self.source.stmnts {
            match item {
                ast::Expr::Const(decl) => {
                    let ScopedRef::Const(const_id) = self
                        .scope
                        .get(&decl.name.value)
                        .expect("verified in last pass");

                    if unit.consts.get(const_id).value.is_none() {
                        // TODO: move `const` value evaluation to separate method
                        let intermediate =
                            IntermediateExpr::verify(cx, unit, &self.scope, &decl.value)?;

                        let ty = if let Some(ty) = &decl.ty {
                            let final_ty = Type::from_ast(cx, unit, &self.scope, &ty.ty)?;
                            final_ty
                        } else {
                            intermediate
                                .default_type()
                                .expect("TODO: uninit not implemented")
                        };

                        let expr = intermediate.clone().coerce(&ty).ok_or_else(|| {
                            cx.type_mismatch(
                                &ty.name(),
                                &intermediate
                                    .default_type()
                                    .expect("must have default type")
                                    .name(),
                                decl.value.span(),
                            )
                        })?;

                        unit.consts.get_mut(const_id).value =
                            Some((ty, Value::eval(expr).expect("TODO: report this")));
                    }
                }
                _ => todo!("throw error when invalid items are found"),
            }
        }

        Ok(())
    }
}

/// An intermediate `const` binding before it's translated to AIR.
#[derive(Debug)]
pub struct Const {
    /// The span of the entire `const` declaration, if available.  Made optional because intrinsic
    /// functions may not have span information.
    pub span: Option<Span>,

    /// The name of the declaration.
    pub name: String,

    /// The value of the constant.  [`None`] if it has not been initialized yet.  All constants
    /// should be initialized past the `define_root_const_values` pass, and this can be `unwrap`ed.
    pub value: Option<(Type, Value)>,
}

/// An AIR expression which has not yet been assigned a specific type.
///
/// Used to infer the type of a value for a variable, especially for literals such as integers and
/// floats which have multiple possible types.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntermediateExpr {
    /// An immediate integer value which has not yet been assigned a specific type.
    ///
    /// When converted into an AIR expression, it uses [`air::Expr::Const`] as both the type and
    /// value have been determined.
    ImmInt(u64),

    /// Any compile time known value with a known type.
    Const(Type, Value),

    /// A reference value, such as a type or a reference.
    Ref(Mutable, Box<IntermediateExpr>),

    /// A function call expression.  The provided type is the type that the function outputs.
    Call(Type, air::Call),
}

impl IntermediateExpr {
    /// Attempts to verify that the provided AST expression is a valid AIR expression.
    pub fn verify(
        cx: &mut Context,
        unit: &mut Unit,
        scope: &Scope,
        expr: &ast::Expr,
    ) -> Result<Self, ()> {
        match expr {
            ast::Expr::Id(id) => {
                // TODO: move to separate method
                if let Some(value) = scope.find(&id.value) {
                    match value {
                        ScopedRef::Const(const_id) => {
                            let const_ = unit.consts.get(const_id);
                            let value = const_
                                .value
                                .clone()
                                .expect("TODO: evaluate value if not defined (values shouldn't need to be used in order)");
                            Ok(Self::Const(value.0, value.1))
                        }
                    }
                } else {
                    cx.could_not_resolve_name(&id.value, id.span());
                    return Err(());
                }
            }
            ast::Expr::Int(int) => Ok(Self::ImmInt(int.value)),
            ast::Expr::Str(str) => Ok(Self::Const(
                Type::thin_ptr(Mutable::Yes, Type::U8),
                Value::Nullterm(str.value.clone()),
            )),
            ast::Expr::Func(func) => {
                let sig = FuncSig::from_ast(cx, unit, scope, &func)?;

                if func.name.is_none() && func.block.is_none() {
                    // it's a function type, rather than a function value.
                    Ok(Self::Const(
                        Type::Type,
                        Value::Type(Type::Func(Box::new(sig))),
                    ))
                } else {
                    let extern_name = func.name.as_ref().map(|name| name.value.clone());

                    // TODO: check if there is a function with the same external name, verify that
                    // the function was not already defined and that the two signatures are
                    // equivalent to eachother.

                    let func_value = air::Func {
                        signature: sig.clone(),
                        signature_span: Some(Span::new(
                            func.span().file_id(),
                            func.span().start(),
                            func.returns.span().end(),
                        )),
                        extern_name,
                        def: None,
                    };
                    let func_ref = unit.funcs.insert(func_value);
                    unit.current_func = func_ref;

                    if let Some(block) = &func.block {
                        let insts = unit.analyze_block(cx, scope, block)?;
                        unit.funcs.get_mut(func_ref).def = Some(air::FuncDef { insts });
                    }

                    Ok(Self::Const(
                        Type::Func(Box::new(sig)),
                        Value::Func(FuncId(func_ref.to_raw())),
                    ))
                }
            }
            ast::Expr::Unary(box ast::Unary { op, operand, .. })
                if *op == ast::UnaryOp::Ref || *op == ast::UnaryOp::RefMut =>
            {
                let mutable = if *op == ast::UnaryOp::Ref {
                    Mutable::No
                } else {
                    Mutable::Yes
                };

                Ok(Self::Ref(
                    mutable,
                    Box::new(IntermediateExpr::verify(cx, unit, scope, operand)?),
                ))
            }
            ast::Expr::Call(call) => {
                let res = unit.analyze_call(cx, scope, call)?;
                Ok(Self::Call(res.0, res.1))
            }
            _ => todo!("implement other expressions"),
        }
    }

    /// Attempts to coerce the [IntermediateExpr] into the provided type.
    pub fn coerce(self, expected_type: &Type) -> Option<air::Expr> {
        match (self, expected_type) {
            (Self::ImmInt(value), expected_type) if expected_type.is_int() => {
                // TODO: possibly move to separate method
                Some(air::Expr::Const(
                    expected_type.clone(),
                    match expected_type {
                        // TODO: check if value fits
                        Type::U8 => Value::U8(value as u8),
                        Type::I32 => Value::I32(value as i32),
                        _ => unreachable!("should be an integer type"),
                    },
                ))
            }
            (Self::Const(ty, value), expected_type) if ty.is_equivalent(&expected_type) => {
                Some(air::Expr::Const(ty, value))
            }
            (Self::Ref(mutable, operand), expected_type)
                if expected_type.is_equivalent(&Type::Type) =>
            {
                let Value::Type(operand_type) = Value::eval(operand.coerce(&Type::Type)?)
                .expect("TODO: throw diagnostic that type is not constant")
                else { unreachable!("should be a type") };

                Some(air::Expr::Const(
                    Type::Type,
                    Value::Type(Type::thin_ptr(mutable, operand_type)),
                ))
            }
            (Self::Call(ty, call), expected_type) if ty.is_equivalent(expected_type) => {
                Some(air::Expr::Call(ty, Box::new(call)))
            }
            _ => None,
        }
    }

    /// Attempts to provide a default type for this [IntermediateExpr].
    ///
    /// Values such as `uninit` which cannot have a known type cannot be inferred.
    ///
    /// Essentially a shorthand for:
    ///
    /// ```no_run
    /// # use ampc::sema::air;
    /// # use ampc::sema::IntermediateExpr;
    /// # fn _test(intermediate_expr: IntermediateExpr) -> Option<air::Expr> {
    /// let ty = intermediate_expr.default_type()?;
    /// intermediate_expr.coerce(&ty)
    /// # }
    /// ```
    pub fn infer(self) -> Option<air::Expr> {
        let ty = self.default_type()?;
        self.coerce(&ty)
    }

    /// Returns the default [Type] for an intermediate expression.
    pub fn default_type(&self) -> Option<Type> {
        match self {
            // TODO: pick a type other than i32 if the literal type is too large.
            Self::ImmInt(_) => Some(Type::I32), // use `i32` as default type for integers
            Self::Const(ty, _) => Some(ty.clone()),
            Self::Ref(mutable, operand) => {
                // TODO: do we need to confirm that there is a default type?
                if operand.default_type()? == Type::Type {
                    Some(Type::Type)
                } else {
                    Some(Type::thin_ptr(*mutable, operand.default_type()?))
                }
            }
            Self::Call(ty, _) => Some(ty.clone()),
        }
    }
}
