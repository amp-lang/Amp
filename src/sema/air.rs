use crate::{
    syntax::ast,
    types::{FuncSig, Type},
    value::Value,
    Context,
};

use super::{scope::Scope, IntermediateExpr, Unit};

/// A typed AIR expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    /// Any value known at compile time.
    Const(Type, Value),
}

/// The implementation of a [Func], if it is defined.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncDef {
    /// The instructions in the function implementation.
    pub insts: Vec<Stmnt>,
}

/// A function value.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Func {
    /// The type signature of the function.
    pub signature: FuncSig,

    /// The name that will be exposed to the linkage unit when the modules are being linked.  If
    /// no name is provided, the function will be anonymous and only accessible in the object file
    /// that it is declared in.
    pub extern_name: Option<String>,

    /// The definition for a function, if it exists.
    pub def: Option<FuncDef>,
}

/// A `return` statement.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Return {
    /// The value being returned.
    ///
    /// Must match the signature of the function that this `return` statement was found in.
    ///
    /// Currently, if the function expects a value to be returned and the value field is empty,
    /// uninitialized memory will be returned.  This will not be supported in the future and
    /// the `uninit` keyword should be used instead.
    ///
    /// ```amp
    /// const MyFunc = func(): i32 {
    ///     return; // currently allowed, won't be in the future
    /// }
    /// ```
    ///
    /// ```amp
    /// const MyFunc = func(): i32 {
    ///     return uninit; // not yet supported, replacement for above example.
    /// }
    /// ```
    pub value: Option<Expr>,
}

impl Return {
    /// Creates a [Return] statement from the provided AST statement.
    pub fn from_ast(
        cx: &mut Context,
        unit: &mut Unit,
        scope: &mut Scope,
        stmnt: &ast::Return,
    ) -> Result<Self, ()> {
        let expected_return_type = unit.funcs.get(unit.current_func).signature.returns.clone();

        // TODO: check if void
        let value = match &stmnt.value {
            Some(value) => Some(
                IntermediateExpr::verify(cx, unit, scope, value)?
                    .coerce(&expected_return_type)
                    .expect("TODO: report error here"),
            ),
            None => None,
        };

        Ok(Self { value })
    }
}

/// A statement in Amp AIR.
///
/// Statements are instructions of code that are used as a base for an operation, such as
/// assignments or function calls.  Expressions such as `1 * 2` cannot be used as a statement as
/// they only produce a value.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stmnt {
    Return(Return),
}
