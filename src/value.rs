//! Constant values in Amp.

use slot_arena::Ref;

use crate::{sema::air, types::Type};

/// A generic identifier for a function.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncId(pub u32);

impl FuncId {
    /// Converts this [FuncId] into an AIR function reference.
    #[inline]
    pub fn to_air_func(&self) -> Ref<air::Func> {
        Ref::from_raw(self.0)
    }
}

/// A value known at compile-time.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    /// A `type` value, whose type is always [`Type::Type`].
    Type(Type),

    /// A constant [u8] value, whose type is always [`Type::U8`].
    U8(u8),

    /// A constant [i32] value, whose type is always [`Type::I32`].
    I32(i32),

    /// A null-terminated string value.  Note that the compiler representation of the string
    /// (before it is outputted to an object file) is not null-terminated.
    Nullterm(String),

    /// A function value.
    ///
    /// The type of this [Value] matches the signature of the function's declaration.
    Func(FuncId),
}

impl Value {
    /// Attempts to evaluate a constant value from the provided expression
    pub fn eval(expr: air::Expr) -> Option<Self> {
        match expr {
            air::Expr::Const(_, value) => Some(value),
            // Not supported as const expression
            _ => None,
        }
    }
}
