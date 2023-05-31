//! Constant values in Amp.

use crate::{sema::air, types::Type};

/// A value known at compile-time.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    /// A constant [u8] value, whose type is always [`Type::U8`].
    U8(u8),

    /// A constant [i32] value, whose type is always [`Type::I32`].
    I32(i32),

    /// A `type` value, whose type is always [`Type::Type`].
    Type(Type),
}

impl Value {
    /// Attempts to evaluate a constant value from the provided expression
    pub fn eval(expr: air::Expr) -> Option<Self> {
        match expr {
            air::Expr::Const(_, value) => Some(value),
        }
    }
}
