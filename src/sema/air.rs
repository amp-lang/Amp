use crate::{types::Type, value::Value};

/// A typed AIR expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    /// Any value known at compile time.
    Const(Type, Value),
}
