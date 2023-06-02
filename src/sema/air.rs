use crate::{types::Type, value::Value};

/// A typed AIR expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    /// Any value known at compile time.
    Const(Type, Value),
}

/// A function value.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Func {
    /// The name that will be exposed to the linkage unit when the modules are being linked.  If
    /// no name is provided, the function will be anonymous and only accessible in the object file
    /// that it is declared in.
    pub extern_name: Option<String>,
}
