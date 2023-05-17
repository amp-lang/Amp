//! The Amp compiler interface for Rust.
//!
//! Provides the capabilities to compile Amp programs without accessing the command line.

pub mod codemap;
mod context;
pub mod diag;
pub mod sema;
pub mod syntax;

pub use context::*;
