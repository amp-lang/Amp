//! The Amp compiler interface for Rust.
//!
//! Provides the capabilities to compile Amp programs without accessing the command line.

#![feature(box_patterns)]

pub mod clif;
pub mod codemap;
pub(crate) mod codespan_reporting;
mod context;
pub mod diag;
pub mod sema;
pub mod syntax;
pub mod types;
pub mod value;

pub use context::*;
