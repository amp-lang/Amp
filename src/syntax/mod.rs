//! The scanner, parser and abstract syntax trees for Amp.

pub mod ast;
pub mod parser;
pub mod scanner;
pub mod token;

pub use scanner::scan;
