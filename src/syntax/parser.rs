//! The parser for the Amp compiler.

use crate::Context;

use super::token::TokenIter;

/// An error that occurred during a [Parse] run.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Recoverable {
    /// The parser was not able to find a match in the token list.  The token iterator has not been
    /// advanced and no diagnostics have been reported.
    Yes,

    /// The token iterator has been advanced and a diagnostic has been reported.
    No,
}

/// [Result<T, Recoverable>] methods.
pub trait IfRecoverable<T> {
    /// Calls the provided callback if this value is a [`Recoverable::Yes`] error type.  If this
    /// value is [Recoverable], returns the result of the provided callback, wrapped in [`Err`].
    fn if_recoverable(self, callback: impl FnMut() -> Recoverable) -> Result<T, Recoverable>;
}

impl<T> IfRecoverable<T> for Result<T, Recoverable> {
    fn if_recoverable(self, mut callback: impl FnMut() -> Recoverable) -> Result<T, Recoverable> {
        match self {
            Self::Err(Recoverable::Yes) => Err(callback()),
            res => res,
        }
    }
}

/// A trait for parser segments.
pub trait Parse<T> {
    /// Parses a value from the provided tokens.
    fn parse(&mut self, cx: &mut Context, tokens: &mut TokenIter) -> Result<T, Recoverable>;
}

impl<T, F: FnMut(&mut Context, &mut TokenIter) -> Result<T, Recoverable>> Parse<T> for F {
    fn parse(&mut self, cx: &mut Context, tokens: &mut TokenIter) -> Result<T, Recoverable> {
        self(cx, tokens)
    }
}
