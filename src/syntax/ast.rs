//! The abstract syntax trees for Amp.

use crate::{
    codemap::{Span, Spanned},
    diag::SyntaxDiagnostics,
    Context,
};

use super::{
    parser::Recoverable,
    token::{LiteralKind, TokenIter},
};

// Literals

/// An identifier literal.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Id {
    pub span: Span,
    pub value: String,
}

impl Id {
    /// [Parse]s an [Id] from the provided tokens.
    pub fn parse(_cx: &mut Context, token: &mut TokenIter) -> Result<Self, Recoverable> {
        let token = token.expect_literal(LiteralKind::Id)?;

        Ok(Self {
            span: token.span(),
            value: token.as_str().to_string(),
        })
    }
}

impl Spanned for Id {
    fn span(&self) -> Span {
        self.span
    }
}

/// A string literal.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Str {
    pub span: Span,
    pub value: String,
}

impl Str {
    /// [Parse]s an [Str] from the provided tokens.
    ///
    /// TODO: implement parsing string escapes
    pub fn parse(_cx: &mut Context, token: &mut TokenIter) -> Result<Self, Recoverable> {
        let token = token.expect_literal(LiteralKind::Str)?;
        let value = token.as_str();

        Ok(Self {
            span: token.span(),
            value: value[1..value.len() - 1].to_string(),
        })
    }
}

/// An integer literal.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Int {
    pub span: Span,
    pub value: u64,
}

impl Int {
    /// [Parse]s an [Str] from the provided tokens.
    ///
    /// TODO: implement parsing non-decimal integers
    pub fn parse(cx: &mut Context, token: &mut TokenIter) -> Result<Self, Recoverable> {
        let token = token.expect_literal(LiteralKind::DecInt)?;

        Ok(Self {
            span: token.span(),
            value: token.as_str().parse::<u64>().or_else(|_| {
                cx.integer_too_large(token.span());
                Err(Recoverable::No)
            })?,
        })
    }
}
