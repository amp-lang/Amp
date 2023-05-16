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
    pub fn parse(_cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        let token = tokens.expect_literal(LiteralKind::Id)?;

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
    /// [Parse]s a [Str] from the provided tokens.
    ///
    /// TODO: implement parsing string escapes
    pub fn parse(_cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        let token = tokens.expect_literal(LiteralKind::Str)?;
        let value = token.as_str();

        Ok(Self {
            span: token.span(),
            value: value[1..value.len() - 1].to_string(),
        })
    }
}

impl Spanned for Str {
    fn span(&self) -> Span {
        self.span
    }
}

/// An integer literal.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Int {
    pub span: Span,
    pub value: u64,
}

impl Int {
    /// [Parse]s an [Int] from the provided tokens.
    ///
    /// TODO: implement parsing non-decimal integers
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        let token = tokens.expect_literal(LiteralKind::DecInt)?;

        Ok(Self {
            span: token.span(),
            value: token.as_str().parse::<u64>().or_else(|_| {
                cx.integer_too_large(token.span());
                Err(Recoverable::No)
            })?,
        })
    }
}

impl Spanned for Int {
    fn span(&self) -> Span {
        self.span
    }
}

/// An Amp expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    Id(Id),
    Str(Str),
    Int(Int),
}

impl Expr {
    /// [Parse]s an [Expr] from the provided tokens.
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        match Id::parse(cx, tokens) {
            Ok(value) => return Ok(Self::Id(value)),
            Err(Recoverable::No) => return Err(Recoverable::No),
            Err(Recoverable::Yes) => {}
        }

        match Str::parse(cx, tokens) {
            Ok(value) => return Ok(Self::Str(value)),
            Err(Recoverable::No) => return Err(Recoverable::No),
            Err(Recoverable::Yes) => {}
        }

        match Int::parse(cx, tokens) {
            Ok(value) => return Ok(Self::Int(value)),
            Err(Recoverable::No) => return Err(Recoverable::No),
            Err(Recoverable::Yes) => {}
        }
        dbg!("TEST");
        Err(Recoverable::Yes)
    }
}
