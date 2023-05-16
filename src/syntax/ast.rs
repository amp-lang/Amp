//! The abstract syntax trees for Amp.

use crate::{
    codemap::{Span, Spanned},
    diag::SyntaxDiagnostics,
    syntax::token::TokenTree,
    Context,
};

use super::{
    parser::{Parse, Recoverable},
    token::{Delimiter, LiteralKind, PunctKind, TokenIter},
};

// Macros

/// An argument list.
///
/// ```amp
/// (42, "Hello, world!")
/// ```
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Arglist<T> {
    pub span: Span,
    pub items: Vec<T>,
}

impl<T> Arglist<T> {
    pub fn parse(
        cx: &mut Context,
        tokens: &mut TokenIter,
        mut parser: impl Parse<T>,
    ) -> Result<Self, Recoverable> {
        let group = tokens.expect_group(Delimiter::Paren)?;
        let mut items = Vec::new();

        let mut ok = true;
        let mut tokens = group.tokens().iter();

        while let Some(token) = tokens.peek().cloned() {
            let item = match parser.parse(cx, &mut tokens) {
                Ok(item) => item,
                Err(recoverable) => {
                    ok = false;

                    if recoverable == Recoverable::Yes {
                        tokens.next();
                        cx.invalid_arglist_param(token.span(), group.end_span());
                    }

                    continue;
                }
            };

            dbg!(tokens.peek());
            items.push(item);
            if let Some(token) = tokens.next() {
                match token {
                    TokenTree::Punct(punct) if punct.kind() == PunctKind::Comma => {}
                    _ => cx.arglist_expected_comma_or_close(token.span(), group.end_span()),
                }
            } else {
                break;
            }
        }

        if !ok {
            // Diagnostics must already have been thrown.
            return Err(Recoverable::No);
        }

        Ok(Self {
            span: group.span(),
            items,
        })
    }
}

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

        Err(Recoverable::Yes)
    }
}
