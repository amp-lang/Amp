//! The abstract syntax trees for Amp.

use crate::{
    codemap::{Span, Spanned},
    diag::SyntaxDiagnostics,
    syntax::token::TokenTree,
    Context,
};

use super::{
    parser::{IfRecoverable, Parse, Recoverable},
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

                    if let Some(token) = tokens.next() {
                        match token {
                            TokenTree::Punct(punct) if punct.kind() == PunctKind::Comma => {}
                            _ => cx.arglist_expected_comma_or_close(token.span(), group.end_span()),
                        }
                    } else {
                        break;
                    }

                    continue;
                }
            };

            items.push(item);
            if let Some(token) = tokens.next() {
                match token {
                    TokenTree::Punct(punct) if punct.kind() == PunctKind::Comma => {}
                    _ => {
                        ok = false;
                        cx.arglist_expected_comma_or_close(token.span(), group.end_span())
                    }
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

/// A function call expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Call {
    pub span: Span,
    pub callee: Expr,
    pub args: Arglist<Expr>,
}

impl Spanned for Call {
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
    Call(Box<Call>),
}

impl Expr {
    /// Parses a single "atom" of an expression, such as a literal or a sub-expression wrapped in
    /// parentheses.
    fn parse_atom(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
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

    /// Parses a basic expression with a Pratt parser-style implementation.
    fn pratt_parse(
        cx: &mut Context,
        tokens: &mut TokenIter,
        min_bp: u8,
    ) -> Result<Self, Recoverable> {
        let mut lhs = Self::parse_atom(cx, tokens)?;

        loop {
            let op = match tokens.clone().next() {
                Some(token) => token,
                None => break,
            };

            // Check if the token is a valid operator.
            if let Some((l_bp, ())) = op.postfix_binding_power() {
                if l_bp < min_bp {
                    break;
                }
            } else {
                break;
            }

            match op {
                TokenTree::Group(group) if group.delim() == Delimiter::Paren => {
                    lhs = Self::Call(Box::new(Call {
                        span: lhs.span(),
                        callee: lhs,
                        args: Arglist::parse(cx, tokens, Expr::parse)
                            .if_recoverable(|| unreachable!("should not be reachable"))?,
                    }));
                }
                _ => unreachable!(
                    "no other operators are implemented yet. replace this with a catchall."
                ),
            }
        }

        Ok(lhs)
    }

    /// [Parse]s an [Expr] from the provided tokens.
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        Self::pratt_parse(cx, tokens, 0)
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Id(expr) => expr.span(),
            Self::Str(expr) => expr.span(),
            Self::Int(expr) => expr.span(),
            Self::Call(expr) => expr.span(),
        }
    }
}
