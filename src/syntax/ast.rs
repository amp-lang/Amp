//! The abstract syntax trees for Amp.

use crate::{
    codemap::{Span, Spanned},
    diag::SyntaxDiagnostics,
    syntax::token::TokenTree,
    Context,
};

use super::{
    parser::{IfRecoverable, Parse, Recoverable},
    token::{Delimiter, LiteralKind, PunctKind, ReservedWord, TokenIter},
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

/// A type annotation.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeAnnotation {
    pub span: Span,
    pub ty: Expr,
}

impl TypeAnnotation {
    /// Parses a [TypeAnnotation] from the provided tokens.
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        let start_span = tokens.expect_punct(PunctKind::Colon)?.span();

        let ty = Expr::parse(cx, tokens).if_recoverable(|| {
            cx.expected_type_annotation_type(start_span);
            Recoverable::No
        })?;

        Ok(Self {
            span: Span::new(start_span.file_id(), start_span.start(), ty.span().end()),
            ty,
        })
    }
}

impl Spanned for TypeAnnotation {
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
    Const(Box<Const>),
    Return(Box<Return>),
}

impl Expr {
    /// Returns `true` if this [Expr] requires a `;` terminator.
    pub fn requires_terminator(&self) -> bool {
        match self {
            _ => true,
        }
    }

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
        match Const::parse(cx, tokens) {
            Ok(value) => return Ok(Self::Const(Box::new(value))),
            Err(Recoverable::No) => return Err(Recoverable::No),
            Err(Recoverable::Yes) => {}
        }

        match Return::parse(cx, tokens) {
            Ok(value) => return Ok(Self::Return(Box::new(value))),
            Err(Recoverable::No) => return Err(Recoverable::No),
            Err(Recoverable::Yes) => {}
        }

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
            Self::Const(expr) => expr.span(),
            Self::Return(expr) => expr.span(),
        }
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

// Expressions

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

// Statements

/// A list of semicolon-terminated statements.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Stmnts {
    pub span: Span,
    pub stmnts: Vec<Expr>,
}

impl Stmnts {
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        let mut ok = true;
        let mut stmnts = Vec::new();
        let mut span = Span::new(0, 0, 0);

        while let Some(next) = tokens.clone().peek() {
            let expr = match Expr::parse(cx, tokens) {
                Ok(expr) => {
                    let expr_span = expr.span();
                    span = Span::new(expr_span.file_id(), span.start(), expr_span.end());
                    expr
                }
                Err(recoverable) => {
                    if recoverable == Recoverable::Yes {
                        cx.invalid_stmnt(next.span());
                        tokens.next();
                    }
                    ok = false;
                    continue;
                }
            };

            if expr.requires_terminator() {
                match tokens.expect_punct(PunctKind::Semi) {
                    Ok(_) => {}
                    Err(_) => {
                        ok = false;
                        cx.expected_semicolon(expr.span());
                        continue;
                    }
                }
            }

            stmnts.push(expr);
        }

        if !ok {
            return Err(Recoverable::No);
        }

        Ok(Self { span, stmnts })
    }
}

/// A code block.
///
/// ```amp
/// {
///     const my_var = 0;
/// }
/// ```
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Block {
    pub span: Span,
    pub stmnts: Stmnts,
}

impl Block {
    /// Parses a [Block] from the provided tokens.
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        let group = tokens.expect_group(Delimiter::Brace)?;
        let stmnts = Stmnts::parse(cx, &mut group.tokens().iter())?;

        Ok(Self {
            span: group.span(),
            stmnts,
        })
    }
}

/// A constant declaration.
///
/// ```amp
/// const MyValue = 42;
/// ```
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Const {
    pub span: Span,
    pub name: Id,
    pub ty: Option<TypeAnnotation>,
}

impl Const {
    /// [Parse]s a [Const] declaration.
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        let start_span = tokens.expect_reserved(ReservedWord::Const)?.span();

        let name = Id::parse(cx, tokens).if_recoverable(|| {
            cx.expected_binding_decl_name(start_span);
            Recoverable::No
        })?;

        let ty = match TypeAnnotation::parse(cx, tokens) {
            Ok(ty) => Some(ty),
            Err(Recoverable::Yes) => None,
            Err(Recoverable::No) => return Err(Recoverable::No),
        };

        tokens.expect_punct(PunctKind::Eq).if_recoverable(|| {
            cx.expected_const_binding_value(Span::new(
                start_span.file_id(),
                start_span.start(),
                match &ty {
                    Some(end) => end.span().end(),
                    None => name.span().end(),
                },
            ));
            Recoverable::No
        })?;

        Ok(Self {
            span: start_span,
            name,
            ty,
        })
    }
}

impl Spanned for Const {
    fn span(&self) -> Span {
        self.span
    }
}

/// A `return` expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Return {
    pub span: Span,
    pub value: Option<Expr>,
}

impl Return {
    /// Parses a [Return] statement from the provided stream.
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        let start_span = tokens.expect_reserved(ReservedWord::Return)?.span();

        let value = match Expr::parse(cx, tokens) {
            Ok(value) => Some(value),
            Err(Recoverable::Yes) => None,
            Err(Recoverable::No) => return Err(Recoverable::No),
        };

        Ok(Self {
            span: Span::new(
                start_span.file_id(),
                start_span.start(),
                match &value {
                    Some(value) => value.span().end(),
                    None => start_span.end(),
                },
            ),
            value,
        })
    }
}

impl Spanned for Return {
    fn span(&self) -> Span {
        self.span
    }
}
