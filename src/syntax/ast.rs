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

impl<T> Spanned for Arglist<T> {
    fn span(&self) -> Span {
        self.span
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
    Func(Box<Func>),
    Unary(Box<Unary>),
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

        match Func::parse(cx, tokens) {
            Ok(value) => return Ok(Self::Func(Box::new(value))),
            Err(Recoverable::No) => return Err(Recoverable::No),
            Err(Recoverable::Yes) => {}
        }

        Err(Recoverable::Yes)
    }

    /// Parse prefix expressions (such as `&` or `-`) or an atom.
    fn pratt_parse_prefix(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        let first = tokens.clone().next().ok_or(Recoverable::Yes)?;
        let start_span = first.span();

        let ((), r_bp) = match first.prefix_binding_power() {
            Some(bp) => bp,
            None => return Self::parse_atom(cx, tokens),
        };

        // iterate past operator
        tokens.next();

        let mut op = UnaryOp::from_token(first).unwrap();
        if op == UnaryOp::Ref {
            match tokens.peek() {
                Some(TokenTree::Reserved(reserved)) if reserved.kind() == ReservedWord::Mut => {
                    tokens.next();
                    op = UnaryOp::RefMut
                }
                _ => {}
            }
        }

        let operand = Self::pratt_parse(cx, tokens, r_bp).if_recoverable(|| {
            cx.expected_operand_expression(start_span);
            Recoverable::No
        })?;

        Ok(Self::Unary(Box::new(Unary {
            span: Span::new(
                start_span.file_id(),
                start_span.start(),
                operand.span().end(),
            ),
            op,
            operand,
        })))
    }

    /// Parses a basic expression with a Pratt parser-style implementation.
    fn pratt_parse(
        cx: &mut Context,
        tokens: &mut TokenIter,
        min_bp: u8,
    ) -> Result<Self, Recoverable> {
        let mut lhs = Self::pratt_parse_prefix(cx, tokens)?;

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
                    let lhs_span = lhs.span();
                    let args = Arglist::parse(cx, tokens, Expr::parse)
                        .if_recoverable(|| unreachable!("should not be reachable"))?;
                    lhs = Self::Call(Box::new(Call {
                        span: Span::new(lhs_span.file_id(), lhs_span.start(), args.span().end()),
                        callee: lhs,
                        args,
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
            Self::Func(expr) => expr.span(),
            Self::Unary(expr) => expr.span(),
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

/// A named [FuncParam].
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NamedParam {
    pub span: Span,
    pub name: Id,
    pub ty: TypeAnnotation,
}

impl NamedParam {
    /// Parses a [NamedParam].
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        // NOTE: named parameters must check for an identifier AND a colon to differentiate
        // [NamedParam] from [Expr]s.
        match tokens.peek_nth(0) {
            Some(TokenTree::Literal(literal)) if literal.kind() == LiteralKind::Id => {}
            _ => return Err(Recoverable::Yes),
        }

        match tokens.peek_nth(1) {
            Some(TokenTree::Punct(punct)) if punct.kind() == PunctKind::Colon => {}
            _ => return Err(Recoverable::Yes),
        }

        let name = Id::parse(cx, tokens).expect("should have been an identifier");

        // note that any errors are unrecoverable, as we've confirmed that there is a type
        // annotation previously.
        let ty = TypeAnnotation::parse(cx, tokens)?;

        Ok(Self {
            span: Span::new(name.span().file_id(), name.span().start(), ty.span().end()),
            name,
            ty,
        })
    }
}

impl Spanned for NamedParam {
    fn span(&self) -> Span {
        self.span
    }
}

/// A parameter of a [Func] expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FuncParam {
    /// A function parameter declared with a name.
    Named(NamedParam),

    /// An anonymous function parameter.
    Anon(Expr),
}

impl FuncParam {
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        match NamedParam::parse(cx, tokens) {
            Ok(value) => return Ok(Self::Named(value)),
            Err(Recoverable::No) => return Err(Recoverable::No),
            Err(Recoverable::Yes) => {}
        }

        Ok(Self::Anon(Expr::parse(cx, tokens)?))
    }
}

impl Spanned for FuncParam {
    fn span(&self) -> Span {
        match self {
            Self::Named(param) => param.span(),
            Self::Anon(param) => param.span(),
        }
    }
}

/// A `func` value.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Func {
    pub span: Span,
    pub name: Option<Id>,
    pub args: Arglist<FuncParam>,
    pub returns: TypeAnnotation,
    pub block: Option<Block>,
}

impl Func {
    /// Parses a [Func] value.
    pub fn parse(cx: &mut Context, tokens: &mut TokenIter) -> Result<Self, Recoverable> {
        let start_span = tokens.expect_reserved(ReservedWord::Func)?.span();

        let name = match Id::parse(cx, tokens) {
            Ok(name) => Some(name),
            Err(Recoverable::Yes) => None,
            Err(Recoverable::No) => return Err(Recoverable::No),
        };

        let args = Arglist::parse(cx, tokens, FuncParam::parse).if_recoverable(|| {
            cx.expected_function_args(Span::new(
                start_span.file_id(),
                start_span.start(),
                match &name {
                    Some(name) => name.span().end(),
                    None => start_span.end(),
                },
            ));
            Recoverable::No
        })?;

        let returns = TypeAnnotation::parse(cx, tokens).if_recoverable(|| {
            cx.expected_function_return_type(Span::new(
                start_span.file_id(),
                start_span.start(),
                args.span().end(),
            ));
            Recoverable::No
        })?;

        let block = match Block::parse(cx, tokens) {
            Ok(block) => Some(block),
            Err(Recoverable::Yes) => None,
            Err(Recoverable::No) => return Err(Recoverable::No),
        };

        Ok(Self {
            span: start_span,
            name,
            args,
            returns,
            block,
        })
    }
}

impl Spanned for Func {
    fn span(&self) -> Span {
        self.span
    }
}

/// A unary operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnaryOp {
    Ref,
    RefMut,
}

impl UnaryOp {
    pub fn from_token(token: &TokenTree) -> Option<Self> {
        match token {
            TokenTree::Punct(punct) => match punct.kind() {
                PunctKind::And => Some(UnaryOp::Ref),
                _ => None,
            },
            _ => None,
        }
    }
}

/// A unary expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Unary {
    pub span: Span,
    pub op: UnaryOp,
    pub operand: Expr,
}

impl Spanned for Unary {
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
    pub value: Expr,
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

        let eq_span = tokens
            .expect_punct(PunctKind::Eq)
            .if_recoverable(|| {
                cx.expected_const_binding_value(Span::new(
                    start_span.file_id(),
                    start_span.start(),
                    match &ty {
                        Some(end) => end.span().end(),
                        None => name.span().end(),
                    },
                ));
                Recoverable::No
            })?
            .span();

        let value = Expr::parse(cx, tokens).if_recoverable(|| {
            cx.expected_const_binding_value(Span::new(
                start_span.file_id(),
                start_span.start(),
                eq_span.end(),
            ));
            Recoverable::No
        })?;

        Ok(Self {
            span: Span::new(start_span.file_id(), start_span.start(), value.span().end()),
            name,
            ty,
            value,
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
