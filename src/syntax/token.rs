use crate::codemap::{Span, Spanned};

use super::parser::Recoverable;

/// The identifying kind of a [Literal] token.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    /// An identifier token.
    Id,

    /// A decimal integer token.
    DecInt,

    /// A string token.
    Str,
}

/// Any literal token which has a variable value (for example, strings, identifiers, numbers).
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Literal<'src> {
    span: Span,
    kind: LiteralKind,
    src: &'src str,
}

impl<'src> Literal<'src> {
    /// Creates a new [Literal] token.
    #[inline]
    pub fn new(span: Span, kind: LiteralKind, src: &'src str) -> Self {
        Self { span, kind, src }
    }

    /// Returns the identifying kind of this [Literal] token.
    #[inline]
    pub fn kind(&self) -> LiteralKind {
        self.kind
    }

    /// Returns the string value of the [Literal] token.
    #[inline]
    pub fn as_str(&self) -> &str {
        self.src
    }
}

impl<'src> Spanned for Literal<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> Token<'src> for Literal<'src> {
    fn from_token_tree<'a>(token_tree: &'a TokenTree<'src>) -> Option<&'a Self> {
        match token_tree {
            TokenTree::Literal(literal) => Some(literal),
            _ => None,
        }
    }

    fn matches(token_tree: &TokenTree) -> bool {
        matches!(token_tree, TokenTree::Literal(_))
    }
}

/// The identifying kind of a [Reserved] token.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReservedWord {
    Const,
    Func,
    Return,
}

/// A reserved word token.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Reserved {
    span: Span,
    kind: ReservedWord,
}

impl Reserved {
    /// Creates a new [Reserved] token.
    #[inline]
    pub fn new(span: Span, kind: ReservedWord) -> Self {
        Self { span, kind }
    }

    /// Returns the kind of this [Reserved] token.
    #[inline]
    pub fn kind(&self) -> ReservedWord {
        self.kind
    }

    /// Returns the raw string value of the [Reserved] token.
    pub fn as_str(&self) -> &str {
        match self.kind {
            ReservedWord::Const => "const",
            ReservedWord::Func => "func",
            ReservedWord::Return => "return",
        }
    }
}

impl Spanned for Reserved {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> Token<'src> for Reserved {
    fn from_token_tree<'a>(token_tree: &'a TokenTree<'src>) -> Option<&'a Self> {
        match token_tree {
            TokenTree::Reserved(reserved) => Some(reserved),
            _ => None,
        }
    }

    fn matches(token_tree: &TokenTree) -> bool {
        matches!(token_tree, TokenTree::Reserved(_))
    }
}

/// The identifying kind of a [Punct] token.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum PunctKind {
    And,
    Colon,
    Comma,
    Eq,
    Semi,
}

/// A punctuator token.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Punct {
    span: Span,
    kind: PunctKind,
}

impl Punct {
    /// Creates a new [Punct] token.
    #[inline]
    pub fn new(span: Span, kind: PunctKind) -> Self {
        Self { span, kind }
    }

    /// Returns the kind of this token.
    #[inline]
    pub fn kind(&self) -> PunctKind {
        self.kind
    }

    /// Returns the raw string value of this [Punct] token.
    pub fn as_str(&self) -> &str {
        match self.kind {
            PunctKind::And => "&",
            PunctKind::Colon => ":",
            PunctKind::Comma => ",",
            PunctKind::Eq => "=",
            PunctKind::Semi => ";",
        }
    }
}

impl Spanned for Punct {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> Token<'src> for Punct {
    fn from_token_tree<'a>(token_tree: &'a TokenTree<'src>) -> Option<&'a Self> {
        match token_tree {
            TokenTree::Punct(punct) => Some(punct),
            _ => None,
        }
    }

    fn matches(token_tree: &TokenTree) -> bool {
        matches!(token_tree, TokenTree::Punct(_))
    }
}

/// The delimiter of a [Group] token.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Delimiter {
    /// `{}`
    Brace,

    /// `()`
    Paren,
}

impl Delimiter {
    /// Returns the postfix binding power of the [Delimiter], if any.
    pub fn postfix_binding_power(&self) -> Option<(u8, ())> {
        match self {
            Self::Paren => Some((1, ())),
            _ => None,
        }
    }
}

/// A group token, such as `{}` or `()`.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Group<'src> {
    span: Span,
    delim: Delimiter,
    stream: TokenStream<'src>,
}

impl<'src> Group<'src> {
    /// Creates a new [Group] token.
    ///
    /// NOTE: the span of the [Group] should span atleast two characters, to make sure the opening
    /// and closing delimiters are included.
    #[inline]
    pub fn new(span: Span, delim: Delimiter, stream: TokenStream<'src>) -> Self {
        Self {
            span,
            delim,
            stream,
        }
    }

    /// Returns the delimiter of the [Group].
    #[inline]
    pub fn delim(&self) -> Delimiter {
        self.delim
    }

    /// Returns the tokens in this [Group], excluding the delimiters.
    #[inline]
    pub fn tokens(&self) -> &TokenStream {
        &self.stream
    }

    /// Returns the [Span] of the opening delimiter.
    #[inline]
    pub fn start_span(&self) -> Span {
        let whole_span = self.span();
        Span::new(
            whole_span.file_id(),
            whole_span.start(),
            whole_span.start() + 1,
        )
    }

    /// Returns the [Span] of the closing delimiter.
    #[inline]
    pub fn end_span(&self) -> Span {
        let whole_span = self.span();
        Span::new(whole_span.file_id(), whole_span.end() - 1, whole_span.end())
    }
}

impl<'src> Spanned for Group<'src> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<'src> Token<'src> for Group<'src> {
    fn from_token_tree<'a>(token_tree: &'a TokenTree<'src>) -> Option<&'a Self> {
        match token_tree {
            TokenTree::Group(group) => Some(group),
            _ => None,
        }
    }

    fn matches(token_tree: &TokenTree) -> bool {
        matches!(token_tree, TokenTree::Group(_))
    }
}

/// Either a token or a group of tokens.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenTree<'src> {
    Literal(Literal<'src>),
    Reserved(Reserved),
    Punct(Punct),
    Group(Group<'src>),
}

impl<'src> TokenTree<'src> {
    /// Returns the postfix binding power of this [TokenTree] (if it can be used a postfix
    /// operator).
    pub fn postfix_binding_power(&self) -> Option<(u8, ())> {
        match self {
            Self::Group(group) => group.delim().postfix_binding_power(),
            _ => None,
        }
    }
}

impl<'src> Spanned for TokenTree<'src> {
    fn span(&self) -> Span {
        match self {
            Self::Literal(token) => token.span(),
            Self::Reserved(token) => token.span(),
            Self::Punct(token) => token.span(),
            Self::Group(token) => token.span(),
        }
    }
}

/// A stream of tokens.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TokenStream<'src> {
    tokens: Vec<TokenTree<'src>>,
}

impl<'src> TokenStream<'src> {
    /// Creates a [TokenStream] from the provided raw array of items.
    #[inline]
    pub fn from_raw(tokens: Vec<TokenTree<'src>>) -> Self {
        Self { tokens }
    }

    /// Returns a slice of the tokens in this [TokenStream].
    #[inline]
    pub fn tokens(&self) -> &[TokenTree<'src>] {
        &self.tokens
    }

    /// Returns a mutable slice of the tokens in this [TokenStream].
    #[inline]
    pub fn tokens_mut(&mut self) -> &mut [TokenTree<'src>] {
        &mut self.tokens
    }

    /// Creates an iterator through the items in this [TokenStream].
    #[inline]
    pub fn iter(&self) -> TokenIter<'_, 'src> {
        TokenIter {
            stream: &self.tokens,
        }
    }
}

/// An iterator through the items in a [TokenStream].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TokenIter<'a, 'src> {
    stream: &'a [TokenTree<'src>],
}

impl<'a, 'src> TokenIter<'a, 'src> {
    /// Returns the next token in the [TokenIter] without advancing it.
    #[inline]
    pub fn peek(&self) -> Option<&TokenTree<'src>> {
        self.peek_nth(0)
    }

    /// Returns the *n*th future token in the [TokenIter] without advancing it.
    pub fn peek_nth(&self, n: usize) -> Option<&TokenTree<'src>> {
        self.stream.get(n)
    }

    /// Expects the next token to be the provided token type.  Returns [`Recoverable::Yes`] if it
    /// was not.
    pub fn expect<T: Token<'src>>(&mut self) -> Result<&T, Recoverable> {
        match self.peek() {
            Some(token) => {
                if T::matches(token) {
                    Ok(T::from_token_tree(self.next().unwrap()).unwrap())
                } else {
                    Err(Recoverable::Yes)
                }
            }
            None => Err(Recoverable::Yes),
        }
    }

    /// Expects the next token to be a literal of the provided type.  Returns [`Recoverable::Yes`]
    /// if it was not.
    pub fn expect_literal(&mut self, kind: LiteralKind) -> Result<&Literal, Recoverable> {
        let old = self.clone();
        match self.next() {
            Some(TokenTree::Literal(literal)) if literal.kind == kind => Ok(literal),
            _ => {
                *self = old;
                Err(Recoverable::Yes)
            }
        }
    }

    /// Expects the next token to be a reserved word of the provided type. Returns
    /// [`Recoverable::Yes`] if it was not.
    pub fn expect_reserved(&mut self, kind: ReservedWord) -> Result<&Reserved, Recoverable> {
        let old = self.clone();
        match self.next() {
            Some(TokenTree::Reserved(reserved)) if reserved.kind == kind => Ok(reserved),
            _ => {
                *self = old;
                Err(Recoverable::Yes)
            }
        }
    }

    /// Expects the next token to be a punctuator of the provided type. Returns
    /// [`Recoverable::Yes`] if it was not.
    pub fn expect_punct(&mut self, kind: PunctKind) -> Result<&Punct, Recoverable> {
        let old = self.clone();
        match self.next() {
            Some(TokenTree::Punct(punct)) if punct.kind == kind => Ok(punct),
            _ => {
                *self = old;
                Err(Recoverable::Yes)
            }
        }
    }

    /// Expects the next token to be a group of the provided type. Returns [`Recoverable::Yes`] if
    /// it was not.
    pub fn expect_group(&mut self, delimiter: Delimiter) -> Result<&Group, Recoverable> {
        let old = self.clone();
        match self.next() {
            Some(TokenTree::Group(group)) if group.delim() == delimiter => Ok(group),
            _ => {
                *self = old;
                Err(Recoverable::Yes)
            }
        }
    }
}

impl<'a, 'src> Iterator for TokenIter<'a, 'src> {
    type Item = &'a TokenTree<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.stream.get(0)?;
        self.stream = &self.stream[1..];
        Some(item)
    }
}

/// Any value which is a token.
pub trait Token<'src>: 'src + Sized {
    /// Returns `true` if the provided [TokenTree] matches this token.
    fn matches(token_tree: &TokenTree<'src>) -> bool;

    /// Attempts to convert a [TokenTree] into this specific token.
    fn from_token_tree<'a>(token_tree: &'a TokenTree<'src>) -> Option<&'a Self>;
}
