use crate::codemap::{Span, Spanned};

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

/// The delimiter of a [Group] token.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Delimiter {
    /// `{}`
    Brace,

    /// `()`
    Paren,
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
}

impl<'src> Spanned for Group<'src> {
    fn span(&self) -> Span {
        self.span
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
            stream: self,
            cursor: 0,
        }
    }
}

/// An iterator through the items in a [TokenStream].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TokenIter<'a, 'src> {
    stream: &'a TokenStream<'src>,
    cursor: usize,
}

impl<'a, 'src> TokenIter<'a, 'src> {
    /// Returns the next token in the [TokenIter] without advancing it.
    #[inline]
    pub fn peek(&self) -> Option<&TokenTree<'src>> {
        self.peek_nth(0)
    }

    /// Returns the *n*th future token in the [TokenIter] without advancing it.
    pub fn peek_nth(&self, n: usize) -> Option<&TokenTree<'src>> {
        if self.cursor + n >= self.stream.tokens.len() {
            None
        } else {
            Some(&self.stream.tokens[self.cursor + n])
        }
    }
}

impl<'a, 'src> Iterator for TokenIter<'a, 'src> {
    type Item = &'a TokenTree<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.stream.tokens.len() {
            return None;
        }

        let item = &self.stream.tokens[self.cursor];
        self.cursor += 1;
        Some(item)
    }
}
