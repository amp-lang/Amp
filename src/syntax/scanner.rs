//! Amp's lexical scanner.

use std::str::Chars;

use unicode_xid::UnicodeXID;

use crate::{
    codemap::Span,
    diag::SyntaxDiagnostics,
    syntax::token::{
        Delimiter, Group, Literal, LiteralKind, Punct, PunctKind, Reserved, ReservedWord,
        TokenStream, TokenTree,
    },
    Context,
};

/// The kind of a token outputted directly by a [Scanner].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    /// Any invalid token, such as an invalid character or unterminated string.  If this token is
    /// encountered, the [Scanner] has already outputted a diagnostic for it.
    Invalid,

    /// `&`
    And,

    /// `:`
    Colon,

    /// `,`
    Comma,

    /// `=`
    Eq,

    /// `;`
    Semi,

    /// `(`
    ParenOpen,

    /// `)`
    ParenClose,

    /// `{`
    BraceOpen,

    /// `}`
    BraceClose,

    /// `const`
    KConst,

    /// `func`
    KFunc,

    /// `return`
    KReturn,

    /// `mut`
    KMut,

    /// A Unicode XID identifier token.
    Id,

    /// A string token.
    ///
    /// ```amp
    /// "Hello, world!"
    /// ```
    Str,

    /// A decimal integer.
    ///
    /// ```amp
    /// 12345678
    /// 1234_5678
    /// ```
    DecInt,
}

/// The state of the lexical scanner.
pub struct Scanner<'cx, 'src> {
    pub(crate) cx: &'cx mut Context,

    /// The span of the current token.
    span: Span,

    /// The source string, used to get the string slice of a token.
    src: &'src str,

    /// The characters of the source string.
    chars: Chars<'src>,
}

impl<'cx, 'src> Scanner<'cx, 'src> {
    /// Creates a new [Scanner] for the provided source string.
    pub fn new(cx: &'cx mut Context, file_id: usize, src: &'src str) -> Self {
        Self {
            cx,
            span: Span::new(file_id, 0, 0),
            src,
            chars: src.chars(),
        }
    }

    /// Returns the [Span] of the current token.
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }

    /// Returns a string slice of the current token.
    #[inline]
    pub fn slice(&self) -> &'src str {
        &self.src[self.span.start()..self.span.end()]
    }

    /// Returns the next token.
    pub fn peek(&mut self) -> Option<Token> {
        let init_span = self.span;
        let init_chars = self.chars.clone();

        let token = self.next::<false>();

        self.span = init_span;
        self.chars = init_chars;

        token
    }

    /// Returns the next character in the source string.  Moves the end of the current span to the
    /// end of the returned character, if any.
    fn next_char(&mut self) -> Option<char> {
        let char = self.chars.next()?;
        self.span.end += char.len_utf8() as u32;
        Some(char)
    }

    /// Returns the next character in the source string, without advancing the iterator.
    #[inline]
    fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }

    /// Returns the *n*th future character in the source string, without advancing the iterator.
    #[inline]
    fn peek_nth_char(&self, n: usize) -> Option<char> {
        self.chars.clone().nth(n)
    }

    /// Iterates to the next span, preparing for the next token.
    #[inline]
    fn next_span(&mut self) {
        self.span.start = self.span.end;
    }

    /// Returns `true` if the provided character can start an identifier token.
    #[inline]
    fn is_id_start(char: char) -> bool {
        char == '_' || char.is_xid_start()
    }

    /// Returns `true` if the provided character can continue an identifier token after it is
    /// started.
    #[inline]
    fn is_id_continue(char: char) -> bool {
        char.is_xid_continue()
    }

    /// Returns `true` if the provided character is a decimal digit.
    #[inline]
    fn is_digit(char: char) -> bool {
        char >= '0' && char <= '9'
    }

    /// Skips all skippable tokens (whitespace, comments) until a non-skippable character is found.
    /// Should be called before [next_span](#method.next_span), as leading whitespace should not be
    /// included in a token's span.
    fn skip(&mut self) {
        while let Some(token) = self.peek_char() {
            if token.is_whitespace() {
                self.next_char();
            } else if token == '/'
                && self.peek_nth_char(1) == Some('/')
                // don't skip doc comments
                && self.peek_nth_char(2) != Some('/')
            {
                while let Some(char) = self.next_char() {
                    if char == '\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }

    /// Scans a single identifier token.  Assumes a valid identifier starting character has been
    /// found.
    fn scan_id(&mut self) -> Token {
        while let Some(next_char) = self.peek_char() {
            if !Self::is_id_continue(next_char) {
                break;
            }

            self.next_char();
        }

        match self.slice() {
            "const" => Token::KConst,
            "func" => Token::KFunc,
            "return" => Token::KReturn,
            "mut" => Token::KMut,
            _ => Token::Id,
        }
    }

    /// Scans a single string token.  Assumes a starting quote (`"`) has found and iterated past.
    ///
    /// TODO: scan escape codes
    fn scan_str<const REPORT: bool>(&mut self) -> Token {
        while let Some(char) = self.peek_char() {
            if char == '"' {
                break;
            }

            self.next_char();
        }

        if self.next_char() != Some('"') {
            if REPORT {
                self.cx.unterminated_string(self.span());
            }
            return Token::Invalid;
        }

        Token::Str
    }

    /// Scans a single number token.  Assumes that a valid starting digit was found but not
    /// iterated past.
    ///
    /// TODO: non-decimal integers, floats.
    /// TODO: disallow identifier prefixes
    fn scan_num(&mut self) -> Token {
        while let Some(digit) = self.peek_char() {
            if !Self::is_digit(digit) && digit != '_' {
                break;
            }

            self.next_char();
        }

        Token::DecInt
    }

    /// Returns the next token in the scanner.
    pub(crate) fn next<const REPORT: bool>(&mut self) -> Option<Token> {
        self.skip();
        self.next_span();

        let first_char = self.peek_char()?;

        if Self::is_id_start(first_char) {
            self.next_char(); // skip first character for optimization purposes
            return Some(self.scan_id());
        } else if first_char == '"' {
            self.next_char();
            return Some(self.scan_str::<REPORT>());
        } else if Self::is_digit(first_char) {
            return Some(self.scan_num());
        }

        self.next_char();
        Some(match first_char {
            '&' => Token::And,
            ':' => Token::Colon,
            ',' => Token::Comma,
            '=' => Token::Eq,
            ';' => Token::Semi,
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            '{' => Token::BraceOpen,
            '}' => Token::BraceClose,
            _ => {
                // None of the previous checks matched any tokens supported by Amp, so we can assume that
                // the character was invalid.
                if REPORT {
                    self.cx.invalid_character(first_char, self.span());
                }

                Token::Invalid
            }
        })
    }
}

impl<'cx, 'src> Iterator for Scanner<'cx, 'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next::<true>()
    }
}

fn scan_group<'src>(
    scanner: &mut Scanner<'_, 'src>,
    closing: Delimiter,
) -> Result<TokenStream<'src>, ()> {
    let mut ok = true;
    let closing_token = match closing {
        Delimiter::Brace => Token::BraceClose,
        Delimiter::Paren => Token::ParenClose,
    };
    let start_span = scanner.span();
    let mut tokens = Vec::new();

    while let Some(next_token) = scanner.peek() {
        if next_token == closing_token {
            break;
        }

        tokens.push(match scan_token(scanner) {
            Ok(token) => token,
            Err(()) => {
                ok = false;
                continue;
            }
        });
    }

    if scanner.next::<true>() != Some(closing_token) {
        scanner.cx.unclosed_delimiter(Span::new(
            start_span.file_id(),
            start_span.start(),
            scanner.span().end(),
        ));
        return Err(());
    }

    if !ok {
        return Err(());
    }

    Ok(TokenStream::from_raw(tokens))
}

/// Scans a single token, returning the produced token tree.
fn scan_token<'src>(scanner: &mut Scanner<'_, 'src>) -> Result<TokenTree<'src>, ()> {
    Ok(match scanner.next::<true>().ok_or(())? {
        Token::Invalid => {
            return Err(());
        }
        Token::And => TokenTree::Punct(Punct::new(scanner.span(), PunctKind::And)),
        Token::Colon => TokenTree::Punct(Punct::new(scanner.span(), PunctKind::Colon)),
        Token::Comma => TokenTree::Punct(Punct::new(scanner.span(), PunctKind::Comma)),
        Token::Eq => TokenTree::Punct(Punct::new(scanner.span(), PunctKind::Eq)),
        Token::Semi => TokenTree::Punct(Punct::new(scanner.span(), PunctKind::Semi)),
        Token::BraceOpen => {
            let start = scanner.span();
            let tokens = scan_group(scanner, Delimiter::Brace)?;
            TokenTree::Group(Group::new(
                Span::new(start.file_id(), start.start(), scanner.span().end()),
                Delimiter::Brace,
                tokens,
            ))
        }
        Token::ParenOpen => {
            let start = scanner.span();
            let tokens = scan_group(scanner, Delimiter::Paren)?;
            TokenTree::Group(Group::new(
                Span::new(start.file_id(), start.start(), scanner.span().end()),
                Delimiter::Paren,
                tokens,
            ))
        }
        Token::BraceClose => {
            scanner
                .cx
                .unmatched_closing_delimiter(scanner.slice(), scanner.span());
            return Err(());
        }
        Token::ParenClose => {
            scanner
                .cx
                .unmatched_closing_delimiter(scanner.slice(), scanner.span());
            return Err(());
        }
        Token::KConst => TokenTree::Reserved(Reserved::new(scanner.span(), ReservedWord::Const)),
        Token::KFunc => TokenTree::Reserved(Reserved::new(scanner.span(), ReservedWord::Func)),
        Token::KReturn => TokenTree::Reserved(Reserved::new(scanner.span(), ReservedWord::Return)),
        Token::KMut => TokenTree::Reserved(Reserved::new(scanner.span(), ReservedWord::Mut)),
        Token::DecInt => TokenTree::Literal(Literal::new(
            scanner.span(),
            LiteralKind::DecInt,
            scanner.slice(),
        )),
        Token::Id => TokenTree::Literal(Literal::new(
            scanner.span(),
            LiteralKind::Id,
            scanner.slice(),
        )),
        Token::Str => TokenTree::Literal(Literal::new(
            scanner.span(),
            LiteralKind::Str,
            scanner.slice(),
        )),
    })
}

/// Scans the provided source, returning the parsed tokens if successful.
///
/// Outputs any recovered diagnostics to the provided context.
pub fn scan<'src>(
    cx: &mut Context,
    file_id: usize,
    src: &'src str,
) -> Result<TokenStream<'src>, ()> {
    let mut ok = true;
    let mut scanner = Scanner::new(cx, file_id, src);
    let mut tokens = Vec::new();

    while let Some(_) = scanner.peek() {
        tokens.push(match scan_token(&mut scanner) {
            Ok(token) => token,
            Err(_) => {
                ok = false;
                continue;
            }
        });
    }

    if !ok {
        return Err(());
    }

    Ok(TokenStream::from_raw(tokens))
}
