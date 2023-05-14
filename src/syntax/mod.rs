//! The scanner, parser and abstract syntax trees for Amp.

use crate::{codemap::Span, diag::SyntaxDiagnostics, syntax::scanner::Scanner, Context};

use self::{
    scanner::Token,
    token::{
        Delimiter, Group, Literal, LiteralKind, Punct, PunctKind, Reserved, ReservedWord,
        TokenStream, TokenTree,
    },
};

pub mod scanner;
pub mod token;

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
