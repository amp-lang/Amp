//! The abstract syntax trees for Amp.

use crate::{
    codemap::{Span, Spanned},
    Context,
};

use super::{
    parser::Recoverable,
    token::{LiteralKind, TokenIter},
};

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
