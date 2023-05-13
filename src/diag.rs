//! Diagnostics for the Amp compiler.

use crate::codemap::Span;

/// The severity level of a [Label] in a [Diag].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Level {
    Error,
    Note,
}

/// A label in a [Diag].
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Label {
    pub level: Level,
    pub message: String,
    pub highlight: Option<Span>,
}

/// A diagnostic, which generally consists of one or more [Label]s.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Diag {
    pub labels: Vec<Label>,
}

impl Diag {
    /// Creates a blank [Diag].
    #[inline]
    pub fn new() -> Self {
        Self { labels: Vec::new() }
    }

    /// Adds a label to the diagnostic.
    #[inline]
    pub fn label(
        mut self,
        level: Level,
        message: impl Into<String>,
        highlight: Option<Span>,
    ) -> Self {
        self.labels.push(Label {
            level,
            message: message.into(),
            highlight,
        });
        self
    }

    /// Adds an error label to the diagnostic.
    #[inline]
    pub fn error(self, message: impl Into<String>, highlight: Option<Span>) -> Self {
        self.label(Level::Error, message, highlight)
    }

    /// Adds a note label to the diagnostic.
    #[inline]
    pub fn note(self, message: impl Into<String>, highlight: Option<Span>) -> Self {
        self.label(Level::Note, message, highlight)
    }
}

/// A trait for contexts which [Diag]s can be reported to.
pub trait Report {
    /// Reports a diagnostic to this context.
    fn report(&mut self, diag: Diag);
}

/// A trait which allows
pub trait SyntaxDiagnostics: Report {
    fn invalid_character(&mut self, offending_char: char, offending_span: Span) {
        self.report(Diag::new().error(
            format!(
                "invalid character '{}'",
                offending_char.escape_debug().collect::<String>()
            ),
            Some(offending_span),
        ))
    }
}

impl<T: Report> SyntaxDiagnostics for T {}
