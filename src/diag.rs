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
    /// Reports that an invalid character was found during tokenization.
    ///
    /// # Params
    /// 1. The invalid character which was found.
    /// 2. The location where the invalid character was found.
    fn invalid_character(&mut self, offending_char: char, offending_span: Span) {
        self.report(Diag::new().error(
            format!(
                "invalid character '{}'",
                offending_char.escape_debug().collect::<String>()
            ),
            Some(offending_span),
        ))
    }

    /// Reports that an unterminated string was found during tokenization.
    ///
    /// # Params
    /// 1. The location of the unterminated string.
    fn unterminated_string(&mut self, offending_span: Span) {
        self.report(Diag::new().error("unterminated string", Some(offending_span)));
    }

    /// Reports that an unclosed delimiter was found.
    ///
    /// # Params
    /// 1. The span of the unclosed delimiter.
    fn unclosed_delimiter(&mut self, offending_span: Span) {
        self.report(Diag::new().error("unclosed delimiter", Some(offending_span)));
    }

    /// Reports that an unclosed delimiter was found.
    ///
    /// # Params
    /// 1. The span of the unmatched delimiter.
    fn unmatched_closing_delimiter(&mut self, offending_delimiter: &str, offending_span: Span) {
        self.report(Diag::new().error(
            format!("unmatched closing delimiter '{}'", offending_delimiter),
            Some(offending_span),
        ));
    }

    /// Reports that a too large integer literal token was found.
    ///
    /// # Params
    /// 1. The span of the integer.
    fn integer_too_large(&mut self, offending_span: Span) {
        self.report(
            Diag::new()
                .error("integer is too large", Some(offending_span))
                .note(format!("currently, {} is the max", u64::MAX), None),
        )
    }

    /// Reports that an invalid parameter was found in an argument list.
    ///
    /// # Params
    /// 1. The span of the invalid parameter.
    /// 2. The span of the end of the argument list.
    fn invalid_arglist_param(&mut self, offending_span: Span, arglist_end: Span) {
        self.report(
            Diag::new()
                .error(
                    "expected parameter or `)` in argument list",
                    Some(offending_span),
                )
                .note("argument list actually ends here", Some(arglist_end)),
        )
    }

    /// Reports that an argument list expected a comma or closing delimiter.
    ///
    /// # Params
    /// 1. The span where a comma/delimiter was expected.
    /// 2. The span of the end of the argument list.
    fn arglist_expected_comma_or_close(&mut self, offending_span: Span, arglist_end: Span) {
        self.report(
            Diag::new()
                .error("expected `,` or `)` here", Some(offending_span))
                .note("argument list actually ends here", Some(arglist_end)),
        )
    }

    /// Reports that a binding declaration was missing a valid name identifier.
    ///
    /// # Params
    /// 1. The span of the binding starting keyword (`const` or `var`).
    fn expected_binding_decl_name(&mut self, offending_span: Span) {
        self.report(Diag::new().error("expected a binding identifier", Some(offending_span)))
    }

    /// Reports that a type annotation was started but no type was found.
    ///
    /// # Params
    /// 1. The span of the `:` that started the type annotation.
    fn expected_type_annotation_type(&mut self, offending_span: Span) {
        self.report(Diag::new().error("expected type in type annotation", Some(offending_span)))
    }

    /// Reports that a `const` binding was missing a value.
    ///
    /// # Params
    /// 1. The span of the `const` keyword to the end of the type annotation/name.
    fn expected_const_binding_value(&mut self, offending_span: Span) {
        self.report(
            Diag::new()
                .error("expected value for `const` binding", Some(offending_span))
                .note(
                    "`const` bindings must be declared with a value known at compile time",
                    None,
                ),
        )
    }

    /// Reports that an invalid statement was found.
    ///
    /// # Params
    /// 1. The span of the statement.
    fn invalid_stmnt(&mut self, offending_span: Span) {
        self.report(Diag::new().error("invalid statement", Some(offending_span)))
    }

    /// Reports that a semicolon was expected after an expression.
    ///
    /// # Params
    /// 1. The span of the statement a semicolon was expected to follow.
    fn expected_semicolon(&mut self, offending_span: Span) {
        self.report(Diag::new().error("expected a semicolon", Some(offending_span)))
    }
}

impl<T: Report> SyntaxDiagnostics for T {}
