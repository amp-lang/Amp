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

    /// Reports that an argument list was expected in a function declaration, such as
    /// `func(my_param: i32): i32`.
    ///
    /// # Params
    /// 1. The span of the offending `func` declaration, up to the name.
    fn expected_function_args(&mut self, offending_span: Span) {
        self.report(Diag::new().error("expected function argument list", Some(offending_span)))
    }

    /// Reports that a return type was expected in a function declaration.
    ///
    /// # Params
    /// 1. The span of the function declaration up until the type annotation.
    fn expected_function_return_type(&mut self, offending_span: Span) {
        self.report(Diag::new().error("expected function return type", Some(offending_span)))
    }

    /// Reports that an operator was found but no valid operand followed.
    ///
    /// # Params
    /// 1. The span of the expression up until the operator.
    fn expected_operand_expression(&mut self, offending_span: Span) {
        self.report(Diag::new().error("expected expression as operand", Some(offending_span)))
    }
}

impl<T: Report> SyntaxDiagnostics for T {}

/// Semantic analysis diagnostics.
pub trait SemaDiagnostics: Report {
    /// Reports that there are two symbols in this scope with the same name.
    ///
    /// # Params
    /// 1. The name of the problematic declaration.
    /// 2. The span of the offending declaration.
    /// 3. The span of the first declaration, if any.
    fn duplicate_declaration(
        &mut self,
        name: &str,
        offending_span: Span,
        original_span: Option<Span>,
    ) {
        let mut diag = Diag::new().error(
            format!("'{}' is already declared in this scope", name),
            Some(offending_span),
        );

        if let Some(original_span) = original_span {
            diag = diag.note("originally declared here", Some(original_span));
        }

        self.report(diag)
    }

    /// Reports that a named value could not be found.
    ///
    /// # Params
    /// 1. The name of the requested value.
    /// 2. The span of the requested value.
    fn could_not_resolve_name(&mut self, name: &str, offending_span: Span) {
        self.report(Diag::new().error(
            format!("could not resolve '{}'", name),
            Some(offending_span),
        ))
    }

    /// Creates the base for a type mismatch error.
    ///
    /// # Params
    /// 1. The name of the expected type.
    /// 2. The name of the type which was found.
    /// 3. The span where the value of the invalid type was found.
    fn type_mismatch_base(
        &mut self,
        expected_type: &str,
        got_type: &str,
        offending_span: Span,
    ) -> Diag {
        Diag::new().error(
            format!(
                "expected value of type `{}`, got `{}`",
                expected_type, got_type
            ),
            Some(offending_span),
        )
    }

    /// A value was used in a type's position.
    ///
    /// # Params
    /// 1. The name of the type which was found.
    /// 2. The span where the type value was expected.
    fn invalid_type_in_type_position(&mut self, got_type: &str, offending_span: Span) {
        let diag = self
            .type_mismatch_base("type", got_type, offending_span)
            .note("values cannot be used in a type's position", None);
        self.report(diag);
    }

    /// Reports that a general type mismatch was found.
    ///
    /// # Params
    /// 1. The name of the expected type.
    /// 2. The name of the found type.
    /// 3. The span of the mismatched value.
    fn type_mismatch(&mut self, expected_type: &str, got_type: &str, offending_span: Span) {
        let diag = self.type_mismatch_base(expected_type, got_type, offending_span);
        self.report(diag);
    }

    /// Reports that a type mismatch was found in a return statement.
    ///
    /// # Params
    /// 1. The name of the expected type.
    /// 2. The name of the found type.
    /// 3. The span of the mismatched value.
    /// 4. The span of the function signature, if applicable (from the `func` keyword to the end
    ///    of the return type).
    fn return_type_mismatch(
        &mut self,
        expected_type: &str,
        got_type: &str,
        offending_span: Span,
        signature_span: Option<Span>,
    ) {
        let mut diag = self.type_mismatch_base(expected_type, got_type, offending_span);

        if let Some(span) = signature_span {
            diag = diag.note("function signature declared here", Some(span));
        }

        self.report(diag.note("return must match function's signature", None));
    }

    /// Reports that an invalid argument was found.
    ///
    /// # Params
    /// 1. The name of the type that was expected.
    /// 2. The name of the type which was found.
    /// 3. The type of the called function.
    /// 4. The span of the invalid argument.
    fn function_argument_type_mismatch(
        &mut self,
        expected_type: &str,
        got_type: &str,
        callee_type: &str,
        offending_span: Span,
    ) {
        let diag = self
            .type_mismatch_base(expected_type, got_type, offending_span)
            .note(format!("arguments must match `{}`", callee_type), None);
        self.report(diag);
    }

    /// Reports that a function call was invalid for the provided argument length.
    ///
    /// # Params
    /// 1. The expected number of arguments.
    /// 2. The number of arguments found.
    /// 3. The type of the callee (the function being called).
    /// 4. The span of the call expression.
    fn function_call_argument_length_mismatch(
        &mut self,
        expected: usize,
        got: usize,
        callee_type: &str,
        offending_span: Span,
    ) {
        self.report(
            Diag::new()
                .error(
                    format!("expected {} argument(s), got {}", expected, got),
                    Some(offending_span),
                )
                .note(format!("callee is of type `{}`", callee_type), None),
        )
    }

    /// Reports that an attempt was made to call a non-function value.
    ///
    /// # Params
    /// 1. The name of the invalid callee's type.
    /// 2. The span of the function call expression.
    fn calling_non_function_type(&mut self, got_type: &str, offending_span: Span) {
        self.report(Diag::new().error(
            format!("attempt to call non-function type `{}`", got_type),
            Some(offending_span),
        ))
    }

    /// Reports that a runtime value was used when a constant value was expected.
    ///
    /// # Params
    /// 1. The span of the offending runtime value.
    fn value_not_known_at_compile_time(&mut self, offending_span: Span) {
        self.report(Diag::new().error(
            "this value is not known at compile time",
            Some(offending_span),
        ))
    }

    /// Reports that an invalid statement was found at the root of a module.
    ///
    /// # Params
    /// 1. The span of the invalid statement.
    fn invalid_statement_at_root(&mut self, offending_span: Span) {
        self.report(Diag::new().error("invalid statement at root of module", Some(offending_span)))
    }

    /// Reports that an invalid statement was found.
    ///
    /// # Params
    /// 1. The span of the invalid statement.
    fn invalid_statement(&mut self, offending_span: Span) {
        self.report(Diag::new().error("invalid statement", Some(offending_span)))
    }

    /// Reports that an invalid expression was found.
    ///
    /// # Params
    /// 1. The span of the invalid expression.
    fn invalid_expression(&mut self, offending_span: Span) {
        self.report(Diag::new().error("invalid expression", Some(offending_span)))
    }

    /// Reports that a `type` cannot be recieved from an argument.
    ///
    /// # Params
    /// 1. The span of the invalid argument.
    fn cannot_use_type_as_argument(&mut self, offending_span: Span) {
        self.report(Diag::new().error(
            "functions cannot recieve `type`s as arguments",
            Some(offending_span),
        ))
    }

    /// Reports that a `type` cannot be outputted as a return value.
    ///
    /// # Params
    /// 1. The span of the invalid return type.
    fn cannot_use_type_as_return(&mut self, offending_span: Span) {
        self.report(Diag::new().error(
            "functions cannot output `type`s as return values",
            Some(offending_span),
        ))
    }
}

impl<T: Report> SemaDiagnostics for T {}
