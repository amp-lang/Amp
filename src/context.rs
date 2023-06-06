use std::io::Write;

use crate::codespan_reporting;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label, Severity},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{Color, ColorChoice, ColorSpec, StandardStream},
        Chars, Config, Styles,
    },
};

use crate::diag::{Diag, Level, Report};

/// A shared context used to communicate with Amp's compiler interface.
#[derive(Clone, Debug, Default)]
pub struct Context {
    files: SimpleFiles<String, String>,
    diags: Vec<Diag>,
}

impl Context {
    /// Creates a new [Context].
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new(),
            diags: Vec::new(),
        }
    }

    /// Adds a file into the context, returning the ID now associated with it.
    #[inline]
    pub fn add_file(&mut self, name: impl Into<String>, source: impl Into<String>) -> usize {
        self.files.add(name.into(), source.into())
    }

    /// Returns the files in the [Context].
    #[inline]
    pub fn files(&self) -> &SimpleFiles<String, String> {
        &self.files
    }

    /// Returns the files in the [Context].
    #[inline]
    pub fn files_mut(&mut self) -> &mut SimpleFiles<String, String> {
        &mut self.files
    }

    /// Reports a diagnostic to this [Context], preparing it to be emitted next time
    /// [emit](#method.emit) is called.
    #[inline]
    pub fn report(&mut self, diag: Diag) {
        self.diags.push(diag);
    }

    /// Returns the configuration to use when emitting diagnostics.
    fn config() -> Config {
        let mut intense = ColorSpec::new().set_intense(true).clone();
        let white = intense.set_fg(Some(Color::White)).clone();
        let red = intense.set_fg(Some(Color::Red)).clone();
        let yellow = intense.set_fg(Some(Color::Yellow)).clone();
        let cyan = intense.set_fg(Some(Color::Cyan)).clone();
        let green = intense.set_fg(Some(Color::Green)).clone();

        let styles = Styles {
            header_bug: red.clone(),
            header_error: red.clone(),
            header_warning: yellow.clone(),
            header_help: cyan.clone(),
            header_note: cyan.clone(),
            header_message: white.clone(),
            primary_label_bug: green.clone(),
            primary_label_error: green.clone(),
            primary_label_warning: green.clone(),
            primary_label_help: green.clone(),
            primary_label_note: green.clone(),
            secondary_label: green.clone(),
            line_number: cyan.clone(),
            source_border: cyan.clone(),
            note_bullet: cyan.clone(),
        };

        Config {
            chars: Chars::ascii(),
            styles,
            ..Default::default()
        }
    }

    /// Emits all diagnostics in this [Context] to the terminal.
    pub fn emit(&self) -> Result<(), codespan_reporting::files::Error> {
        let config = Self::config();

        let mut stderr = StandardStream::stderr(ColorChoice::Auto);
        let mut stdout = StandardStream::stdout(ColorChoice::Auto);

        for diag in &self.diags {
            // Each Amp label corresponds to a `codespan-reporting` diagnostic, so convert each
            // label and emit them individually.
            for label in &diag.labels {
                let diag = Diagnostic::new(match label.level {
                    Level::Error => Severity::Error,
                    Level::Note => Severity::Note,
                })
                .with_message(label.message.clone())
                .with_labels(
                    label
                        .highlight
                        .iter()
                        .map(|highlight| Label::primary(highlight.file_id(), *highlight))
                        .collect(),
                );

                term::emit(
                    match label.level {
                        Level::Error => &mut stderr,
                        Level::Note => &mut stdout,
                    },
                    &config,
                    &self.files,
                    &diag,
                )?;
            }

            // Append newline between diagnostics to separate them better.
            write!(&mut stdout, "\n")?;
        }

        Ok(())
    }
}

impl Report for Context {
    fn report(&mut self, diag: Diag) {
        self.report(diag);
    }
}
