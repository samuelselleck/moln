use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::lexer::{Span, Token};

#[derive(Debug)]
pub struct Annotation {
    span: Span,
    text: String,
    annotation_type: AnnotationType,
}

#[derive(Debug)]
pub enum AnnotationType {
    Primary,
    Secondary,
}

#[derive(Debug)]
pub struct CompilerError {
    _error_code: usize,
    short_description: String,
    annotations: Vec<Annotation>,
    help: Option<String>,
}

pub type CResult<T> = Result<T, CompilerError>;

impl CompilerError {
    pub fn new(short_description: impl Into<String>) -> Self {
        Self {
            // TODO fill in error codes
            _error_code: 0,
            short_description: short_description.into(),
            annotations: Vec::new(),
            help: None,
        }
    }

    pub fn unexpected_token(token: Token, message: impl Into<String>) -> Self {
        Self::new("unexpected character(s)").annotation(token.span, message)
    }

    pub fn annotation(mut self, span: Span, text: impl Into<String>) -> Self {
        self.annotations.push(Annotation {
            annotation_type: match self.annotations.is_empty() {
                true => AnnotationType::Primary,
                false => AnnotationType::Secondary,
            },
            span,
            text: text.into(),
        });
        self
    }

    pub fn print_with_file(
        &self,
        file_name: &str,
        file: &str,
    ) -> Result<(), codespan_reporting::files::Error> {
        let mut files = SimpleFiles::new();
        let file_id = files.add(file_name, file);
        let diagnostic = Diagnostic::error()
            .with_message(&self.short_description)
            .with_code("E")
            .with_labels(
                self.annotations
                    .iter()
                    .map(|a| match a.annotation_type {
                        AnnotationType::Primary => {
                            Label::primary(file_id, a.span).with_message(&a.text)
                        }
                        AnnotationType::Secondary => {
                            Label::secondary(file_id, a.span).with_message(&a.text)
                        }
                    })
                    .collect(),
            )
            .with_notes(self.help.as_slice().to_vec());

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;
        Ok(())
    }
}
