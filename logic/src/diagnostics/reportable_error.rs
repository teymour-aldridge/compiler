use codespan_reporting::diagnostic::{Diagnostic, Label};

use super::span::Span;

#[derive(Debug)]
pub struct ReportableError {
    span: Span,
    explanation: String,
}

impl ReportableError {
    pub fn new(span: Span, explanation: String) -> Self {
        Self { span, explanation }
    }

    pub fn report<ID>(&self, id: ID) -> Diagnostic<ID>
    where
        ID: Copy,
    {
        let diagnostic: Diagnostic<ID> =
            Diagnostic::error().with_message("Your program contains an error!");

        diagnostic.with_labels(vec![
            Label::primary(id, self.span.index_only().range()).with_message(&self.explanation)
        ])
    }

    pub fn explanation(&self) -> &str {
        self.explanation.as_ref()
    }

    pub fn could_not_infer_ty(span: Span) -> ReportableError {
        Self::new(
            span,
            "Could not infer the type of this variable.".to_owned(),
        )
    }
}

pub type ReportableResult = Result<(), ReportableError>;
