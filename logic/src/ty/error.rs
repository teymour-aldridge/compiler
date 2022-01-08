use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::diagnostics::span::IndexOnlySpan;

use super::track::ErrorReporter;

#[derive(Debug)]
pub enum TyCheckError<'ctx> {
    ConstraintGatheringError(ConstraintGatheringError),
    Reportable(ErrorReporter<'ctx>),
}

impl<'ctx> From<ConstraintGatheringError> for TyCheckError<'ctx> {
    fn from(err: ConstraintGatheringError) -> Self {
        Self::ConstraintGatheringError(err)
    }
}

impl<'ctx> TyCheckError<'ctx> {
    pub fn report<ID>(self, id: ID) -> Diagnostic<ID>
    where
        ID: Copy,
    {
        match self {
            TyCheckError::ConstraintGatheringError(err) => err.report(id),
            TyCheckError::Reportable(reporter) => reporter.report(id),
        }
    }
}

#[derive(Debug)]
pub enum ConstraintGatheringError {
    CannotAssignToExpression {
        span: IndexOnlySpan,
        explanation: String,
    },
    UnresolvableFunction {
        span: IndexOnlySpan,
        explanation: String,
    },
    MismatchedFunctionCall {
        span: IndexOnlySpan,
        explanation: String,
    },
    ReturnOutsideFunction {
        span: IndexOnlySpan,
        explanation: String,
    },
}

impl ConstraintGatheringError {
    pub fn report<ID>(&self, id: ID) -> Diagnostic<ID> {
        // todo: give this a better message
        let diagnostic =
            Diagnostic::error().with_message("Your program contains an invalid reference");
        match self {
            ConstraintGatheringError::CannotAssignToExpression { span, explanation }
            | ConstraintGatheringError::UnresolvableFunction { span, explanation }
            | ConstraintGatheringError::MismatchedFunctionCall { span, explanation }
            | ConstraintGatheringError::ReturnOutsideFunction { span, explanation } => diagnostic
                .with_labels(vec![
                    Label::primary(id, span.range()).with_message(explanation)
                ]),
        }
    }
}
