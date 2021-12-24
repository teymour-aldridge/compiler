use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::diagnostics::span::IndexOnlySpan;

#[derive(Debug)]
pub enum TyCheckError {
    ConstraintGatheringError(ConstraintGatheringError),
    TypeMismatch,
}

impl From<ConstraintGatheringError> for TyCheckError {
    fn from(err: ConstraintGatheringError) -> Self {
        Self::ConstraintGatheringError(err)
    }
}

impl TyCheckError {
    pub fn report(&self, id: usize) -> Diagnostic<usize> {
        match self {
            TyCheckError::ConstraintGatheringError(err) => err.report(id),
            TyCheckError::TypeMismatch => Diagnostic::error().with_message(
                "Your program contains a type error! This unhelpfulmessage will be updated
                    soon.",
            ),
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
    pub fn report(&self, id: usize) -> Diagnostic<usize> {
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
