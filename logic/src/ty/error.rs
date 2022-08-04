use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{diagnostics::span::IndexOnlySpan, parse::table::ParseTable};

use super::track::ErrorReporter;

#[derive(Debug)]
pub enum TyCheckError {
    ConstraintGatheringError(ConstraintGatheringError),
    Reportable(ErrorReporter),
}

impl From<ConstraintGatheringError> for TyCheckError {
    fn from(err: ConstraintGatheringError) -> Self {
        Self::ConstraintGatheringError(err)
    }
}

impl TyCheckError {
    pub fn report<ID>(self, id: ID, table: &ParseTable<'_>) -> Diagnostic<ID>
    where
        ID: Copy,
    {
        match self {
            TyCheckError::ConstraintGatheringError(err) => err.report(id),
            TyCheckError::Reportable(reporter) => reporter.report(id, table),
        }
    }

    pub fn as_constraint_gathering_error(&self) -> Option<&ConstraintGatheringError> {
        if let Self::ConstraintGatheringError(v) = self {
            Some(v)
        } else {
            None
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
    UnresolvableRecord {
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
    LiteralForFieldOrMethodAccess {
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
            | ConstraintGatheringError::ReturnOutsideFunction { span, explanation }
            | ConstraintGatheringError::LiteralForFieldOrMethodAccess { span, explanation }
            | ConstraintGatheringError::UnresolvableRecord { span, explanation } => diagnostic
                .with_labels(vec![
                    Label::primary(id, span.range()).with_message(explanation)
                ]),
        }
    }
}
