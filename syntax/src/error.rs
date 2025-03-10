use crate::ast::Ident;
use crate::source::{SourcedNode, SourcedSpan};
use crate::types::SemanticType;

type SN<T> = SourcedNode<T>;

#[derive(Debug, Clone)]
pub enum SemanticError {
    ArityMismatch(SN<Ident>, usize, usize),
    DuplicateIdent(SN<Ident>),
    TypeMismatch(SourcedSpan, SemanticType, SemanticType),
    AssignmentWithBothSidesUnknown(SourcedSpan),
    MismatchedArgCount(SourcedSpan, usize, usize),
    InvalidIndexType(SourcedSpan, SemanticType),
    InvalidFreeType(SourcedSpan, SemanticType),
    InvalidNumberOfIndexes(SourcedSpan, usize, usize),
    UndefinedIdent(SN<Ident>),
    ReturnInMain(SourcedSpan),
}

impl SemanticError {
    #[inline]
    #[must_use]
    pub fn span(&self) -> SourcedSpan {
        match self {
            Self::ArityMismatch(s, _, _) | Self::DuplicateIdent(s) | Self::UndefinedIdent(s) => {
                s.span()
            }
            Self::TypeMismatch(s, _, _)
            | Self::AssignmentWithBothSidesUnknown(s)
            | Self::MismatchedArgCount(s, _, _)
            | Self::InvalidIndexType(s, _)
            | Self::InvalidFreeType(s, _)
            | Self::InvalidNumberOfIndexes(s, _, _)
            | Self::ReturnInMain(s) => s.clone(),
        }
    }

    #[inline]
    #[must_use]
    pub fn semantic_error_to_reason(&self) -> String {
        match self {
            SemanticError::ArityMismatch(ident, expected, actual) => {
                format!(
                    "Function '{}' expects {} arguments, but got {}",
                    ident.inner(),
                    expected,
                    actual
                )
            }
            SemanticError::DuplicateIdent(ident) => {
                format!(
                    "Identifier '{}' already defined in current scope.",
                    ident.inner()
                )
            }
            SemanticError::TypeMismatch(_span, actual, expected) => {
                format!("Expected {}, but got {}", expected, actual)
            }
            SemanticError::AssignmentWithBothSidesUnknown(_span) => {
                format!("Cannot assign to unknown type")
            }
            SemanticError::MismatchedArgCount(_span, expected, actual) => {
                format!("Expected {} arguments, but got {}", expected, actual)
            }
            SemanticError::InvalidIndexType(_span, ty) => {
                // TODO: need to make sure SemanticType is a valid WACC type when displaying as a
                // String and not smth we defined
                format!("{} cannot be used to index into an array", ty)
            }
            SemanticError::InvalidNumberOfIndexes(_span, got, expected) => {
                format!("Expected maximum {} index(es), but got {}", expected, got)
            }
            SemanticError::UndefinedIdent(ident) => {
                format!("Undefined identifier '{}'", ident.inner())
            }
            SemanticError::ReturnInMain(_) => {
                format!("Cannot return from main function")
            }
            SemanticError::InvalidFreeType(_span, actual) => {
                format!(
                    "Cannot free {} type, can only free an array or a pair",
                    actual
                )
            }
        }
    }
}
