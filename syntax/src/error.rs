use crate::ast::Ident;
use crate::source::{SourcedNode, SourcedSpan};
use crate::types::SemanticType;

type SN<T> = SourcedNode<T>;

#[derive(Debug, Clone)]
pub enum SemanticError {
    ArityMismatch(SN<Ident>, usize, usize),
    DuplicateIdent(SN<Ident>),
    // TODO: import strum crate to make it easier to convert this to a string
    TypeMismatch(SourcedSpan, SemanticType, SemanticType),
    AssignmentWithBothSidesUnknown(SourcedSpan),
    SimpleTypeMismatch(SemanticType, SemanticType), // TODO: remove this temp error
    MismatchedArgCount(SourcedSpan, usize, usize),
    InvalidIndexType(SourcedSpan, SemanticType),
    InvalidNumberOfIndexes(usize),
    UndefinedIdent(SN<Ident>),
    ReturnInMain,
}

pub fn semantic_error_to_reason(error: &SemanticError) -> String {
    match error {
        SemanticError::ArityMismatch(ident, expected, actual) => {
            format!(
                "Function '{}' expects {} arguments, but got {}",
                ident.inner(),
                expected,
                actual
            )
        }
        SemanticError::DuplicateIdent(ident) => {
            format!("Duplicate identifier '{}'", ident.inner())
        }
        SemanticError::TypeMismatch(span, expected, actual) => {
            format!("Expected type {}, but got {}", expected, actual)
        }
        SemanticError::AssignmentWithBothSidesUnknown(span) => {
            format!("Cannot assign to unknown type")
        }
        SemanticError::SimpleTypeMismatch(expected, actual) => {
            format!("Expected type {}, but got {}", expected, actual)
        }
        SemanticError::MismatchedArgCount(span, expected, actual) => {
            format!("Expected {} arguments, but got {}", expected, actual)
        }
        SemanticError::InvalidIndexType(span, ty) => {
            format!("Invalid index type {}", ty)
        }
        SemanticError::InvalidNumberOfIndexes(count) => {
            format!("Expected 1 index, but got {}", count)
        }
        SemanticError::UndefinedIdent(ident) => {
            format!("Undefined identifier '{}'", ident.inner())
        }
        SemanticError::ReturnInMain => {
            format!("Cannot return from main function")
        }
    }
}
