//! TODO: crate documentation
//!
//! this is here as a placeholder documentation
//!
//!

// here, AST lowering is performed:
// renaming is done, and after:
// >eliminate scopes in statements,
// >parenthesis in expressions
// >remove error nodes from AST
// >remove all loop kinds and change to loop+if+break

use crate::parsing::ast;
use crate::source::SourcedNode;

pub mod hir;
mod lower_ast;

type SN<T> = SourcedNode<T>;

#[derive(Debug, Clone)]
pub enum AstLoweringError {
    // ArityMismatch(SN<ast::Ident>, usize, usize),
    DuplicateIdent(SN<ast::Ident>),
    // TypeMismatch(SourcedSpan, SemanticType, SemanticType),
    // AssignmentWithBothSidesUnknown(SourcedSpan),
    // MismatchedArgCount(SourcedSpan, usize, usize),
    // InvalidIndexType(SourcedSpan, SemanticType),
    // InvalidFreeType(SourcedSpan, SemanticType),
    // InvalidNumberOfIndexes(SourcedSpan, usize, usize),
    UndefinedIdent(SN<ast::Ident>),
    // ReturnInMain(SourcedSpan),
}
