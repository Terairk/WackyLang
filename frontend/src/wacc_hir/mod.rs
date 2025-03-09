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

pub mod hir;
mod lower_ast;
