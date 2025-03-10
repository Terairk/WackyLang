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

use crate::source::SourcedNode;
use chumsky::input::Input;
use chumsky::Parser;
use std::io;
use std::io::Write;
use thiserror::Error;

pub mod hir;
mod lower_ast;

type SN<T> = SourcedNode<T>;

#[derive(Debug, Error)]
pub enum AstLoweringPhaseError {
    #[error(
        "Encountered error when lowering AST, corresponding error report written to provided output"
    )]
    AstLoweringErrorWritten,
    #[error(transparent)]
    IoError(#[from] io::Error),
}

// /// # Errors
// /// TODO: add errors docs
// ///
// #[allow(
//     clippy::missing_panics_doc,
//     clippy::expect_used,
//     clippy::needless_pass_by_value
// )]
// #[inline]
// pub fn ast_lowering_phase<S: AsRef<str>, W: Write + Clone>(
//     source: S,
//     program_ast: ast::Program,
//     ast_lowering_error_code: i32,
//     stream_type: StreamType,
//     output_stream: W,
// ) -> Result<(Program, FuncSymbolTable), AstLoweringPhaseError> {
//     let source = source.as_ref();
//
//     // perform lowering
//     let AstLoweringPhaseResult {
//         output,
//         errors,
//         func_symbol_table,
//     } = lower_ast(program_ast);
//
//     // Done to appease the borrow checker while displaying errors
//     if !errors.is_empty() {
//         for e in &errors {
//             build_syntactic_report(
//                 e,
//                 source,
//                 ast_lowering_error_code,
//                 stream_type,
//                 output_stream.clone(),
//             )?;
//         }
//         return Err(AstLoweringPhaseError::AstLoweringErrorWritten);
//     }
//
//     Ok((output, func_symbol_table))
// }
