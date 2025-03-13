//! This phase lowers the AST into Higher Intermediate Representation by:
//! 1) Renaming all (repeating and non-repeating) variables, so all variable identifiers are unique
//! 2) Eliminating scopes in statements
//! 3) Eliminating parentheses in expressions
//! 4) Removing all error-nodes from the parsing phase
//! 5) Lowering all conditional statement constructs to [`hir::Stat::IfThenElse`]
//! 6) Lowering all loop constructs into a combination of [`hir::Stat::LoopDo`], [`hir::Stat::IfThenElse`], and [`hir::Stat::Break`]

use crate::parsing::ast;
use crate::wacc_hir::hir::Program;
use crate::wacc_hir::lower_ast::{lower_ast, AstLoweringResult, HirFuncSymbolTable};
use crate::{build_semantic_report, StreamType};
use std::io;
use std::io::Write;
use thiserror::Error;

pub mod hir;
pub mod lower_ast;

#[derive(Debug, Error)]
pub enum AstLoweringPhaseError {
    #[error(
        "Encountered error when lowering AST, corresponding error report written to provided output"
    )]
    AstLoweringErrorWritten,
    #[error(transparent)]
    IoError(#[from] io::Error),
}

#[derive(Debug)]
pub struct AstLoweringPhaseOutput {
    pub hir_program: Program,
    pub func_symbol_table: HirFuncSymbolTable,
    pub ident_counter: usize,
}

/// # Errors
/// TODO: add errors docs
///
#[allow(
    clippy::missing_panics_doc,
    clippy::expect_used,
    clippy::needless_pass_by_value
)]
#[inline]
pub fn ast_lowering_phase<S: AsRef<str>, W: Write + Clone>(
    source: S,
    program_ast: ast::Program,
    ast_lowering_error_code: i32,
    stream_type: StreamType,
    output_stream: W,
) -> Result<AstLoweringPhaseOutput, AstLoweringPhaseError> {
    let source = source.as_ref();

    // perform lowering
    let AstLoweringResult {
        output,
        errors,
        func_symbol_table,
        ident_counter,
    } = lower_ast(program_ast);

    // Done to appease the borrow checker while displaying errors
    if !errors.is_empty() {
        for e in errors {
            build_semantic_report(
                e,
                source,
                ast_lowering_error_code,
                stream_type,
                output_stream.clone(),
            )?;
        }
        return Err(AstLoweringPhaseError::AstLoweringErrorWritten);
    }

    Ok(AstLoweringPhaseOutput {
        hir_program: output,
        func_symbol_table,
        ident_counter,
    })
}
