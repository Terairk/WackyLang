//! This phase lowers the HIR into Typed Higher Intermediate Representation by:
//! 1) Typechecking and replacing syntactic  
//! TODO: complete this documentation
//!
//!

use crate::wacc_hir::hir;
use crate::wacc_hir::lower_ast::FuncSymbolTable;
use crate::wacc_thir::lower_hir::{lower_hir, HirLoweringPhaseResult, IdentSymbolTable};
use crate::wacc_thir::thir::Program;
use crate::{build_semantic_report, StreamType};
use std::io;
use std::io::Write;
use thiserror::Error;

pub mod lower_hir;
pub mod thir;
mod type_set;
pub mod types;

#[derive(Debug, Error)]
pub enum HirLoweringPhaseError {
    #[error(
        "Encountered error when lowering HIR, corresponding error report written to provided output"
    )]
    HirLoweringErrorWritten,
    #[error(transparent)]
    IoError(#[from] io::Error),
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
pub fn hir_lowering_phase<S: AsRef<str>, W: Write + Clone>(
    source: S,
    func_symbol_table: FuncSymbolTable,
    hir_program: hir::Program,
    hir_lowering_error_code: i32,
    stream_type: StreamType,
    output_stream: W,
) -> Result<(Program, FuncSymbolTable, IdentSymbolTable), crate::wacc_hir::AstLoweringPhaseError> {
    let source = source.as_ref();

    // perform lowering
    let HirLoweringPhaseResult {
        output,
        errors,
        func_symbol_table,
        hir_ident_symbol_table,
    } = lower_hir(func_symbol_table, hir_program);

    // Done to appease the borrow checker while displaying errors
    if !errors.is_empty() {
        for e in errors {
            build_semantic_report(
                e,
                source,
                hir_lowering_error_code,
                stream_type,
                output_stream.clone(),
            )?;
        }
        return Err(crate::wacc_hir::AstLoweringPhaseError::AstLoweringErrorWritten);
    }

    Ok((output, func_symbol_table, hir_ident_symbol_table))
}
