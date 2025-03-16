//! This phase lowers the HIR into Typed Higher Intermediate Representation by:
//! 1) Typechecking and replacing syntactic  
//! TODO: complete this documentation
//!
//!

use crate::wacc_hir::AstLoweringPhaseOutput;
use crate::wacc_thir::lower_hir::{
    HirLoweringResult, IdentSymbolTable, ThirFuncSymbolTable, lower_hir,
};
use crate::wacc_thir::thir::Program;
use crate::{StreamType, build_semantic_report};
use std::io;
use std::io::Write;
use thiserror::Error;

pub mod lower_hir;
pub(crate) mod optimizations;
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

#[derive(Debug)]
pub struct HirLoweringPhaseOutput {
    pub thir_program: Program,
    pub func_symbol_table: ThirFuncSymbolTable,
    pub hir_ident_symbol_table: IdentSymbolTable,
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
pub fn hir_lowering_phase<S: AsRef<str>, W: Write + Clone>(
    source: S,
    ast_lowering: AstLoweringPhaseOutput,
    hir_lowering_error_code: i32,
    stream_type: StreamType,
    output_stream: W,
    should_tailrec_optimize: bool,
) -> Result<HirLoweringPhaseOutput, crate::wacc_hir::AstLoweringPhaseError> {
    let source = source.as_ref();

    // perform lowering
    let HirLoweringResult {
        output,
        errors,
        func_symbol_table,
        hir_ident_symbol_table,
        ident_counter,
    } = lower_hir(ast_lowering, should_tailrec_optimize);

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

    Ok(HirLoweringPhaseOutput {
        thir_program: output,
        func_symbol_table,
        hir_ident_symbol_table,
        ident_counter,
    })
}
