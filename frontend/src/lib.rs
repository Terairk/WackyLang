//! TODO: crate documentation
//!
//! this is here as a placeholder documentation
//!
//!

// enable Rust-unstable features for convenience
#![feature(trait_alias)]
#![feature(stmt_expr_attributes)]
#![feature(unboxed_closures)]
#![feature(assert_matches)]

use crate::source::SourcedSpan;
use ariadne::{CharSet, Label, Report, Source};
use std::io;
use std::io::Write;

pub mod multi_item;
pub mod node;
pub mod parsing;
pub mod source;
pub mod wacc_hir;
pub mod wacc_thir;

/// Namespace for all the type/trait aliases used by this crate.
pub(crate) mod alias {
    use internment::Intern;

    pub type InternStr = Intern<str>;
}

/// Namespace for crate-wide extension traits/methods
pub(crate) mod ext {}

pub(crate) mod private {
    // sealed traits support
    pub trait Sealed {}
    impl<T: ?Sized> Sealed for T {}
}

/// Output stream to check for [`ariadne`] reports.
#[derive(Clone, Copy, Debug)]
pub enum StreamType {
    /// Standard Output
    Stdout,
    /// Standard Error
    Stderr,
}

pub(crate) struct SemanticError<MHeader: AsRef<str>, MBody: ToString> {
    pub message_header: MHeader,
    pub message_body: MBody,
    pub span: SourcedSpan,
}

/// # Errors
/// TODO: add errors docs
#[allow(clippy::unwrap_used)]
#[inline]
pub(crate) fn build_semantic_report<
    MHeader: AsRef<str>,
    MBody: ToString,
    E: Into<SemanticError<MHeader, MBody>>,
    S: AsRef<str>,
    W: Write,
>(
    semantic_error: E,
    source: S,
    error_code: i32,
    stream_type: StreamType,
    output_stream: W,
) -> io::Result<()> {
    // decompose error
    let SemanticError {
        message_header: error_message_header,
        message_body: error_message_body,
        span: error_span,
    } = semantic_error.into();

    // build report
    let config = ariadne::Config::default().with_char_set(CharSet::Ascii);
    let report = Report::build(ariadne::ReportKind::Error, error_span.clone())
        .with_config(config)
        .with_message(error_message_header.as_ref())
        .with_code(error_code)
        .with_label(Label::new(error_span.clone()).with_message(error_message_body))
        .finish();
    let cache = (error_span.source_id().clone(), Source::from(source));
    match stream_type {
        StreamType::Stdout => report.write_for_stdout(cache, output_stream),
        StreamType::Stderr => report.write(cache, output_stream),
    }
}
