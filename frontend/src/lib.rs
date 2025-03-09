//! TODO: crate documentation
//!
//! this is here as a placeholder documentation
//!
//!

// enable Rust-unstable features for convenience
#![feature(trait_alias)]
#![feature(stmt_expr_attributes)]
#![feature(unboxed_closures)]

use chumsky::input::Input;
use chumsky::Parser;
use std::io::Write;

pub mod node;
pub mod parsing;
pub mod source;

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
