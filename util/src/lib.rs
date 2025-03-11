// enable Rust-unstable features for convenience
// #![feature(trait_alias)]
// #![feature(stmt_expr_attributes)]
// #![feature(specialization)]

pub mod cfg;
pub mod gen_flags;
pub mod nonempty;
pub mod opt_flags;
pub mod rust_gadt_playground;

// Public Re-exports
pub use cfg::CFG;
pub use cfg::Instruction;
pub use cfg::SimpleInstr;

// pub(crate) mod private {
//     // sealed traits support
//     pub trait Sealed {}
//     impl<T: ?Sized> Sealed for T {}
// }
