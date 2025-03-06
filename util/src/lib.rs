// enable Rust-unstable features for convenience
#![feature(trait_alias)]
#![feature(stmt_expr_attributes)]
#![feature(type_alias_impl_trait)]
#![feature(specialization)]

pub mod dyn_traits;
pub mod gen_flags;
pub mod nonempty;
pub mod rust_gadt_playground;

pub(crate) mod private {
    // sealed traits support
    pub trait Sealed {}
    impl<T: ?Sized> Sealed for T {}
}
