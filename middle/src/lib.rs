// enable Rust-unstable features for convenience
#![feature(trait_alias)]
#![feature(stmt_expr_attributes)]

pub mod ast_transform;
pub mod optimizations;
pub mod types;
pub mod wackir;

#[inline]
pub fn middle() {
    println!("middle");
}

#[cfg(test)]
mod tests {}
