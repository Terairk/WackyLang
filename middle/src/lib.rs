// enable Rust-unstable features for convenience
#![feature(trait_alias)]
#![feature(stmt_expr_attributes)]

pub mod optimizations;
pub mod thir_transform;
pub mod types;
pub mod wackir;

/// Namespace for all the type/trait aliases used by this crate.
pub(crate) mod alias {
    use internment::Intern;

    pub type InternStr = Intern<str>;
}

#[inline]
pub fn middle() {
    println!("middle");
}

#[cfg(test)]
mod tests {}
