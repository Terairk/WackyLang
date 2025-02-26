pub mod ast_transform;
pub mod wackir;

#[inline]
pub fn middle() {
    println!("middle");
}

#[cfg(test)]
mod tests {}
