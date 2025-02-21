pub mod assembly_ast;
pub mod assembly_trans;
pub mod gen_flags;

#[inline]
pub fn backend() {
    println!("backend");
}

#[cfg(test)]
mod tests {
    use super::*;
}
