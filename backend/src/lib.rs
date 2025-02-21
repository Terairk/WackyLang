pub mod assembly_ast;
pub mod assembly_fix;
pub mod assembly_trans;
pub mod gen_flags;
pub mod replace_pseudo;

#[inline]
pub fn backend() {
    println!("backend");
}

#[cfg(test)]
mod tests {
    use super::*;
}
