pub mod assembly_ast;
pub mod assembly_fix;
pub mod assembly_trans;
pub mod emission;
pub mod predefined;
pub mod regalloc;
pub mod registers;
pub mod replace_pseudo;

#[inline]
pub fn backend() {
    println!("backend");
}

#[cfg(test)]
mod tests {}
