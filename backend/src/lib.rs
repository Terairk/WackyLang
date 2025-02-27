pub mod assembly_ast;
pub mod assembly_fix;
pub mod assembly_trans;
pub mod emission;
pub mod gen_predefined;
pub mod predefined;
pub mod replace_pseudo;
pub mod types;

#[inline]
pub fn backend() {
    println!("backend");
}

#[cfg(test)]
mod tests {}
