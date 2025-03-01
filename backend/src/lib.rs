pub mod assembly_ast;
pub mod assembly_fix;
pub mod assembly_trans;
pub mod emission;
pub mod future_types_playground;
pub mod predefined;
pub mod replace_pseudo;

#[inline]
pub fn backend() {
    println!("backend");
}

#[cfg(test)]
mod tests {}
