pub mod assembly_ast;
pub mod assembly_trans;

#[inline]
pub fn backend() {
    println!("backend");
}

#[cfg(test)]
mod tests {
    use super::*;
}
