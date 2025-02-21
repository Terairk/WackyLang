use middle::wackir::WackProgram;

use crate::assembly_ast::AsmProgram;

/* ================== PUBLIC API ================== */

#[inline]
pub fn tacky_to_assembly(program: WackProgram) -> AsmProgram {
    let wack_functions = program.top_level;
    let main_wack_function = program.body;
    unimplemented!()
}

/* ================== INTERNAL API ================== */
