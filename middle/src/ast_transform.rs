use std::collections::HashMap;

use syntax::ast::Program;
use syntax::rename::RenamedName;
use syntax::typecheck::TypeResolver;
use syntax::{rename::IdFuncTable, types::SemanticType};

use crate::wackir::{ConvertToMidIdent as _, MidIdent, WackProgram};

/* ================== PUBLIC API ================== */

type TypedAST = Program<RenamedName, SemanticType>;

// Only take in type_resolver so i can discard the heck out of it
// I can probably take it in by reference but I'd prefer not to clone
pub fn lower_program(program: TypedAST, type_resolver: TypeResolver) -> WackProgram {
    let context = create_lowering_context(type_resolver);
    unimplemented!();
    // let mut wack_program = WackProgram::Program {
    //     top_level: Vec::new(),
    //     body: Vec::new(),
    // };
    // for func in program.funcs.iter() {
    //     let top_level = lower_func(func, &context);
    //     wack_program.top_level.push(top_level);
    // }
    // wack_program
}

/* ================== INTERNAL API ================== */
struct LoweringContext {
    counter: usize,
    func_table: IdFuncTable,
    symbol_table: HashMap<MidIdent, SemanticType>,
}

fn create_lowering_context(type_resolver: TypeResolver) -> LoweringContext {
    let renamer = type_resolver.renamer;
    let mut counter = renamer.counter();
    let func_table = renamer.id_func_table;
    let symbol_table: HashMap<MidIdent, SemanticType> = type_resolver
        .symid_table
        .into_iter()
        .map(|(id, symid)| (id.to_mid_ident(&mut counter), symid))
        .collect();
    LoweringContext {
        counter,
        func_table,
        symbol_table,
    }
}
