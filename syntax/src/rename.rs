#![allow(clippy::arbitrary_source_item_ordering)]
use std::collections::HashMap;

pub(crate) use crate::ast::{Expr, Func, FuncParam, Ident, Program, RenamedName, StatBlock};
use crate::source::{SourcedNode, SourcedSpan};
use crate::types::{SemanticType, Type};

// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;
type UntypedAST = Program<Ident, ()>;
type RenamedAST = Program<RenamedName, ()>;

// TODO: UndefinedIdent could be its own enum
// with specifics such as int x = x where x is not defined yet
pub enum SemanticError {
    ArityMismatch(SN<Ident>, usize, usize),
    DuplicateIdent(SN<Ident>),
    // TODO: import strum crate to make it easier to convert this to a string
    TypeMismatch(SN<Expr<RenamedName, SemanticType>>, Type, Type),
    UndefinedIdent(SN<Ident>),
}

// We handle functions separately from variables since its easier
struct IdFuncTable {
    functions: HashMap<Ident, (SemanticType, Vec<SemanticType>)>,
}

struct SymbolTable {
    symbols: HashMap<RenamedName, SemanticType>,
}

struct RenameContext {
    id_func_table: IdFuncTable,
    symbol_table: SymbolTable,
    counter: usize,
    errors: Vec<SemanticError>,
}

impl IdFuncTable {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
}

impl RenameContext {
    fn new() -> Self {
        Self {
            id_func_table: IdFuncTable::new(),
            symbol_table: SymbolTable::new(),
            counter: 0,
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }
}

mod rename {
    use super::*;

    // Module contains lots of boiler plate for folding/traversing the tree
}

fn build_func_table(
    id_func_table: &mut IdFuncTable,
    program: &Program<Ident, ()>,
) -> Result<(), SemanticError> {
    program.funcs.iter().try_for_each(|func| {
        if id_func_table.functions.contains_key(&func.name) {
            return Err(SemanticError::DuplicateIdent(func.name.clone()));
        }
        let (return_type, param_types) = (
            func.return_type.clone(),
            func.params
                .iter()
                .map(|param| param.r#type.to_semantic_type()),
        );
        let (return_type, param_types) = (return_type.to_semantic_type(), param_types.collect());
        id_func_table
            .functions
            .insert(func.name.inner().clone(), (return_type, param_types));
        Ok(())
    })
}

// mod fold {
//     use super::*;
//
//     pub trait Folder {
//         fn fold_program(&mut self, program: UntypedAST) -> RenamedAST;
//         fn fold_stat_block(
//             &mut self,
//             stat_block: StatBlock<Ident, ()>,
//         ) -> StatBlock<RenamedName, ()>;
//         fn fold_expr(&mut self, expr: Expr<Ident, ()>) -> Expr<RenamedName, ()>;
//         fn fold_func(&mut self, func: Func<Ident, ()>) -> Func<RenamedName, ()>;
//         fn fold_func_param(&mut self, func_param: FuncParam<Ident>) -> FuncParam<RenamedName>;
//     }
// }

// TODO: maybe have to return the counter i use so i can use it for IR Generation
fn rename(program: UntypedAST) -> Result<(RenamedAST, IdFuncTable), Vec<SemanticError>> {
    let mut context = RenameContext::new();
    unimplemented!()
    // // Do stuff
    //
    // // Return the renamed program and the function table
    // Ok((
    //     fold::Folder::fold_program(&mut context, program),
    //     context.id_func_table,
    // ))
}
