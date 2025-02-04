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
    DuplicateIdent(SN<Ident>),
    UndefinedIdent(SN<Ident>),
    // TODO: import strum crate to make it easier to convert this to a string
    TypeMismatch(SN<Expr<RenamedName, SemanticType>>, Type, Type),
    ArityMismatch(SN<Ident>, usize, usize),
}

// We handle functions separately from variables since its easier
struct IdFuncTable {
    functions: HashMap<RenamedName, (SemanticType, Vec<SemanticType>)>,
}

struct SymbolTable {
    symbols: HashMap<RenamedName, SemanticType>,
}

struct RenameContext {
    id_func_table: IdFuncTable,
    symbol_table: SymbolTable,
    errors: Vec<SemanticError>,
}

impl RenameContext {
    fn new() -> Self {
        Self {
            id_func_table: IdFuncTable::new(),
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }
}
