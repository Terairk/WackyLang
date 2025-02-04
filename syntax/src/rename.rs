#![allow(clippy::arbitrary_source_item_ordering)]
use std::collections::HashMap;

use crate::ast::{Expr, Func, FuncParam, Ident, Program, Stat, StatBlock};
use crate::source::{SourcedNode, SourcedSpan};
use crate::types::{SemanticType, Type};
use std::hash::{Hash, Hasher};

// TODO: check if ident can be SN<Ident>, only problem is that
// Node does't implement Hash
#[derive(Clone, Debug)]
pub struct RenamedName {
    ident: SN<Ident>,
    uuid: usize,
}

// Make Hash depend only on the ident and uuid so that we can use it in a HashMap
// while still keeping the SN for error reporting
impl Hash for RenamedName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
        self.uuid.hash(state);
    }
}

impl PartialEq for RenamedName {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.uuid == other.uuid
    }
}

impl RenamedName {
    fn new(counter: &mut usize, ident: SN<Ident>) -> Self {
        *counter += 1;
        Self {
            ident,
            uuid: *counter,
        }
    }
}

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
    identifier_map: HashMap<Ident, RenamedName>,
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
            identifier_map: HashMap::new(),
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

fn resolve_declaration(
    context: &mut RenameContext,
    r#type: SN<Type>,
    name: SN<Ident>,
    rvalue: Expr<Ident, ()>,
) -> Result<Stat<RenamedName, ()>, SemanticError> {
    let id_map = &mut context.identifier_map;
    if id_map.contains_key(&name.inner()) {
        return Err(SemanticError::DuplicateIdent(name.clone()));
    }
    // Evaluate the rhs before creating unique name to not allow int x = x
    // where x is not defined yet
    let resolved_rvalue = resolve_expr(context, rvalue)?;

    let unique_name = RenamedName::new(&mut context.counter, name.clone());
    id_map.insert(name.inner().clone(), unique_name);

    Ok(Stat::VarDefinition {
        r#type: r#type.clone(),
        name: SN::new(unique_name, name.span()),
        rvalue: resolved_rvalue,
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
