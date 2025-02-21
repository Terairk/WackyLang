#![allow(clippy::arbitrary_source_item_ordering)]
use std::collections::HashMap;

use crate::ast::{Expr, Func, FuncParam, Ident, Program, RValue, Stat, StatBlock};
use crate::error::SemanticError;
use crate::fold_program::Folder;
use crate::fold_program::{BoxedSliceFold as _, NonEmptyFold as _};
use crate::source::SourcedNode;
use crate::types::{SemanticType, Type};
use std::fmt;
use std::hash::Hash;
use std::mem;

/* This file contains information pertaining to the renaming side of our compiler
 * The renaming phase is responsible for renaming all variables to unique_names
 * Usually using a counter i.e x -> x@1, y -> y@2 etc
 * Notably functions don't get renamed
 * We use a linear flat table to check scope
 * The main function is a rename(UntypedAST) -> RenamedAST
 */

// RenamedName is simply a struct that contains the original identifier and a unique id
// We make sure its Hashable as we may want to use it for a HashMap ID
//
// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;
type RenamedAST = Program<RenamedName, ()>;
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct RenamedName {
    pub ident: Ident,
    pub uuid: usize,
}

// Customised debug for prettier debugging
impl fmt::Debug for RenamedName {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}", self.ident, self.uuid)
    }
}

impl RenamedName {
    // We use a counter to generate unique ids, counter is automatically incremented
    // Note we use the same counter for all identifiers
    // Takes in SN<Ident> as this is what we always get from the AST
    #[inline]
    fn new_sn(counter: &mut usize, ident: &SN<Ident>) -> SN<Self> {
        // would rather crash in debug builds than define a saturating
        // so we can change this to u128
        // though I suspect we'd have bigger problems before then
        #[allow(clippy::arithmetic_side_effects)]
        *counter += 1;
        SN::new(
            Self {
                ident: ident.inner().clone(),
                uuid: *counter,
            },
            ident.span(),
        )
    }

    // Function used to create a rogue renamed so we can still build tree
    // even if we have errors
    fn new_sn_0(ident: &SN<Ident>) -> SN<Self> {
        SN::new(
            Self {
                ident: ident.inner().clone(),
                uuid: 0,
            },
            ident.span(),
        )
    }
}

// We handle functions separately from variables since its easier
#[derive(Debug)]
pub struct IdFuncTable {
    pub functions: HashMap<Ident, (SemanticType, Vec<SemanticType>)>,
}

// This struct helps us keep track of the identifiers in the current block
// Useful for checking for duplicates with scoping
struct IDMapEntry {
    renamed_name: SN<RenamedName>,
    from_current_block: bool,
}

impl IDMapEntry {
    const fn new(renamed_name: SN<RenamedName>, from_current_block: bool) -> Self {
        Self {
            renamed_name,
            from_current_block,
        }
    }

    // Helper function to create a new IDMapEntry with from_current_block set to false
    // Used when you enter a new scope and need to make a new entry
    fn create_false(&self) -> Self {
        Self {
            renamed_name: self.renamed_name.clone(),
            from_current_block: false,
        }
    }
}

// struct responsible for traversing/folding the AST
// holds state relevant for the renaming phase
pub struct Renamer {
    pub id_func_table: IdFuncTable,
    errors: Vec<SemanticError>,
    in_main: bool,
    counter: usize,
    identifier_map: HashMap<Ident, IDMapEntry>,
}

impl Renamer {
    #[inline]
    pub fn new() -> Self {
        Self {
            id_func_table: IdFuncTable {
                functions: HashMap::new(),
            },
            identifier_map: HashMap::new(),
            counter: 0,
            in_main: true,
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }

    #[inline]
    pub fn return_errors(&self) -> Vec<SemanticError> {
        self.errors.clone()
    }

    #[inline]
    pub const fn get_func_table(&self) -> &IdFuncTable {
        &self.id_func_table
    }

    // This function is used to create a copy of the current identifier map
    // with from_current_block set to false, this is so we can allow shadowing
    // of identifiers provided they're created in a new scope
    fn copy_id_map_with_false(&self) -> HashMap<Ident, IDMapEntry> {
        self.identifier_map
            .iter()
            .map(|(k, v)| (k.clone(), v.create_false()))
            .collect()
    }

    #[inline]
    pub fn lookup_func_args(&self, ident: &SN<Ident>) -> Vec<SemanticType> {
        if let Some((_, args)) = self.id_func_table.functions.get(ident) {
            args.clone()
        } else {
            Vec::new()
        }
    }
    #[inline]
    pub fn lookup_func_return_type(&self, ident: &SN<Ident>) -> SemanticType {
        if let Some((return_type, _)) = self.id_func_table.functions.get(ident) {
            return_type.clone()
        } else {
            SemanticType::Error(ident.span())
        }
    }

    #[inline]
    pub const fn counter(&self) -> usize {
        self.counter
    }

    // Helper function to fold a statement with a new identifier map
    // so we don't have to worry about resetting the map
    // in one situation, it gets too difficult so we manually need to revert
    // back to manually adjusting the map
    fn with_temporary_map<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let new_map = self.copy_id_map_with_false();
        let old_id_map = mem::replace(&mut self.identifier_map, new_map);
        let result = f(self);
        self.identifier_map = old_id_map;
        result
    }
}

impl Default for Renamer {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl Folder for Renamer {
    type N = Ident;
    type T = ();
    type OutputN = RenamedName;
    type OutputT = ();

    #[inline]
    fn fold_program(
        &mut self,
        program: Program<Self::N, Self::T>,
    ) -> Program<Self::OutputN, Self::OutputT> {
        // Build function table
        if let Err(err) = build_func_table(&mut self.id_func_table, &program) {
            self.add_error(err);
        }

        let folded_funcs = program.funcs.fold_with(|func| self.fold_func(func));

        let folded_body = self.fold_stat_block(program.body);

        // Return type depends on individual implementation
        // Just make sure make_program returns Self::Output
        self.make_program(folded_funcs, folded_body)
    }

    // When you enter a new function, you create a new scope just for the args,
    // Then you create another scope for the body of the function
    // We also keep track if we're in the main function
    #[inline]
    fn fold_func(&mut self, func: Func<Self::N, Self::T>) -> Func<Self::OutputN, Self::OutputT> {
        self.in_main = false;
        let new_map = self.copy_id_map_with_false();
        let old_id_map = mem::replace(&mut self.identifier_map, new_map);
        let params = func.params.fold_with(|param| self.fold_func_param(param));
        let body = self.with_temporary_map(|slf| slf.fold_stat_block(func.body));
        self.identifier_map = old_id_map;
        self.in_main = true;

        Func {
            return_type: func.return_type, // Type remains unchanged
            name: func.name,
            params,
            body,
        }
    }

    #[inline]
    fn fold_func_param(&mut self, param: FuncParam<Self::N>) -> FuncParam<Self::OutputN> {
        // shouldn't panic since we check the key exists
        let name = &param.name;
        if self.identifier_map.contains_key(name.inner())
            && self.identifier_map[name.inner()].from_current_block
        {
            self.add_error(SemanticError::DuplicateIdent(name.clone()));
            // return dummy Stat
            return FuncParam {
                r#type: param.r#type,
                name: RenamedName::new_sn_0(name),
            };
        }

        let unique_name = RenamedName::new_sn(&mut self.counter, name);
        let map_entry = IDMapEntry::new(unique_name.clone(), true);
        self.identifier_map.insert(name.inner().clone(), map_entry);

        FuncParam {
            r#type: param.r#type,
            name: unique_name,
        }
    }

    // We fold a stat block by creating a new scope for the block
    // then we fold the statements in the block within that scope
    #[inline]
    fn fold_stat_block(
        &mut self,
        block: StatBlock<Self::N, Self::T>,
    ) -> StatBlock<Self::OutputN, Self::OutputT> {
        let folded_block =
            self.with_temporary_map(|slf| block.0.map_with(|stat| slf.fold_stat_sn(stat)));
        StatBlock(folded_block)
    }

    #[inline]
    fn fold_var_definition(
        &mut self,
        r#type: SN<Type>,
        name: SN<Self::N>,
        rvalue: SN<RValue<Self::N, Self::T>>,
    ) -> Stat<Self::OutputN, Self::OutputT> {
        // Sorry about the lots of clones here
        // Evaluate the rhs before creating unique name to not allow int x = x
        // where x is not defined yet
        // resolved_expr returns a new copy of the initializer with any variables renamed
        let resolved_rvalue = self.fold_rvalue(rvalue);

        // Check for duplicate id after resolving rvalue
        // shouldn't panic since we check the key exists
        if self.identifier_map.contains_key(name.inner())
            && self.identifier_map[name.inner()].from_current_block
        {
            self.add_error(SemanticError::DuplicateIdent(name.clone()));
            // return dummy Stat
            return Stat::VarDefinition {
                r#type,
                name: RenamedName::new_sn_0(&name),
                rvalue: resolved_rvalue,
            };
        }

        let unique_name = RenamedName::new_sn(&mut self.counter, &name);
        let map_entry = IDMapEntry::new(unique_name.clone(), true);
        self.identifier_map.insert(name.inner().clone(), map_entry);

        Stat::VarDefinition {
            r#type,
            name: unique_name,
            rvalue: resolved_rvalue,
        }
    }

    #[inline]
    fn fold_stat_return(
        &mut self,
        expr: SN<Expr<Self::N, Self::T>>,
    ) -> Stat<Self::OutputN, Self::OutputT> {
        // If we do a return in main, we add an error
        if self.in_main {
            self.add_error(SemanticError::ReturnInMain(expr.span()));
        }
        Stat::Return(self.fold_expr(expr))
    }

    #[inline]
    fn fold_expr_ident(
        &mut self,
        ident: SN<Self::N>,
        r#type: Self::T,
    ) -> Expr<Self::OutputN, Self::OutputT> {
        if let Some(entry) = self.identifier_map.get(ident.inner()) {
            let renamed_name = &entry.renamed_name;
            Expr::Ident(renamed_name.clone(), r#type)
        } else {
            self.add_error(SemanticError::UndefinedIdent(ident.clone()));
            // Return a dummy value so we can maybe very hopefully
            // allow multiple semantic errors
            Expr::Ident(RenamedName::new_sn_0(&ident), r#type)
        }
    }

    #[inline]
    fn fold_name_sn(&mut self, name: SN<Self::N>) -> SN<Self::OutputN> {
        if let Some(entry) = self.identifier_map.get(name.inner()) {
            let renamed_name = &entry.renamed_name;
            renamed_name.clone()
        } else {
            self.add_error(SemanticError::UndefinedIdent(name.clone()));
            // Return a dummy value so we can maybe very hopefully
            // allow multiple semantic errors
            RenamedName::new_sn_0(&name)
        }
    }

    #[inline]
    fn fold_funcname_sn(&mut self, name: SN<Ident>) -> SN<Ident> {
        // Use ident part of name and then check if it exists in the function table
        // return name regardless but add an error if it doesn't exist
        let ident = name.inner();
        if self.id_func_table.functions.contains_key(ident) {
        } else {
            self.add_error(SemanticError::UndefinedIdent(name.clone()));
            // Return a dummy value so we can maybe very hopefully
            // allow multiple semantic errors
        }

        name
    }

    // OutputT is a () so return empty value, this looks wacc as hell
    // but rust requires this
    #[inline]
    fn fold_type(&mut self, _ty: Self::T) -> Self::OutputT {}
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

// We return Renamer here so we can get the errors and some state from it
#[inline]
pub fn rename(program: Program<Ident, ()>) -> (RenamedAST, Renamer) {
    let mut renamer = Renamer::new();
    let renamed_program = renamer.fold_program(program);
    (renamed_program, renamer)
}
