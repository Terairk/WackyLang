#![allow(clippy::arbitrary_source_item_ordering)]
use std::collections::HashMap;

use crate::ast::{Expr, Ident, Program, RValue, Stat, StatBlock};
use crate::fold_program::Folder;
use crate::fold_program::{BoxedSliceFold, NonEmptyFold};
use crate::source::SourcedNode;
use crate::types::{SemanticType, Type};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem;

// TODO: check if ident can be SN<Ident>, only problem is that
// Node does't implement Hash
#[derive(Clone)]
pub struct RenamedName {
    ident: Ident,
    uuid: usize,
}

impl fmt::Debug for RenamedName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}", self.ident, self.uuid)
    }
}

// Make Hash depend only on the ident and uuid so that we can use it in a HashMap
// while still keeping the SN for error reporting
impl Hash for RenamedName {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
        self.uuid.hash(state);
    }
}

impl PartialEq for RenamedName {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.uuid == other.uuid
    }
}

impl RenamedName {
    #[inline]
    fn new_sn(counter: &mut usize, ident: SN<Ident>) -> SN<Self> {
        // would rather crash in debug builds than define a saturating
        // so we can change this to u128
        // though i suspect we'd have bigger problems before then
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
    fn new_sn_0(ident: SN<Ident>) -> SN<Self> {
        SN::new(
            Self {
                ident: ident.inner().clone(),
                uuid: 0,
            },
            ident.span(),
        )
    }
}

// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;
type UntypedAST = Program<Ident, ()>;
type RenamedAST = Program<RenamedName, ()>;

// TODO: UndefinedIdent could be its own enum
// with specifics such as int x = x where x is not defined yet
// honestly for duplicate Ident, could probably list the other Ident it was duplicating
// for potentially better errors
#[derive(Debug, Clone)]
pub enum SemanticError {
    ArityMismatch(SN<Ident>, usize, usize),
    DuplicateIdent(SN<Ident>),
    // TODO: import strum crate to make it easier to convert this to a string
    TypeMismatch(SN<Expr<RenamedName, SemanticType>>, Type, Type),
    UndefinedIdent(SN<Ident>),
}

// We handle functions separately from variables since its easier
#[derive(Debug)]
pub struct IdFuncTable {
    functions: HashMap<Ident, (SemanticType, Vec<SemanticType>)>,
}

struct SymbolTable {
    symbols: HashMap<RenamedName, SemanticType>,
}

struct IDMapEntry {
    renamed_name: SN<RenamedName>,
    from_current_block: bool,
}

impl IDMapEntry {
    fn new(renamed_name: SN<RenamedName>, from_current_block: bool) -> Self {
        Self {
            renamed_name,
            from_current_block,
        }
    }

    fn create_false(&self) -> Self {
        Self {
            renamed_name: self.renamed_name.clone(),
            from_current_block: false,
        }
    }
}

// struct responsible for traversing/folding the AST
pub struct Renamer {
    id_func_table: IdFuncTable,
    // bool refers to from_current_block used for renaming
    identifier_map: HashMap<Ident, IDMapEntry>,
    counter: usize,
    errors: Vec<SemanticError>,
}

impl Renamer {
    pub fn new() -> Self {
        Self {
            id_func_table: IdFuncTable {
                functions: HashMap::new(),
            },
            identifier_map: HashMap::new(),
            counter: 0,
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }

    pub fn return_errors(&self) -> Vec<SemanticError> {
        self.errors.clone()
    }

    pub fn get_func_table(&self) -> &IdFuncTable {
        &self.id_func_table
    }

    fn copy_id_map(&self) -> HashMap<Ident, IDMapEntry> {
        self.identifier_map
            .iter()
            .map(|(k, v)| (k.clone(), v.create_false()))
            .collect()
    }

    // fn with_temporary_map<F, R>(&mut self, f: F) -> R
    // where
    //     F: FnOnce(&mut Self) -> R,
    // {
    //     let new_map = self.copy_id_map();
    //     let old_id_map = mem::replace(&mut self.identifier_map, new_map);
    //     let result = f(self);
    //     self.identifier_map = old_id_map;
    //     result
    // }
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

        let folded_body = self.fold_stat_block_sn(program.body);

        // Return type depends on individual implementation
        // Just make sure make_program returns Self::Output
        self.make_program(folded_funcs, folded_body)
    }

    #[inline]
    fn fold_stat_block(
        &mut self,
        block: StatBlock<Self::N, Self::T>,
    ) -> StatBlock<Self::OutputN, Self::OutputT> {
        // self.with_temporary_map(|slf| StatBlock(block.0.map_with(|stat| slf.fold_stat_sn(stat))))
        let new_map = self.copy_id_map();
        let old_id_map = mem::replace(&mut self.identifier_map, new_map);
        let folded_block = block.0.map_with(|stat| self.fold_stat_sn(stat));
        self.identifier_map = old_id_map;
        StatBlock(folded_block)
    }

    #[inline]
    fn fold_name_sn(&mut self, name: SN<Self::N>) -> SN<Self::OutputN> {
        if let Some(entry) = self.identifier_map.get(name.inner()) {
            let renamed_name = &entry.renamed_name;
            renamed_name.clone()
        } else {
            self.add_error(SemanticError::UndefinedIdent(name.clone()));
            // Return a dummy value so we can maybe maybe very hopefully
            // allow multiple semantic errors
            RenamedName::new_sn_0(name)
        }
    }

    #[inline]
    fn fold_var_definition(
        &mut self,
        r#type: SN<Type>,
        name: SN<Self::N>,
        rvalue: RValue<Self::N, Self::T>,
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
                name: RenamedName::new_sn_0(name),
                rvalue: resolved_rvalue,
            };
        }

        let unique_name = RenamedName::new_sn(&mut self.counter, name.clone());
        let map_entry = IDMapEntry::new(unique_name.clone(), true);
        self.identifier_map.insert(name.inner().clone(), map_entry);

        Stat::VarDefinition {
            r#type,
            name: unique_name,
            rvalue: resolved_rvalue,
        }
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
            // Return a dummy value so we can maybe maybe very hopefully
            // allow multiple semantic errors
            Expr::Ident(RenamedName::new_sn_0(ident), r#type)
        }
    }

    // OutputT is a () so return empty value
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

// TODO: maybe have to return the counter i use so i can use it for IR Generation
fn rename(program: UntypedAST) -> Result<(RenamedAST, IdFuncTable), Vec<SemanticError>> {
    let mut context = Renamer::new();
    unimplemented!()
    // // Do stuff
    //
    // // Return the renamed program and the function table
    // Ok((
    //     fold::Folder::fold_program(&mut context, program),
    //     context.id_func_table,
    // ))
}
