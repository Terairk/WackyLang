#![allow(clippy::arbitrary_source_item_ordering)]
use std::collections::HashMap;

use crate::ast::{Expr, Func, FuncParam, Ident, Program, RValue, Stat, StatBlock};
use crate::fold_program::Folder;
use crate::fold_program::{BoxedSliceFold as _, NonEmptyFold as _};
use crate::source::{SourcedNode, SourcedSpan};
use crate::types::{SemanticType, Type};
use std::fmt;
use std::hash::Hash;
use std::mem;

#[derive(Clone, Hash, PartialEq)]
pub struct RenamedName {
    pub ident: Ident,
    pub uuid: usize,
}

impl fmt::Debug for RenamedName {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}", self.ident, self.uuid)
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
    TypeMismatch(SourcedSpan, SemanticType, SemanticType),
    SimpleTypeMismatch(SemanticType, SemanticType), // TODO: remove this temp error
    MismatchedArgCount(SourcedSpan, usize, usize),
    InvalidIndexType(SourcedSpan, SemanticType),
    InvalidNumberOfIndexes(usize),
    UndefinedIdent(SN<Ident>),
    ReturnInMain,
}

// We handle functions separately from variables since its easier
#[derive(Debug)]
pub struct IdFuncTable {
    functions: HashMap<Ident, (SemanticType, Vec<SemanticType>)>,
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
    pub id_func_table: IdFuncTable,
    pub symbol_table: HashMap<Ident, SemanticType>,
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
            symbol_table: HashMap::new(),
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

    #[inline]
    pub const fn get_symbol_table(&self) -> &HashMap<Ident, SemanticType> {
        &self.symbol_table
    }

    fn copy_id_map_with_false(&self) -> HashMap<Ident, IDMapEntry> {
        self.identifier_map
            .iter()
            .map(|(k, v)| (k.clone(), v.create_false()))
            .collect()
    }

    #[inline]
    pub fn lookup_symbol_table(&self, renamed_name: &SN<RenamedName>) -> SemanticType {
        if let Some(semantic_type) = self.symbol_table.get(&renamed_name.inner().ident) {
            semantic_type.clone()
        } else {
            SemanticType::Error(renamed_name.span())
        }
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
        let folded_block =
            self.with_temporary_map(|slf| block.0.map_with(|stat| slf.fold_stat_sn(stat)));
        StatBlock(folded_block)
    }

    #[inline]
    fn fold_func(&mut self, func: Func<Self::N, Self::T>) -> Func<Self::OutputN, Self::OutputT> {
        self.in_main = false;
        let new_map = self.copy_id_map_with_false();
        let old_id_map = mem::replace(&mut self.identifier_map, new_map);
        let params = func.params.fold_with(|param| self.fold_func_param(param));
        let body = self.with_temporary_map(|slf| slf.fold_stat_block_sn(func.body));
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
    fn fold_name_sn(&mut self, name: SN<Self::N>) -> SN<Self::OutputN> {
        if let Some(entry) = self.identifier_map.get(name.inner()) {
            let renamed_name = &entry.renamed_name;
            renamed_name.clone()
        } else {
            self.add_error(SemanticError::UndefinedIdent(name.clone()));
            // Return a dummy value so we can maybe maybe very hopefully
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
            // Return a dummy value so we can maybe maybe very hopefully
            // allow multiple semantic errors
        }

        name
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
                name: RenamedName::new_sn_0(&name),
                rvalue: resolved_rvalue,
            };
        }

        let unique_name = RenamedName::new_sn(&mut self.counter, name.clone());
        let map_entry = IDMapEntry::new(unique_name.clone(), true);
        self.identifier_map.insert(name.inner().clone(), map_entry);
        self.symbol_table
            .insert(name.inner().clone(), r#type.inner().to_semantic_type());

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
        if self.in_main {
            self.add_error(SemanticError::ReturnInMain);
        }
        Stat::Return(self.fold_expr_sn(expr))
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

        let unique_name = RenamedName::new_sn(&mut self.counter, name.clone());
        let map_entry = IDMapEntry::new(unique_name.clone(), true);
        self.identifier_map.insert(name.inner().clone(), map_entry);
        self.symbol_table
            .insert(name.inner().clone(), param.r#type.to_semantic_type());

        FuncParam {
            r#type: param.r#type,
            name: unique_name,
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
            Expr::Ident(RenamedName::new_sn_0(&ident), r#type)
        }
    }

    // OutputT is a () so return empty value
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
