use crate::parsing::ast;
use crate::parsing::ast_frame::ExprFrame;
use crate::source::{SourcedBoxedNode, SourcedNode, SourcedSpan};
use crate::wacc_hir::hir::{
    ArrayElem, ArrayType, BaseType, BinaryOper, Expr, Func, FuncParam, Ident, LValue, Liter,
    PairElem, PairElemSelector, PairElemType, Program, RValue, Stat, StatBlock, Type, UnaryOper,
};
use crate::wacc_hir::AstLoweringError;
use recursion::CollapsibleExt;
use std::collections::HashMap;
use std::mem;

// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;
type SBN<T> = SourcedBoxedNode<T>;

// We handle functions separately from variables since its easier
#[derive(Debug)]
pub struct IdFuncTable {
    pub functions: HashMap<ast::Ident, (Type, Box<[Type]>)>,
}

// This struct helps us keep track of the identifiers in the current block
// Useful for checking for duplicates with scoping
struct IDMapEntry {
    renamed_name: SN<Ident>,
    from_current_block: bool,
}

impl IDMapEntry {
    const fn new(renamed_name: SN<Ident>, from_current_block: bool) -> Self {
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
pub struct LoweringCtx {
    pub id_func_table: IdFuncTable,
    errors: Vec<AstLoweringError>,
    in_main: bool,
    counter: usize,
    identifier_map: HashMap<ast::Ident, IDMapEntry>,
}

impl LoweringCtx {
    #[inline]
    pub fn new() -> Self {
        Self {
            id_func_table: IdFuncTable {
                functions: HashMap::new(),
            },
            identifier_map: HashMap::new(),
            counter: Ident::ZERO_UUID,
            in_main: true,
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, error: AstLoweringError) {
        self.errors.push(error);
    }

    #[inline]
    pub fn return_errors(&self) -> Vec<AstLoweringError> {
        self.errors.clone()
    }

    #[inline]
    pub const fn get_func_table(&self) -> &IdFuncTable {
        &self.id_func_table
    }

    // This function is used to create a copy of the current identifier map
    // with from_current_block set to false, this is so we can allow shadowing
    // of identifiers provided they're created in a new scope
    #[inline]
    fn copy_id_map_with_false(&self) -> HashMap<ast::Ident, IDMapEntry> {
        self.identifier_map
            .iter()
            .map(|(k, v)| (k.clone(), v.create_false()))
            .collect()
    }

    #[inline]
    pub fn lookup_func_args(&self, ident: &SN<ast::Ident>) -> Box<[Type]> {
        if let Some((_, args)) = self.id_func_table.functions.get(ident) {
            args.clone()
        } else {
            Box::new([])
        }
    }

    #[inline]
    pub fn lookup_func_return_type(&self, ident: &SN<ast::Ident>) -> Result<Type, SourcedSpan> {
        if let Some((return_type, _)) = self.id_func_table.functions.get(ident) {
            Ok(return_type.clone())
        } else {
            Err(ident.span())
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

    // ----

    pub fn lower_program(&mut self, program: ast::Program) -> Program {
        todo!()
    }

    pub fn lower_func(&mut self, func: ast::Func) -> Func {
        todo!()
    }

    pub fn lower_func_param(&mut self, func_param: ast::FuncParam) -> FuncParam {
        todo!()
    }

    pub fn lower_stat_block(&mut self, stat_block: ast::StatBlock) -> StatBlock {
        todo!()
    }

    pub fn lower_stat(&mut self, stat: ast::Stat) -> Stat {
        todo!()
    }

    pub fn lower_lvalue(&mut self, lvalue: ast::LValue) -> LValue {
        todo!()
    }

    pub fn lower_rvalue(&mut self, rvalue: ast::RValue) -> RValue {
        todo!()
    }

    #[inline]
    pub const fn lower_pair_elem_selector(
        pair_elem_selector: &ast::PairElemSelector,
    ) -> PairElemSelector {
        match *pair_elem_selector {
            ast::PairElemSelector::Fst => PairElemSelector::Fst,
            ast::PairElemSelector::Snd => PairElemSelector::Snd,
        }
    }

    pub fn lower_pair_elem(&mut self, pair_elem: ast::PairElem) -> PairElem {
        todo!()
    }

    pub fn lower_array_elem(&mut self, array_elem: ast::ArrayElem) -> ArrayElem {
        todo!()
    }

    pub fn lower_expr(&mut self, expr: &ast::Expr) -> Expr {
        expr.collapse_frames(|frame: ExprFrame<Expr>| match frame {
            ExprFrame::Liter(l) => Expr::Liter(l.map_inner(|l| Self::lower_liter(&l))),
            ExprFrame::ArrayElem(a) => todo!(),
            ExprFrame::Ident(i) => Expr::Ident(self.lower_ident_sn(i)),
            ExprFrame::Unary(op, e) => Expr::Unary(
                op.map_inner(|op| Self::lower_unary_oper(&op)),
                e.box_inner(),
            ),
            ExprFrame::Binary(le, op, re) => Expr::Binary(
                le.box_inner(),
                op.map_inner(|op| Self::lower_binary_oper(&op)),
                re.box_inner(),
            ),
            ExprFrame::Paren(e) => e.into_inner(),
            ExprFrame::IfThenElse {
                if_cond,
                then_val,
                else_val,
            } => Expr::IfThenElse {
                if_cond: if_cond.box_inner(),
                then_val: then_val.box_inner(),
                else_val: else_val.box_inner(),
            },
            ExprFrame::Error(_) => unreachable!(
                "The error-expression {:?} should not be present in the AST past the parsing stage",
                r#frame
            ),
        })
    }

    #[allow(clippy::as_conversions)]
    #[inline]
    pub const fn lower_liter(liter: &ast::Liter) -> Liter {
        match *liter {
            ast::Liter::IntLiter(ref i) => Liter::IntLiter(*i),
            ast::Liter::BoolLiter(ref b) => Liter::BoolLiter(*b),
            ast::Liter::CharLiter(ref c) => Liter::CharLiter(*c as u8),
            ast::Liter::StrLiter(ref s) => Liter::StrLiter(*s),
            ast::Liter::PairLiter => Liter::PairLiter,
        }
    }

    #[inline]
    pub const fn lower_unary_oper(unary_oper: &ast::UnaryOper) -> UnaryOper {
        match *unary_oper {
            ast::UnaryOper::BNot => UnaryOper::BNot,
            ast::UnaryOper::LNot => UnaryOper::LNot,
            ast::UnaryOper::Minus => UnaryOper::Minus,
            ast::UnaryOper::Len => UnaryOper::Len,
            ast::UnaryOper::Ord => UnaryOper::Ord,
            ast::UnaryOper::Chr => UnaryOper::Chr,
        }
    }

    #[inline]
    pub const fn lower_binary_oper(binary_oper: &ast::BinaryOper) -> BinaryOper {
        match *binary_oper {
            ast::BinaryOper::Mul => BinaryOper::Mul,
            ast::BinaryOper::Div => BinaryOper::Div,
            ast::BinaryOper::Mod => BinaryOper::Mod,
            ast::BinaryOper::Add => BinaryOper::Add,
            ast::BinaryOper::Sub => BinaryOper::Sub,
            ast::BinaryOper::Lte => BinaryOper::Lte,
            ast::BinaryOper::Lt => BinaryOper::Lt,
            ast::BinaryOper::Gte => BinaryOper::Gte,
            ast::BinaryOper::Gt => BinaryOper::Gt,
            ast::BinaryOper::Eq => BinaryOper::Eq,
            ast::BinaryOper::Neq => BinaryOper::Neq,
            ast::BinaryOper::BAnd => BinaryOper::BAnd,
            ast::BinaryOper::BXor => BinaryOper::BXor,
            ast::BinaryOper::BOr => BinaryOper::BOr,
            ast::BinaryOper::LAnd => BinaryOper::LAnd,
            ast::BinaryOper::LOr => BinaryOper::LOr,
        }
    }

    pub fn lower_type(r#type: ast::Type) -> Type {
        match r#type {
            ast::Type::BaseType(b) => Type::BaseType(b.map_inner(|b| Self::lower_base_type(&b))),
            ast::Type::ArrayType(a) => Type::ArrayType(a.map_inner_unboxed(Self::lower_array_type)),
            ast::Type::PairType(fst, snd) => Type::PairType(
                Self::lower_pair_elem_type(fst),
                Self::lower_pair_elem_type(snd),
            ),
            ast::Type::Error(_) => unreachable!(
                "The error-type {:?} should not be present in the AST past the parsing stage",
                r#type
            ),
        }
    }

    #[inline]
    pub const fn lower_base_type(base_type: &ast::BaseType) -> BaseType {
        match *base_type {
            ast::BaseType::Int => BaseType::Int,
            ast::BaseType::Bool => BaseType::Bool,
            ast::BaseType::Char => BaseType::Char,
            ast::BaseType::String => BaseType::String,
        }
    }

    pub fn lower_array_type(array_type: ast::ArrayType) -> ArrayType {
        let ast::ArrayType { elem_type } = array_type;
        ArrayType {
            elem_type: Self::lower_type(elem_type),
        }
    }

    pub fn lower_pair_elem_type(pair_elem_type: ast::PairElemType) -> PairElemType {
        match pair_elem_type {
            ast::PairElemType::ArrayType(a) => {
                PairElemType::ArrayType(a.map_inner_unboxed(Self::lower_array_type))
            }
            ast::PairElemType::BaseType(b) => {
                PairElemType::BaseType(b.map_inner(|b| Self::lower_base_type(&b)))
            }
            ast::PairElemType::Pair(s) => PairElemType::Pair(s),
        }
    }

    #[inline]
    pub fn lower_ident_sn(&mut self, ident: SN<ast::Ident>) -> SN<Ident> {
        match self.identifier_map.get(ident.inner()) {
            Some(entry) => {
                let renamed_name = &entry.renamed_name;
                renamed_name.clone()
            }
            None => {
                self.add_error(AstLoweringError::UndefinedIdent(ident.clone()));
                // Return a dummy value so we can maybe very hopefully
                // allow multiple semantic errors
                Ident::new_rouge_sn_zero(ident)
            }
        }
    }

    #[inline]
    fn fold_funcname_sn(&mut self, name: SN<ast::Ident>) -> SN<ast::Ident> {
        // Use ident part of name and then check if it exists in the function table
        // return name regardless but add an error if it doesn't exist
        let ident = name.inner();
        if self.id_func_table.functions.contains_key(ident) {
        } else {
            self.add_error(AstLoweringError::UndefinedIdent(name.clone()));
            // Return a dummy value so we can maybe very hopefully
            // allow multiple semantic errors
        }

        name
    }
}
