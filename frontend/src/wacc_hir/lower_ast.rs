use crate::multi_item::{MultiItem, MultiItemVec};
use crate::parsing::ast;
use crate::source::{SourcedBoxedNode, SourcedNode, SourcedSpan};
use crate::wacc_hir::hir::{
    ArrayElem, ArrayType, BaseType, BinaryOper, Expr, Func, FuncParam, Ident, LValue, Liter,
    PairElem, PairElemSelector, PairElemType, Program, RValue, Stat, StatBlock, Type, UnaryOper,
};
use crate::wacc_hir::AstLoweringError;
use chumsky::container::Container;
use std::collections::HashMap;
use std::mem;
use util::ext::BoxedSliceExt as _;
use util::func::f1::F1OnceExt;
use util::func::f2::F2OnceExt;

use util::nonempty::NonemptyArray;

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
        // cannot curry mutable-references and still have FnMut, RIP :(
        let lowered_funcs = program.funcs.map(|f| self.lower_func(f));
        let lowered_body = self.lower_stat_block(program.body);

        // return lowered program
        Program {
            funcs: lowered_funcs,
            body: lowered_body,
        }
    }

    #[inline]
    pub fn lower_func(&mut self, func: ast::Func) -> Func {
        let ast::Func {
            return_type,
            name,
            params,
            body,
        } = func;
        Func {
            return_type: Self::lower_type_sn(&return_type),
            name,
            // cannot curry mutable-references and still have FnMut, RIP :(
            params: params.map(|p| self.lower_func_param(p)),
            body: self.lower_stat_block(body),
        }
    }

    #[inline]
    pub fn lower_func_param(&mut self, func_param: ast::FuncParam) -> FuncParam {
        let ast::FuncParam { r#type, name } = func_param;

        FuncParam {
            r#type: Self::lower_type_sn(&r#type),
            name: self.lower_ident_sn(name),
        }
    }

    #[allow(clippy::expect_used)]
    pub fn lower_stat_block(&mut self, stat_block: ast::StatBlock) -> StatBlock {
        // accumulate lowered statements into output vector
        let mut lowered_stats: MultiItemVec<SN<Stat>> = MultiItemVec::new();
        for stat_sn in stat_block.0 {
            lowered_stats.push_multi_item(self.lower_stat_sn(stat_sn));
        }

        // there should always be >1 lowered statements by this point
        StatBlock(NonemptyArray::try_from_boxed_slice(lowered_stats).expect(
            "Lowering a `StatBlock` should always leave produce at least one lowered `Stat`",
        ))
    }

    #[allow(clippy::shadow_unrelated)]
    #[allow(clippy::indexing_slicing)]
    pub fn lower_stat_sn(&mut self, stat_sn: SN<ast::Stat>) -> MultiItem<SN<Stat>> {
        // decompose statement, and create curried constructor
        let (stat, span) = stat_sn.into_tuple();
        let sn_new_flipped = (|c, i| SN::new(i, c)).curry();
        let stat_sn = sn_new_flipped(span).chain(MultiItem::Item);

        // match on statement, and produce either one spanned-statement or many of them
        match stat {
            ast::Stat::Skip => stat_sn(Stat::Skip),
            ast::Stat::VarDefinition {
                r#type,
                name,
                rvalue,
            } => {
                let lowered_type = Self::lower_type_sn(&r#type);

                // Sorry about the lots of clones here
                // Evaluate the rhs before creating unique name to not allow int x = x
                // where x is not defined yet
                // resolved_expr returns a new copy of the initializer with any variables renamed
                let lowered_rvalue = self.lower_rvalue_sn(rvalue);

                // Check for duplicate id after resolving rvalue
                // shouldn't panic since we check the key exists
                if self.identifier_map.contains_key(name.inner())
                    && self.identifier_map[name.inner()].from_current_block
                {
                    self.add_error(AstLoweringError::DuplicateIdent(name.clone()));
                    // return dummy Stat
                    return stat_sn(Stat::VarDefinition {
                        r#type: lowered_type,
                        name: Ident::new_rouge_zero_sn(name),
                        rvalue: lowered_rvalue,
                    });
                }

                let unique_name = Ident::new_sn(&mut self.counter, name.clone());
                let map_entry = IDMapEntry::new(unique_name.clone(), true);
                self.identifier_map.insert(name.inner().clone(), map_entry);

                stat_sn(Stat::VarDefinition {
                    r#type: lowered_type,
                    name: unique_name,
                    rvalue: lowered_rvalue,
                })
            }
            ast::Stat::Assignment { lvalue, rvalue } => stat_sn(Stat::Assignment {
                lvalue: self.lower_lvalue_sn(lvalue),
                rvalue: self.lower_rvalue_sn(rvalue),
            }),
            ast::Stat::Read(lv) => stat_sn(Stat::Read(self.lower_lvalue_sn(lv))),
            ast::Stat::Free(e) => stat_sn(Stat::Free(self.lower_expr_sn(e))),
            ast::Stat::Return(e) => stat_sn(Stat::Return(self.lower_expr_sn(e))),
            ast::Stat::Exit(e) => stat_sn(Stat::Exit(self.lower_expr_sn(e))),
            ast::Stat::Print(e) => stat_sn(Stat::Print(self.lower_expr_sn(e))),
            ast::Stat::Println(e) => stat_sn(Stat::Println(self.lower_expr_sn(e))),
            ast::Stat::IfThenElse {
                if_cond,
                then_body,
                else_body,
            } => stat_sn(Stat::IfThenElse {
                if_cond: self.lower_expr_sn(if_cond),
                then_body: self.lower_stat_block(then_body),
                else_body: self.lower_stat_block(else_body),
            }),
            // TODO: refactor this branch once loop-break implemented
            ast::Stat::WhileDo { while_cond, body } => stat_sn(Stat::WhileDo {
                while_cond: self.lower_expr_sn(while_cond),
                body: self.lower_stat_block(body),
            }),
            ast::Stat::Scoped(block) => MultiItem::multi(self.lower_stat_block(block).0),
        }
    }

    pub fn lower_lvalue(&mut self, lvalue: ast::LValue) -> LValue {
        match lvalue {
            ast::LValue::Ident(i) => LValue::Ident(self.lower_ident_sn(i)),
            ast::LValue::ArrayElem(a) => LValue::ArrayElem(self.lower_array_elem_sn(a)),
            ast::LValue::PairElem(p) => LValue::PairElem(self.lower_pair_elem_sn(p)),
        }
    }

    #[inline]
    pub fn lower_lvalue_sn(&mut self, lvalue_sn: SN<ast::LValue>) -> SN<LValue> {
        lvalue_sn.map_inner(Self::lower_lvalue.curry()(self))
    }

    #[inline]
    pub fn lower_lvalue_sbn(&mut self, lvalue_sbn: SBN<ast::LValue>) -> SBN<LValue> {
        lvalue_sbn.map_inner_unboxed(Self::lower_lvalue.curry()(self))
    }

    #[inline]
    pub fn lower_rvalue(&mut self, rvalue: ast::RValue) -> RValue {
        match rvalue {
            ast::RValue::Expr(e) => RValue::Expr(self.lower_expr_sn(e)),
            ast::RValue::ArrayLiter(a) => RValue::ArrayLiter(a.map(|e| self.lower_expr_sn(e))),
            ast::RValue::NewPair(le, re) => {
                RValue::NewPair(self.lower_expr_sn(le), self.lower_expr_sn(re))
            }
            ast::RValue::PairElem(p) => RValue::PairElem(self.lower_pair_elem_sn(p)),
            ast::RValue::Call { func_name, args } => RValue::Call {
                func_name,
                // cannot curry mutable-references and still have FnMut, RIP :(
                args: args.map(|e| self.lower_expr_sn(e)),
            },
        }
    }

    #[inline]
    pub fn lower_rvalue_sn(&mut self, rvalue_sbn: SN<ast::RValue>) -> SN<RValue> {
        rvalue_sbn.map_inner(Self::lower_rvalue.curry()(self))
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

    #[inline]
    pub fn lower_pair_elem(&mut self, pair_elem: ast::PairElem) -> PairElem {
        let ast::PairElem(selector, lvalue) = pair_elem;
        PairElem(
            Self::lower_pair_elem_selector(&selector),
            self.lower_lvalue_sbn(lvalue),
        )
    }

    #[inline]
    pub fn lower_pair_elem_sn(&mut self, pair_elem_sn: SN<ast::PairElem>) -> SN<PairElem> {
        pair_elem_sn.map_inner(Self::lower_pair_elem.curry()(self))
    }

    #[inline]
    pub fn lower_array_elem(&mut self, array_elem: ast::ArrayElem) -> ArrayElem {
        let ast::ArrayElem {
            array_name,
            indices,
        } = array_elem;
        ArrayElem {
            array_name: self.lower_ident_sn(array_name),
            // cannot curry mutable-references and still have FnMut, RIP :(
            indices: indices.map(|e| self.lower_expr_sn(e)),
        }
    }

    #[inline]
    pub fn lower_array_elem_sn(&mut self, array_elem_sn: SN<ast::ArrayElem>) -> SN<ArrayElem> {
        array_elem_sn.map_inner(Self::lower_array_elem.curry()(self))
    }

    #[inline]
    pub fn lower_array_elem_sbn(&mut self, array_elem_sbn: SBN<ast::ArrayElem>) -> SBN<ArrayElem> {
        array_elem_sbn.map_inner_unboxed(Self::lower_array_elem.curry()(self))
    }

    pub fn lower_expr(&mut self, expr: ast::Expr) -> Expr {
        match expr {
            ast::Expr::Liter(l) => Expr::Liter(Self::lower_liter_sn(&l)),
            ast::Expr::Ident(i) => Expr::Ident(self.lower_ident_sn(i)),
            ast::Expr::ArrayElem(a) => Expr::ArrayElem(self.lower_array_elem_sbn(a)),
            ast::Expr::Unary(op, e) => {
                Expr::Unary(Self::lower_unary_oper_sn(&op), self.lower_expr_sbn(e))
            }
            ast::Expr::Binary(le, op, re) => Expr::Binary(
                self.lower_expr_sbn(le),
                Self::lower_binary_oper_sn(&op),
                self.lower_expr_sbn(re),
            ),
            ast::Expr::Paren(e) => self.lower_expr(e.into_inner_unboxed()),
            ast::Expr::IfThenElse {
                if_cond,
                then_val,
                else_val,
            } => Expr::IfThenElse {
                if_cond: self.lower_expr_sbn(if_cond),
                then_val: self.lower_expr_sbn(then_val),
                else_val: self.lower_expr_sbn(else_val),
            },
            ast::Expr::Error(_) => unreachable!(
                "The error-expression {:?} should not be present in the AST past the parsing stage",
                expr
            ),
        }
    }

    #[inline]
    pub fn lower_expr_sn(&mut self, expr_sn: SN<ast::Expr>) -> SN<Expr> {
        expr_sn.map_inner(Self::lower_expr.curry()(self))
    }

    #[inline]
    pub fn lower_expr_sbn(&mut self, expr_sbn: SBN<ast::Expr>) -> SBN<Expr> {
        expr_sbn.map_inner_unboxed(Self::lower_expr.curry()(self))
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
    pub fn lower_liter_sn(liter: &SN<ast::Liter>) -> SN<Liter> {
        liter.transpose_ref().map_inner(Self::lower_liter)
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
    pub fn lower_unary_oper_sn(unary_oper_sn: &SN<ast::UnaryOper>) -> SN<UnaryOper> {
        unary_oper_sn
            .transpose_ref()
            .map_inner(Self::lower_unary_oper)
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

    #[inline]
    pub fn lower_binary_oper_sn(binary_oper_sn: &SN<ast::BinaryOper>) -> SN<BinaryOper> {
        binary_oper_sn
            .transpose_ref()
            .map_inner(Self::lower_binary_oper)
    }

    pub fn lower_type(r#type: &ast::Type) -> Type {
        match *r#type {
            ast::Type::BaseType(ref b) => Type::BaseType(Self::lower_base_type_sn(b)),
            ast::Type::ArrayType(ref a) => Type::ArrayType(Self::lower_array_type_sbn(a)),
            ast::Type::PairType(ref fst, ref snd) => Type::PairType(
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
    pub fn lower_type_sn(type_sn: &SN<ast::Type>) -> SN<Type> {
        type_sn.transpose_ref().map_inner(Self::lower_type)
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

    #[inline]
    pub fn lower_base_type_sn(base_type_sn: &SN<ast::BaseType>) -> SN<BaseType> {
        base_type_sn
            .transpose_ref()
            .map_inner(Self::lower_base_type)
    }

    pub fn lower_array_type(array_type: &ast::ArrayType) -> ArrayType {
        let ast::ArrayType { elem_type } = array_type;
        ArrayType {
            elem_type: Self::lower_type(elem_type),
        }
    }

    #[inline]
    pub fn lower_array_type_sbn(array_type_sbn: &SBN<ast::ArrayType>) -> SBN<ArrayType> {
        array_type_sbn
            .transpose_ref_unboxed()
            .map_inner(Self::lower_array_type)
            .box_inner()
    }

    pub fn lower_pair_elem_type(pair_elem_type: &ast::PairElemType) -> PairElemType {
        match *pair_elem_type {
            ast::PairElemType::ArrayType(ref a) => {
                PairElemType::ArrayType(Self::lower_array_type_sbn(a))
            }
            ast::PairElemType::BaseType(ref b) => {
                PairElemType::BaseType(Self::lower_base_type_sn(b))
            }
            ast::PairElemType::Pair(ref s) => PairElemType::Pair(s.clone()),
        }
    }

    #[allow(clippy::option_if_let_else)]
    #[inline]
    pub fn lower_ident_sn(&mut self, ident: SN<ast::Ident>) -> SN<Ident> {
        if let Some(entry) = self.identifier_map.get(ident.inner()) {
            let renamed_name = &entry.renamed_name;
            renamed_name.clone()
        } else {
            self.add_error(AstLoweringError::UndefinedIdent(ident.clone()));
            // Return a dummy value so we can maybe very hopefully
            // allow multiple semantic errors
            Ident::new_rouge_zero_sn(ident)
        }
    }

    #[inline]
    fn lower_funcname_sn(&mut self, name: SN<ast::Ident>) -> SN<ast::Ident> {
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
