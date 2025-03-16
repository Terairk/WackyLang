use crate::SemanticError;
use crate::parsing::ast;
use crate::source::{SourcedBoxedNode, SourcedNode, SourcedSpan};
use crate::wacc_hir::hir::PairElemSelector;
use crate::wacc_hir::lower_ast::HirFuncSymbolTable;
use crate::wacc_hir::{AstLoweringPhaseOutput, hir};
use crate::wacc_thir::optimizations::{tail_recursion_optimization, unreachable_code_elimination};
use crate::wacc_thir::thir::{
    ArrayElem, ArrayLiter, BinaryExpr, Expr, Func, Ident, LValue, Liter, NewPair, PairElem,
    Program, RValue, Stat, StatBlock, UnaryExpr,
};
use crate::wacc_thir::types::{BaseType, PairType, Type};
use ariadne::Span as _;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use util::ext::BoxedSliceExt as _;

// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;
type SBN<T> = SourcedBoxedNode<T>;

#[derive(Debug, Clone)]
pub enum HirLoweringError {
    TypeMismatch {
        span: SourcedSpan,
        actual: Type,
        expected: Type,
    },
    ExpectedArrayType {
        span: SourcedSpan,
        actual: Type,
    },
    ExpectedPairType {
        span: SourcedSpan,
        actual: Type,
    },
    UntypedIdentEncountered(SN<hir::Ident>),
    AssignmentWithBothSidesUnknown(SourcedSpan),
    MismatchedArgCount {
        span: SourcedSpan,
        expected: usize,
        actual: usize,
    },
    InvalidIndexType {
        span: SourcedSpan,
        actual: Type,
    },
    InvalidFreeType {
        span: SourcedSpan,
        actual: Type,
    },
    InvalidNumberOfIndexes {
        span: SourcedSpan,
        max_possible_indices: usize,
        actual_indices: usize,
    },
    FuncNotTailrec(SN<ast::Ident>),
}

impl HirLoweringError {
    #[inline]
    #[must_use]
    pub const fn message_header(&self) -> &'static str {
        match *self {
            // These two get a special header
            Self::TypeMismatch { .. } => "Type Error",
            Self::InvalidNumberOfIndexes { .. } => "Wrong number of indexes",

            // Handle other error variants the same way
            _ => "Semantic Error",
        }
    }

    #[inline]
    #[must_use]
    fn message_body(&self) -> String {
        match *self {
            Self::TypeMismatch {
                ref expected,
                ref actual,
                ..
            } => {
                format!("Expected {}, but got {}", expected, actual)
            }
            Self::ExpectedArrayType { ref actual, .. } => {
                format!(
                    "Expected an array-type, i.e. `t[]` for any `t`, but got {}",
                    actual
                )
            }
            Self::ExpectedPairType { ref actual, .. } => {
                format!(
                    "Expected a pair-type, i.e. `pair(t1, t2)` for any `t1` and `t2`, but got {}",
                    actual,
                )
            }
            Self::UntypedIdentEncountered(ref ident) => {
                format!(
                    "Encountered untyped identifier: {} - THIS MAY BE A BUG IN THE PREVIOUS FRONTEND PHASE!!!!!",
                    ident
                )
            }
            Self::AssignmentWithBothSidesUnknown(_) => "Cannot assign to unknown type".to_string(),
            Self::MismatchedArgCount {
                ref expected,
                ref actual,
                ..
            } => {
                format!("Expected {} arguments, but got {}", expected, actual)
            }
            Self::InvalidIndexType { ref actual, .. } => {
                // TODO: need to make sure SemanticType is a valid WACC type when displaying as a
                // String and not smth we defined
                format!("{} cannot be used to index into an array", actual)
            }
            Self::InvalidFreeType { ref actual, .. } => {
                format!(
                    "Cannot free {} type, can only free an array or a pair",
                    actual
                )
            }
            Self::InvalidNumberOfIndexes {
                ref max_possible_indices,
                ref actual_indices,
                ..
            } => {
                format!(
                    "Expected maximum {} index(es), but got {}",
                    max_possible_indices, actual_indices
                )
            }
            Self::FuncNotTailrec(ref func_name) => format!(
                "Function `{}` was marked as tail-recursive but isn't",
                func_name
            ),
        }
    }

    #[inline]
    fn into_span(self) -> SourcedSpan {
        match self {
            Self::UntypedIdentEncountered(ident) => ident.span(),
            Self::FuncNotTailrec(ident) => ident.span(),
            Self::TypeMismatch { span, .. }
            | Self::ExpectedArrayType { span, .. }
            | Self::ExpectedPairType { span, .. }
            | Self::AssignmentWithBothSidesUnknown(span)
            | Self::MismatchedArgCount { span, .. }
            | Self::InvalidIndexType { span, .. }
            | Self::InvalidFreeType { span, .. }
            | Self::InvalidNumberOfIndexes { span, .. } => span,
        }
    }

    #[inline]
    fn into_semantic_error(self) -> SemanticError<&'static str, String> {
        SemanticError {
            message_header: self.message_header(),
            message_body: self.message_body(),
            span: self.into_span(),
        }
    }
}

impl From<HirLoweringError> for SemanticError<&'static str, String> {
    #[inline]
    fn from(value: HirLoweringError) -> Self {
        value.into_semantic_error()
    }
}

// We handle functions separately from variables since its easier
#[derive(Debug)]
pub struct ThirFuncSymbolTable {
    pub functions: HashMap<ast::Ident, (Type, Box<[Type]>)>,
}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct IdentSymbolTable(pub HashMap<hir::Ident, Type>);

impl Deref for IdentSymbolTable {
    type Target = HashMap<hir::Ident, Type>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for IdentSymbolTable {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug)]
pub struct HirLoweringResult {
    pub output: Program,
    pub errors: Vec<HirLoweringError>,
    pub func_symbol_table: ThirFuncSymbolTable,
    pub hir_ident_symbol_table: IdentSymbolTable,
    pub ident_counter: usize,
}

struct HirLoweringCtx {
    errors: Vec<HirLoweringError>,
    func_symbol_table: HirFuncSymbolTable,
    current_func_hir_return_type: Option<hir::Type>,
    hir_ident_symbol_table: IdentSymbolTable,
    ident_counter: usize,
    should_tailrec_optimize: bool,
}

impl HirLoweringCtx {
    const TAILREC_LOOP: &'static str = "outermost_tailrec_loop";

    fn new(
        func_symbol_table: HirFuncSymbolTable,
        ident_counter: usize,
        should_tailrec_optimize: bool,
    ) -> Self {
        Self {
            errors: Vec::new(),
            func_symbol_table,
            current_func_hir_return_type: None,
            hir_ident_symbol_table: IdentSymbolTable(HashMap::new()),
            ident_counter,
            should_tailrec_optimize,
        }
    }

    fn add_error(&mut self, error: HirLoweringError) {
        self.errors.push(error);
    }

    fn pair_inner_erasable(from: &Type, to: &Type) -> bool {
        if let (Type::PairType(_), Type::PairType(_)) = (from, to) {
            return to == &Type::erased_pair() || from == &Type::erased_pair() || from == to;
        }
        from == to
    }

    #[must_use]
    #[inline]
    pub fn can_coerce_into(from: &Type, to: &Type) -> bool {
        if to == &Type::STRING {
            if let Type::ArrayType(boxed) = from {
                return boxed.elem_type == Type::CHAR;
            }
        }

        match (from, to) {
            (Type::Any, _) => true,
            (_, Type::Any) => true,
            (Type::BaseType(a), Type::BaseType(b)) => a == b,
            (Type::ArrayType(a), Type::ArrayType(b)) => {
                a.elem_type == b.elem_type || // arrays are invariant

                    Self::pair_inner_erasable(&a.elem_type, &b.elem_type) ||
                    a.elem_type == Type::Any
            }
            (Type::PairType(boxed_from), Type::PairType(boxed_to)) => {
                let (a1, b1) = (&boxed_from.fst_type, &boxed_from.snd_type);
                let (a2, b2) = (&boxed_to.fst_type, &boxed_to.snd_type);

                (a1 == a2 && b1 == b2) || // pairs are invariant
                    (Self::pair_inner_erasable(a1, a2) && Self::pair_inner_erasable(b1, b2))
                    || from == &Type::erased_pair()
                    || to == &Type::erased_pair()
            }
            _ => false,
        }
    }

    #[inline]
    fn try_coerce_into(&mut self, from: RValue, to: Type) -> Result<RValue, (RValue, Type)> {
        // grab resolved type, and make sure this is a legal coersion
        let resolved_type = from.r#type();

        // if special case of arrays, make sure inner elements are coercible
        // if let (Type::ArrayType(a), Type::BaseType(b)) = (resolved_type, to) {}elements

        if !Self::can_coerce_into(&resolved_type, &to) {
            return Err((from, resolved_type));
        }

        // make sure we are not coercing to `AnyType` as this looses type information
        if to == Type::Any {
            return Ok(from);
        }

        Ok(match from {
            RValue::Expr(expr) => RValue::Expr(expr.map_type(|_| to)),
            RValue::ArrayLiter(ArrayLiter { liter_values, .. }) => RValue::ArrayLiter(ArrayLiter {
                liter_values,
                r#type: to,
            }),
            RValue::NewPair(NewPair { fst, snd, .. }) => RValue::NewPair(NewPair {
                fst,
                snd,
                r#type: to,
            }),
            RValue::PairElem(pair_elem) => {
                RValue::PairElem(self.refine_pair_elem_type(pair_elem, to))
            }
            RValue::Call {
                func_name,
                args,
                return_type: _return_type,
            } => RValue::Call {
                func_name,
                args,
                return_type: to,
            },
        })
    }

    fn refine_lvalue_type(&mut self, lvalue: LValue, refinement_ty: Type) -> LValue {
        // make sure we are not coercing to `AnyType` as this looses type information
        if refinement_ty == Type::Any {
            return lvalue;
        };

        match lvalue {
            LValue::Ident(Ident { ident, .. }) => LValue::Ident(Ident {
                ident,
                r#type: refinement_ty,
            }),
            LValue::ArrayElem(a) => LValue::ArrayElem(a.map_type(|_| refinement_ty)),
            LValue::PairElem(p) => LValue::PairElem(self.refine_pair_elem_type(p, refinement_ty)),
        }
    }

    fn refine_pair_elem_type(&mut self, pair_elem: PairElem, refinement_ty: Type) -> PairElem {
        // make sure we are not coercing to `AnyType` as this looses type information
        if refinement_ty == Type::Any {
            return pair_elem;
        };

        let (selector, lvalue) = pair_elem.pair_elem;
        let (fst_ty, snd_ty) = match &selector {
            PairElemSelector::Fst => {
                // refinement type becomes FST-type
                let fst_ty = refinement_ty.clone();

                // if there is any type in lvalue for SND-type, use it, or use any as fallback
                let snd_ty = match lvalue.r#type() {
                    Type::PairType(boxed) => {
                        let PairType { snd_type, .. } = *boxed;
                        snd_type
                    }
                    _ => Type::Any,
                };
                (fst_ty, snd_ty)
            }
            PairElemSelector::Snd => {
                // refinement type becomes SND-type
                let snd_ty = refinement_ty.clone();

                // if there is any type in lvalue for FST-type, use it, or use any as fallback
                let fst_ty = match lvalue.r#type() {
                    Type::PairType(boxed) => {
                        let PairType { fst_type, .. } = *boxed;
                        fst_type
                    }
                    _ => Type::Any,
                };
                (fst_ty, snd_ty)
            }
        };

        // reconstruct inner type, and use it as a refinement type for lvalue
        let nested_refinement_ty = Type::pair_type(fst_ty, snd_ty);
        let refined_lvalue = Box::new(self.refine_lvalue_type(*lvalue, nested_refinement_ty));

        // reconstruct pair type
        PairElem {
            pair_elem: (selector, refined_lvalue),
            r#type: refinement_ty,
        }
    }

    #[inline]
    pub fn make_new_ident(&mut self, ident: ast::Ident, r#type: Type) -> Ident {
        let ident = hir::Ident::new(&mut self.ident_counter, ident);
        self.hir_ident_symbol_table
            .insert(ident.clone(), r#type.clone());
        Ident { ident, r#type }
    }

    #[inline]
    pub const fn make_new_loop_label(&mut self, ident: ast::Ident) -> hir::LoopLabel {
        hir::LoopLabel::new(&mut self.ident_counter, ident)
    }

    // ----

    pub fn lower_program(&mut self, program: hir::Program) -> Program {
        let folded_funcs = program.funcs.map(|func| self.lower_func(func));
        let folded_body = self.lower_stat_block(program.body);

        Program {
            funcs: folded_funcs,
            body: folded_body,
        }
    }

    #[allow(clippy::let_and_return)]
    #[inline]
    pub fn lower_func(&mut self, func: hir::Func) -> Func {
        // lower function to unoptimized
        self.current_func_hir_return_type = Some(func.return_type.inner().clone());
        let unoptimized = Func {
            return_type: Self::lower_type(&func.return_type), // Type remains unchanged
            name: func.name.clone().into_inner(),
            params: func.params.map(|param| self.lower_func_param(param)),
            body: self.lower_stat_block(func.body),
        };
        self.current_func_hir_return_type = None;

        // perform tail-call optimization. if function is `tailrec` and the optimization failed,
        // raise an error about it
        if !self.should_tailrec_optimize {
            return unoptimized;
        }
        let optimized = match (
            tail_recursion_optimization(
                self.make_new_loop_label(ast::Ident::from_str(Self::TAILREC_LOOP)),
                &mut |ident, r#type| self.make_new_ident(ast::Ident::from_str(ident), r#type),
                unoptimized,
            ),
            func.is_tailrec,
        ) {
            (Ok(o) | Err(o), false) | (Ok(o), true) => o,
            (Err(o), true) => {
                self.add_error(HirLoweringError::FuncNotTailrec(func.name));
                o
            }
        };
        optimized
        // unoptimized
    }

    #[allow(clippy::indexing_slicing)]
    #[inline]
    pub fn lower_func_param(&mut self, param: hir::FuncParam) -> Ident {
        let lowered_type = Self::lower_type(&param.r#type);
        self.hir_ident_symbol_table
            .insert(param.name.inner().clone(), lowered_type.clone());
        Ident {
            ident: param.name.into_inner(),
            r#type: lowered_type,
        }
    }

    #[allow(clippy::expect_used, clippy::let_and_return)]
    #[inline]
    pub fn lower_stat_block(&mut self, stat_block: hir::StatBlock) -> StatBlock {
        let unoptimized = StatBlock(stat_block.0.map(|stat| self.lower_stat(stat.into_inner())));

        // apply optimizations
        let optimized = unreachable_code_elimination(unoptimized, self.should_tailrec_optimize);
        optimized
    }

    #[allow(clippy::too_many_lines)]
    pub fn lower_stat(&mut self, stat: hir::Stat) -> Stat {
        match stat {
            hir::Stat::Skip => Stat::Skip,
            hir::Stat::VarDefinition {
                r#type,
                name,
                rvalue,
            } => {
                let expected_type = Self::lower_type_sn(&r#type);
                let resolved_rvalue = self.lower_rvalue_sn(rvalue.clone());

                // try to coerce the resolved rvalue, or add error
                let resolved_rvalue =
                    match self.try_coerce_into(resolved_rvalue, expected_type.clone()) {
                        Ok(new) => new,
                        Err((old, resolved_type)) => {
                            self.add_error(HirLoweringError::TypeMismatch {
                                span: rvalue.span(),
                                actual: resolved_type,
                                expected: expected_type.clone(),
                            });
                            old
                        }
                    };

                // insert into symbol table and return folded variable definition
                self.hir_ident_symbol_table
                    .insert(name.inner().clone(), expected_type.clone());
                let name = Ident {
                    ident: name.into_inner(),
                    r#type: expected_type,
                };
                Stat::VarDefinition {
                    name,
                    rvalue: resolved_rvalue,
                }
            }
            hir::Stat::Assignment { lvalue, rvalue } => {
                let mut resolved_lvalue = self.lower_lvalue_sn(lvalue.clone());
                let mut resolved_rvalue = self.lower_rvalue_sn(rvalue.clone());
                let resolved_lval_type = resolved_lvalue.r#type();
                let resolved_rval_type = resolved_rvalue.r#type();
                if resolved_lval_type == Type::Any && resolved_rval_type == Type::Any {
                    // check for double-Any errors
                    self.add_error(HirLoweringError::AssignmentWithBothSidesUnknown(
                        lvalue
                            .span()
                            .map_span(|lspan| (lspan.start..rvalue.span().end()).into()),
                    ));
                } else {
                    // try to coerce, or fail and emmit error
                    resolved_rvalue =
                        match self.try_coerce_into(resolved_rvalue, resolved_lval_type.clone()) {
                            Ok(new) => {
                                // if coersion was successful, refine the LHS aswell. This is useful to
                                // reclaim useful type information for cases of both-directional nested lhs-lhs
                                // e.g. `fst snd ident = snd arr[3]`, we need to know the type of each stage of that
                                let resolved_rval_type = new.r#type();
                                resolved_lvalue =
                                    self.refine_lvalue_type(resolved_lvalue, resolved_rval_type);

                                // return new type
                                new
                            }
                            Err((old, resolved_rval_type)) => {
                                self.add_error(HirLoweringError::TypeMismatch {
                                    span: rvalue.span(),
                                    actual: resolved_rval_type,
                                    expected: resolved_lval_type,
                                });
                                old
                            }
                        };
                }
                Stat::Assignment {
                    lvalue: resolved_lvalue,
                    rvalue: resolved_rvalue,
                }
            }
            hir::Stat::Read(lvalue) => {
                let resolved_lvalue = self.lower_lvalue_sn(lvalue.clone());
                match resolved_lvalue.r#type() {
                    Type::BaseType(BaseType::Int | BaseType::Char) => (),
                    _ => self.add_error(HirLoweringError::TypeMismatch {
                        span: lvalue.span(),
                        actual: resolved_lvalue.r#type(),
                        expected: Type::INT,
                    }),
                }
                Stat::Read(resolved_lvalue)
            }
            hir::Stat::Free(expr) => {
                let resolved_expr = self.lower_expr_sn(expr.clone());
                match resolved_expr.r#type() {
                    Type::ArrayType(_) | Type::PairType(_) => (),
                    _ => self.add_error(HirLoweringError::InvalidFreeType {
                        span: expr.span(),
                        actual: resolved_expr.r#type(),
                    }),
                }
                Stat::Free(resolved_expr)
            }
            hir::Stat::Return(expr) => {
                let resolved_expr = self.lower_expr_sn(expr.clone());
                if let Some(expected_ret_type) = self.current_func_hir_return_type.as_ref() {
                    let resolved_ret_value = resolved_expr.r#type();
                    let expected_ret_type = Self::lower_type(expected_ret_type);
                    if !Self::can_coerce_into(&resolved_ret_value, &expected_ret_type) {
                        self.add_error(HirLoweringError::TypeMismatch {
                            span: expr.span(),
                            actual: expected_ret_type,
                            expected: resolved_ret_value,
                        });
                    }
                } // otherwise we're in the main body, where the renaming stage will have emitted an error if there's a return statement

                Stat::Return(resolved_expr)
            }
            hir::Stat::Exit(expr) => {
                let resolved_expr = self.lower_expr_sn(expr.clone());
                if resolved_expr.r#type() != Type::INT {
                    self.add_error(HirLoweringError::TypeMismatch {
                        span: expr.span(),
                        actual: resolved_expr.r#type(),
                        expected: Type::INT,
                    });
                }
                Stat::Exit(resolved_expr)
            }
            hir::Stat::Print(expr) => Stat::Print(self.lower_expr_sn(expr)),
            hir::Stat::Println(expr) => Stat::Println(self.lower_expr_sn(expr)),
            hir::Stat::IfThenElse {
                if_cond,
                then_body,
                else_body,
            } => {
                let resolved_if_cond = self.lower_expr_sn(if_cond.clone());
                if resolved_if_cond.r#type() != Type::BOOL {
                    self.add_error(HirLoweringError::TypeMismatch {
                        span: if_cond.span(),
                        actual: resolved_if_cond.r#type(),
                        expected: Type::BOOL,
                    });
                }
                Stat::IfThenElse {
                    if_cond: resolved_if_cond,
                    then_body: self.lower_stat_block(then_body),
                    else_body: self.lower_stat_block(else_body),
                }
            }
            hir::Stat::LoopDo { label, body } => Stat::LoopDo {
                label,
                body: self.lower_stat_block(body),
            },
            hir::Stat::Break(label) => Stat::Break(label),
            hir::Stat::NextLoop(label) => Stat::NextLoop(label),
        }
    }

    #[inline]
    pub fn lower_lvalue(&mut self, lvalue: hir::LValue) -> LValue {
        match lvalue {
            hir::LValue::Ident(i) => LValue::Ident(self.lookup_ident_sn(i)),
            hir::LValue::ArrayElem(a) => LValue::ArrayElem(self.lower_array_elem_sn(a)),
            hir::LValue::PairElem(p) => LValue::PairElem(self.lower_pair_elem_sn(p)),
        }
    }

    #[inline]
    pub fn lower_lvalue_sn(&mut self, lvalue_sn: SN<hir::LValue>) -> LValue {
        self.lower_lvalue(lvalue_sn.into_inner())
    }

    #[inline]
    pub fn lower_lvalue_sbn(&mut self, lvalue_sbn: SBN<hir::LValue>) -> LValue {
        self.lower_lvalue(lvalue_sbn.into_inner_unboxed())
    }

    #[inline]
    pub fn lower_rvalue(&mut self, rvalue: hir::RValue) -> RValue {
        match rvalue {
            hir::RValue::Expr(e) => RValue::Expr(self.lower_expr_sn(e)),
            hir::RValue::ArrayLiter(exprs) => {
                let resolved_exprs = exprs.clone().map(|expr| self.lower_expr_sn(expr));
                if resolved_exprs.is_empty() {
                    return RValue::ArrayLiter(ArrayLiter {
                        liter_values: resolved_exprs,
                        r#type: Type::unknown_array(),
                    });
                }
                let mut resolved_type = resolved_exprs.clone()[0].r#type();
                for (i, expr) in resolved_exprs.clone().into_iter().enumerate() {
                    let curr_expr_type = expr.r#type();
                    if curr_expr_type != resolved_type
                        && !Self::can_coerce_into(&curr_expr_type, &resolved_type)
                    {
                        if Self::can_coerce_into(&resolved_type, &curr_expr_type) {
                            resolved_type = curr_expr_type;
                        } else {
                            let expr_sn = &exprs[i];
                            self.add_error(HirLoweringError::TypeMismatch {
                                span: expr_sn.span(),
                                actual: expr.r#type(),
                                expected: resolved_type.clone(),
                            });
                        }
                    }
                }
                RValue::ArrayLiter(ArrayLiter {
                    liter_values: resolved_exprs,
                    r#type: Type::array_type(resolved_type),
                })
            }
            hir::RValue::NewPair(fst, snd) => {
                let resolved_fst = self.lower_expr_sn(fst);
                let resolved_snd = self.lower_expr_sn(snd);
                let resolved_type = Type::pair_type(resolved_fst.r#type(), resolved_snd.r#type());

                RValue::NewPair(NewPair {
                    fst: resolved_fst,
                    snd: resolved_snd,
                    r#type: resolved_type,
                })
            }
            hir::RValue::PairElem(pair_elem) => {
                RValue::PairElem(self.lower_pair_elem_sn(pair_elem))
            }
            hir::RValue::Call { func_name, args } => {
                let resolved_return_type = self.func_symbol_table.lookup_func_return_type(&func_name).expect(format!("the function call to `{:?}` should always resolve, if the prior HIR stage is correct", &func_name).as_str());
                let expected_args_types =
                    self.func_symbol_table.lookup_func_args(&func_name).expect(
                        "the function parameters should always resolve if the prior HIR is correct",
                    );
                let resolved_args = args.clone().map(|arg| self.lower_expr_sn(arg));
                if expected_args_types.len() != resolved_args.len() {
                    self.add_error(HirLoweringError::MismatchedArgCount {
                        span: func_name.span(),
                        expected: expected_args_types.len(),
                        actual: resolved_args.len(),
                    });
                }
                for ((i, arg), expected_type) in resolved_args
                    .clone()
                    .iter()
                    .enumerate()
                    .zip(expected_args_types)
                {
                    let expected_type = Self::lower_type(&expected_type);
                    if !Self::can_coerce_into(&arg.r#type(), &expected_type) {
                        let arg_sn = &args[i];
                        self.add_error(HirLoweringError::TypeMismatch {
                            span: arg_sn.span(),
                            actual: arg.r#type(),
                            expected: expected_type,
                        });
                    }
                }
                RValue::Call {
                    func_name: func_name.into_inner(),
                    args: resolved_args,
                    return_type: Self::lower_type(&resolved_return_type),
                }
            }
        }
    }

    #[inline]
    pub fn lower_rvalue_sn(&mut self, rvalue_sn: SN<hir::RValue>) -> RValue {
        self.lower_rvalue(rvalue_sn.into_inner())
    }

    #[inline]
    pub fn lower_pair_elem(&mut self, pair_elem: hir::PairElem) -> PairElem {
        enum QuasiPairType {
            Pair(PairType),
            Any,
        }

        let hir::PairElem(selector, lvalue) = pair_elem;

        // get value and error if not pair
        let lval = self.lower_lvalue_sbn(lvalue.clone());
        let pair_type = match lval.r#type() {
            Type::PairType(boxed) => QuasiPairType::Pair(*boxed),
            Type::Any => QuasiPairType::Any,
            _ => {
                self.add_error(HirLoweringError::ExpectedPairType {
                    span: lvalue.span(),
                    actual: lval.r#type(),
                });
                QuasiPairType::Any
            }
        };

        // extract pair type
        let extracted_type = match (&selector, pair_type) {
            (_, QuasiPairType::Any) => Type::Any,
            (&PairElemSelector::Fst, QuasiPairType::Pair(PairType { fst_type, .. })) => fst_type,
            (&PairElemSelector::Snd, QuasiPairType::Pair(PairType { snd_type, .. })) => snd_type,
        };

        // build pair elem
        PairElem {
            pair_elem: (selector, Box::new(lval)),
            r#type: extracted_type,
        }
    }

    #[inline]
    pub fn lower_pair_elem_sn(&mut self, pair_elem_sn: SN<hir::PairElem>) -> PairElem {
        self.lower_pair_elem(pair_elem_sn.into_inner())
    }

    #[inline]
    pub fn lower_array_elem(&mut self, array_elem: hir::ArrayElem) -> ArrayElem {
        let hir::ArrayElem {
            array_name: array_name_sn,
            indices: indices_sn,
        } = array_elem;

        // look up identifier and its type
        let array_name = self.lookup_ident_sn(array_name_sn.clone());

        // resolve indices, ensuring they are of the `int` type
        let indices = indices_sn.clone().map(|i| {
            // get resolved expression
            let resolved_i = self.lower_expr_sn(i.clone());
            let resolved_i_type = resolved_i.r#type();

            // ensure that type is `int`, or error
            // TODO: check if this needs type casting or not
            if resolved_i_type != Type::BaseType(BaseType::Int) {
                self.add_error(HirLoweringError::InvalidIndexType {
                    span: i.span(),
                    actual: resolved_i_type,
                });
            }

            resolved_i
        });

        // unwind the indices of array to ensure that the array has the right number of indices
        let actual_indices = indices.len();
        let mut indices_iter = indices.into_iter().enumerate();
        let (_, first_index) = indices_iter
            .next()
            .expect("The first index value should always be there");
        let mut curr_type = match array_name.clone().r#type {
            Type::ArrayType(boxed) => boxed.elem_type,
            Type::Any => Type::Any,
            other => {
                self.add_error(HirLoweringError::ExpectedArrayType {
                    span: array_name_sn.span(),
                    actual: other.clone(),
                });
                other
            }
        };
        let mut curr_array_elem = ArrayElem::FirstAccess {
            array_name,
            index: first_index,
            r#type: curr_type.clone(),
        };
        let mut max_possible_indices = 1;

        for (i, index) in indices_iter {
            match curr_type {
                Type::ArrayType(boxed) => {
                    // update type, extend number of indices
                    curr_type = boxed.elem_type;
                    max_possible_indices += 1;

                    // update array elem value
                    curr_array_elem = ArrayElem::NestedAccess {
                        array_elem: Box::new(curr_array_elem),
                        index,
                        r#type: curr_type.clone(),
                    }
                }
                Type::Any => {
                    // update array elem value
                    curr_array_elem = ArrayElem::NestedAccess {
                        array_elem: Box::new(curr_array_elem),
                        index,
                        r#type: Type::Any,
                    }
                }
                _ => {
                    self.add_error(HirLoweringError::InvalidNumberOfIndexes {
                        span: indices_sn[i].span(),
                        max_possible_indices,
                        actual_indices,
                    });
                    // update array elem value
                    curr_array_elem = ArrayElem::NestedAccess {
                        array_elem: Box::new(curr_array_elem),
                        index,
                        r#type: Type::Any,
                    }
                }
            }
        }

        // return resolved array_elem
        curr_array_elem
    }

    #[inline]
    pub fn lower_array_elem_sn(&mut self, array_elem_sn: SN<hir::ArrayElem>) -> ArrayElem {
        self.lower_array_elem(array_elem_sn.into_inner())
    }

    #[inline]
    pub fn lower_array_elem_sbn(&mut self, array_elem_sbn: SBN<hir::ArrayElem>) -> ArrayElem {
        self.lower_array_elem(array_elem_sbn.into_inner_unboxed())
    }

    #[allow(clippy::too_many_lines)]
    pub fn lower_expr(&mut self, expr: hir::Expr) -> Expr {
        match expr {
            hir::Expr::Liter(l) => Expr::Liter(Self::lower_liter_sn(l)),
            hir::Expr::Ident(i) => Expr::Ident(self.lookup_ident_sn(i)),
            hir::Expr::ArrayElem(a) => Expr::ArrayElem(Box::new(self.lower_array_elem_sbn(a))),
            hir::Expr::Unary(op, e) => {
                let resolved_expr = self.lower_expr_sbn(e.clone());
                let mut unary_expect = |a, b| self.unary_expect(e.span(), &resolved_expr, a, b);
                let resolved_type = match *op.inner() {
                    hir::UnaryOper::LNot => unary_expect(Type::BOOL, Type::BOOL),
                    hir::UnaryOper::Minus => unary_expect(Type::INT, Type::INT),
                    hir::UnaryOper::Len => {
                        if let Type::ArrayType(_) = resolved_expr.r#type() {
                            Type::INT
                        } else {
                            self.add_error(HirLoweringError::TypeMismatch {
                                span: e.span(),
                                actual: resolved_expr.r#type(),
                                expected: Type::BOOL,
                            });
                            Type::Any
                        }
                    }
                    hir::UnaryOper::Ord => unary_expect(Type::CHAR, Type::INT),
                    hir::UnaryOper::Chr => unary_expect(Type::INT, Type::CHAR),
                };

                // build final unary-expression
                Expr::Unary(Box::new(UnaryExpr {
                    expr: (op.into_inner(), resolved_expr),
                    r#type: resolved_type,
                }))
            }
            hir::Expr::Binary(lhs, op, rhs) => {
                let resolved_lhs = self.lower_expr_sbn(lhs.clone());
                let resolved_rhs = self.lower_expr_sbn(rhs.clone());
                let mut binary_expect = |a, b| {
                    self.binary_expect(
                        (resolved_lhs.clone(), lhs.span()),
                        (resolved_rhs.clone(), rhs.span()),
                        a,
                        b,
                    )
                };

                let resolved_type = match *op.inner() {
                    // Operations that expect (Int, Int) -> Int
                    hir::BinaryOper::Mul
                    | hir::BinaryOper::Div
                    | hir::BinaryOper::Mod
                    | hir::BinaryOper::Add
                    | hir::BinaryOper::Sub => binary_expect(Type::INT, Type::INT),

                    // Comparison ops (Int, Int) -> Bool or (char, char) -> Bool
                    hir::BinaryOper::Lte
                    | hir::BinaryOper::Lt
                    | hir::BinaryOper::Gte
                    | hir::BinaryOper::Gt => {
                        let resolved_lhs_type = resolved_lhs.r#type();
                        match resolved_lhs_type {
                            // if integer or character type, use it
                            Type::BaseType(BaseType::Int | BaseType::Char) => {
                                binary_expect(resolved_lhs_type, Type::BOOL)
                            }

                            // otherwise use integer as fallback for typechecking
                            _ => binary_expect(Type::INT, Type::BOOL),
                        }
                    }
                    // Equality ops: (T, T) -> Bool (allow any matching type)
                    hir::BinaryOper::Eq | hir::BinaryOper::Neq => {
                        let lhs_type = resolved_lhs.r#type();
                        let rhs_type = resolved_rhs.r#type();
                        if Self::can_coerce_into(&lhs_type, &rhs_type)
                            || Self::can_coerce_into(&rhs_type, &lhs_type)
                        {
                            Type::BOOL
                        } else {
                            self.add_error(HirLoweringError::TypeMismatch {
                                span: lhs.span(),
                                actual: resolved_rhs.r#type(),
                                expected: resolved_lhs.r#type(),
                            });
                            Type::Any
                        }
                    }
                    // Logical ops (Bool, Bool) -> Bool
                    hir::BinaryOper::LAnd | hir::BinaryOper::LOr => {
                        binary_expect(Type::BOOL, Type::BOOL)
                    }
                };

                // construct binary expression
                Expr::Binary(Box::new(BinaryExpr {
                    expr: (resolved_lhs, op.into_inner(), resolved_rhs),
                    r#type: resolved_type,
                }))
            }
            hir::Expr::IfThenElse {
                if_cond,
                then_val,
                else_val,
            } => {
                // enforce that the if-cond is of an appropriate tye
                let resolved_if_cond = self.lower_expr_sbn(if_cond.clone());
                if resolved_if_cond.r#type() != Type::BOOL {
                    self.add_error(HirLoweringError::TypeMismatch {
                        span: if_cond.span(),
                        actual: Type::BOOL,
                        expected: resolved_if_cond.r#type(),
                    });
                }

                // resolve then/else values and types
                let resolved_then_val = self.lower_expr_sbn(then_val.clone());
                let resolved_then_val_type = resolved_then_val.r#type();
                let resolved_else_val = self.lower_expr_sbn(else_val.clone());
                let resolved_else_val_type = resolved_else_val.r#type();

                // Do not coerce into any-type if can be avoided
                let bad_coerce_target = |t: &Type| t == &Type::Any;

                // try to coerce then-to-else
                if !bad_coerce_target(&resolved_else_val_type)
                    && Self::can_coerce_into(&resolved_then_val_type, &resolved_else_val_type)
                {
                    Expr::IfThenElse {
                        if_cond: Box::new(resolved_if_cond),
                        then_val: Box::new(resolved_then_val),
                        else_val: Box::new(resolved_else_val),
                        r#type: resolved_else_val_type,
                    }
                }
                // try to coerce else-to-then, even if it might be a bad target
                else if Self::can_coerce_into(&resolved_else_val_type, &resolved_then_val_type) {
                    Expr::IfThenElse {
                        if_cond: Box::new(resolved_if_cond),
                        then_val: Box::new(resolved_then_val),
                        else_val: Box::new(resolved_else_val),
                        r#type: resolved_then_val_type,
                    }
                }
                // upon reaching incompatible types, emmit a type-mismatch error
                else {
                    // if we tried coercing then-to-else and failed, add error to then-val
                    if !bad_coerce_target(&resolved_else_val_type) {
                        self.add_error(HirLoweringError::TypeMismatch {
                            span: then_val.span(),
                            actual: resolved_then_val_type,
                            expected: resolved_else_val_type,
                        });
                        Expr::IfThenElse {
                            if_cond: Box::new(resolved_if_cond),
                            then_val: Box::new(resolved_then_val),
                            else_val: Box::new(resolved_else_val),
                            r#type: Type::Any,
                        }
                    }
                    // otherwise if we tried coercing else-to-then, add report to else-val
                    else {
                        self.add_error(HirLoweringError::TypeMismatch {
                            span: else_val.span(),
                            actual: resolved_else_val_type,
                            expected: resolved_then_val_type,
                        });
                        Expr::IfThenElse {
                            if_cond: Box::new(resolved_if_cond),
                            then_val: Box::new(resolved_then_val),
                            else_val: Box::new(resolved_else_val),
                            r#type: Type::Any,
                        }
                    }
                }
            }
        }
    }

    #[inline]
    pub fn lower_expr_sn(&mut self, expr_sn: SN<hir::Expr>) -> Expr {
        self.lower_expr(expr_sn.into_inner())
    }

    #[inline]
    pub fn lower_expr_sbn(&mut self, expr_sbn: SBN<hir::Expr>) -> Expr {
        self.lower_expr(expr_sbn.into_inner_unboxed())
    }

    #[allow(clippy::as_conversions)]
    #[inline]
    pub fn lower_liter(liter: hir::Liter) -> Liter {
        // get type by pattern matching
        let r#type = match liter {
            hir::Liter::IntLiter(_) => Type::BaseType(BaseType::Int),
            hir::Liter::BoolLiter(_) => Type::BaseType(BaseType::Bool),
            hir::Liter::CharLiter(_) => Type::BaseType(BaseType::Char),
            hir::Liter::StrLiter(_) => Type::BaseType(BaseType::String),
            hir::Liter::PairLiter => Type::erased_pair(),
        };

        Liter { liter, r#type }
    }

    #[inline]
    pub fn lower_liter_sn(liter: SN<hir::Liter>) -> Liter {
        Self::lower_liter(liter.into_inner())
    }

    #[inline]
    fn unary_expect(
        &mut self,
        hir_span_source: SourcedSpan,
        expr: &Expr,
        expected_type: Type,
        result_type: Type,
    ) -> Type {
        let expr_type = expr.r#type();
        if Self::can_coerce_into(&expr_type, &expected_type) {
            result_type
        } else {
            self.add_error(HirLoweringError::TypeMismatch {
                span: hir_span_source,
                actual: expr_type,
                expected: expected_type,
            });
            Type::Any
        }
    }

    fn binary_expect(
        &mut self,
        lhs: (Expr, SourcedSpan),
        rhs: (Expr, SourcedSpan),
        expected_type: Type,
        result_type: Type,
    ) -> Type {
        let (lhs_type, (_lhs, lhs_span)) = (lhs.0.r#type(), lhs);
        let (rhs_type, (_rhs, rhs_span)) = (rhs.0.r#type(), rhs);
        if !Self::can_coerce_into(&lhs_type, &expected_type) {
            self.add_error(HirLoweringError::TypeMismatch {
                span: lhs_span,
                actual: lhs_type,
                expected: expected_type,
            });
            return Type::Any;
        }
        if !Self::can_coerce_into(&rhs_type, &expected_type) {
            self.add_error(HirLoweringError::TypeMismatch {
                span: rhs_span,
                actual: rhs_type,
                expected: expected_type,
            });
            Type::Any
        } else {
            result_type
        }
    }
    pub fn lower_type(r#type: &hir::Type) -> Type {
        match *r#type {
            hir::Type::BaseType(ref b) => Type::BaseType(Self::lower_base_type_sn(b)),
            hir::Type::ArrayType(ref a) => Self::lower_array_type_sbn(a),
            hir::Type::PairType(ref fst, ref snd) => Type::pair_type(
                Self::lower_pair_elem_type(fst),
                Self::lower_pair_elem_type(snd),
            ),
        }
    }

    #[inline]
    pub fn lower_type_sn(type_sn: &SN<hir::Type>) -> Type {
        Self::lower_type(type_sn.inner())
    }

    #[inline]
    pub const fn lower_base_type(base_type: &hir::BaseType) -> BaseType {
        match *base_type {
            hir::BaseType::Int => BaseType::Int,
            hir::BaseType::Bool => BaseType::Bool,
            hir::BaseType::Char => BaseType::Char,
            hir::BaseType::String => BaseType::String,
        }
    }

    #[inline]
    pub const fn lower_base_type_sn(base_type_sn: &SN<hir::BaseType>) -> BaseType {
        Self::lower_base_type(base_type_sn.inner())
    }

    #[inline]
    pub fn lower_array_type(array_type: &hir::ArrayType) -> Type {
        Type::array_type(Self::lower_type(&array_type.elem_type))
    }

    #[inline]
    pub fn lower_array_type_sbn(array_type_sbn: &SBN<hir::ArrayType>) -> Type {
        Self::lower_array_type(array_type_sbn.inner())
    }

    pub fn lower_pair_elem_type(pair_elem_type: &hir::PairElemType) -> Type {
        match *pair_elem_type {
            hir::PairElemType::ArrayType(ref a) => Self::lower_array_type_sbn(a),
            hir::PairElemType::BaseType(ref b) => Type::BaseType(Self::lower_base_type_sn(b)),
            hir::PairElemType::Pair(_) => Type::erased_pair(),
        }
    }

    #[inline]
    pub fn lookup_ident_sn(&mut self, ident: SN<hir::Ident>) -> Ident {
        // find actual type, or emmit error
        let actual_type = if let Some(actual_type) = self.hir_ident_symbol_table.get(ident.inner())
        {
            actual_type
        } else {
            self.add_error(HirLoweringError::UntypedIdentEncountered(ident.clone()));

            // the identifier is of type `Any` at this point, which casts to all other types
            return Ident {
                ident: ident.inner().clone(),
                r#type: Type::Any,
            };
        };

        // return resulting actual type
        Ident {
            ident: ident.inner().clone(),
            r#type: actual_type.clone(),
        }
    }
}

#[inline]
#[must_use]
pub fn lower_hir(
    ast_lowering: AstLoweringPhaseOutput,
    should_tailrec_optimize: bool,
) -> HirLoweringResult {
    // map program
    let mut ctx = HirLoweringCtx::new(
        ast_lowering.func_symbol_table,
        ast_lowering.ident_counter,
        should_tailrec_optimize,
    );
    let thir_program = ctx.lower_program(ast_lowering.hir_program);

    // map function symbol table
    let func_symbol_table = ctx
        .func_symbol_table
        .functions
        .iter()
        .map(|(ident, (ret_ty, args_ty))| {
            let ret_ty = HirLoweringCtx::lower_type(ret_ty);
            let args_ty = args_ty.iter().map(HirLoweringCtx::lower_type).collect();

            (ident.clone(), (ret_ty, args_ty))
        })
        .collect();

    // return result
    HirLoweringResult {
        output: thir_program,
        errors: ctx.errors,
        func_symbol_table: ThirFuncSymbolTable {
            functions: func_symbol_table,
        },
        hir_ident_symbol_table: ctx.hir_ident_symbol_table,
        ident_counter: ctx.ident_counter,
    }
}
