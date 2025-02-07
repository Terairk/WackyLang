use crate::ast::*;
use crate::error::SemanticError;
use crate::error::SemanticError::{InvalidIndexType, TypeMismatch};
use crate::fold_program::BoxedSliceFold;
use crate::fold_program::{Folder, NonEmptyFold};
use crate::rename::RenamedName;
use crate::rename::Renamer;
use crate::source::{SourcedNode, WithSourceId};
use crate::types::{SemanticType, Type};

type SN<T> = SourcedNode<T>;
type RenamedAst = Program<RenamedName, ()>;
type TypedAst = Program<RenamedName, SemanticType>;
impl SN<Expr<RenamedName, SemanticType>> {
    pub fn get_type(&self, renamer: &Renamer) -> SemanticType {
        match (**self).clone() {
            Expr::Liter(_, t) => t.clone(),
            Expr::Ident(_, t) => t.clone(),
            Expr::ArrayElem(arr_elem, _) => arr_elem
                .get_type(&renamer)
                .unwrap_or(SemanticType::Error(self.span())),
            Expr::Unary(_, _, t) => t.clone(),
            Expr::Binary(_, _, _, t) => t.clone(),
            Expr::Paren(_, t) => t.clone(),
            Expr::Error(_) => unreachable!(), // Parser errors can't be possible here
        }
    }
}

impl SN<LValue<RenamedName, SemanticType>> {
    pub fn get_type(&self, renamer: &Renamer) -> SemanticType {
        match (**self).clone() {
            LValue::ArrayElem(arr_elem, _t) => arr_elem
                .get_type(&renamer)
                .unwrap_or(SemanticType::Error(self.span())),
            LValue::PairElem(pair_elem, _t) => pair_elem
                .get_type(&renamer)
                .unwrap_or(SemanticType::Error(self.span())),
            LValue::Ident(_n, t) => t.clone(),
        }
    }
}

impl SN<RValue<RenamedName, SemanticType>> {
    pub fn get_type(&self, renamer: &Renamer) -> SemanticType {
        match (**self).clone() {
            RValue::Expr(expr, t) => expr.get_type(&renamer),
            RValue::ArrayLiter(_, t) => t.clone(),
            RValue::NewPair(_, _, t) => t.clone(),
            RValue::PairElem(pelem) => pelem
                .get_type(&renamer)
                .unwrap_or(SemanticType::Error(self.span())),
            RValue::Call { return_type, .. } => return_type.clone(),
        }
    }
}

impl SN<PairElem<RenamedName, SemanticType>> {
    pub fn get_type(&self, renamer: &Renamer) -> Option<SemanticType> {
        match (**self).clone() {
            PairElem::Fst(lvalue_sn) => {
                match lvalue_sn.get_type(renamer) {
                    SemanticType::Pair(fst, _) => Some(*fst.clone()),
                    SemanticType::ErasedPair => Some(SemanticType::AnyType),
                    _ => None, // TODO: Replace with smth else
                }
            }
            PairElem::Snd(lvalue_sn) => {
                match lvalue_sn.get_type(renamer) {
                    SemanticType::Pair(_, snd) => Some(*snd.clone()),
                    SemanticType::ErasedPair => Some(SemanticType::AnyType),
                    _ => None, // TODO: Replace with smth else
                }
            }
        }
    }
}

impl SN<ArrayElem<RenamedName, SemanticType>> {
    pub fn get_type(&self, renamer: &Renamer) -> Option<SemanticType> {
        let resolved_type = renamer.lookup_symbol_table(&self.array_name);
        match resolved_type {
            SemanticType::Array(elem_type) => Some(*elem_type.clone()),
            SemanticType::AnyType => Some(SemanticType::AnyType),
            _ => None, // TODO: replase with smth else
        }
    }
}

pub struct TypeResolver {
    pub type_errors: Vec<SemanticError>,
    pub renamer: Renamer,
    pub curr_func_ret_type: Option<Type>,
}

impl TypeResolver {
    fn new(renamer: Renamer) -> Self {
        TypeResolver {
            type_errors: Vec::new(),
            renamer,
            curr_func_ret_type: None,
        }
    }
    fn add_error(&mut self, error: SemanticError) {
        self.type_errors.push(error);
    }

    fn unary_expect(
        &mut self,
        expr: SN<Expr<RenamedName, SemanticType>>,
        expected_type: SemanticType,
        result_type: SemanticType,
    ) -> SemanticType {
        if expr.get_type(&self.renamer).can_coerce_into(&expected_type) {
            result_type
        } else {
            self.add_error(TypeMismatch(
                expr.span(),
                expr.get_type(&self.renamer),
                expected_type,
            ));
            SemanticType::Error(expr.span())
        }
    }
    fn binary_expect(
        &mut self,
        lhs: SN<Expr<RenamedName, SemanticType>>,
        rhs: SN<Expr<RenamedName, SemanticType>>,
        expected_type: SemanticType,
        result_type: SemanticType,
    ) -> SemanticType {
        if !lhs.get_type(&self.renamer).can_coerce_into(&expected_type) {
            self.add_error(TypeMismatch(
                lhs.span(),
                lhs.get_type(&self.renamer),
                expected_type,
            ));
            SemanticType::Error(lhs.span())
        } else if !rhs.get_type(&self.renamer).can_coerce_into(&expected_type) {
            self.add_error(TypeMismatch(
                rhs.span(),
                rhs.get_type(&self.renamer),
                expected_type,
            ));
            SemanticType::Error(rhs.span())
        } else {
            result_type
        }
    }
}

fn err_if_not_pair(resolved_type: &SemanticType, span: &WithSourceId, resolver: &mut TypeResolver) {
    if !matches!(
        resolved_type,
        SemanticType::Pair(_, _)
            | SemanticType::ErasedPair
            | SemanticType::Error(_)
            | SemanticType::AnyType
    ) {
        resolver.add_error(SemanticError::TypeMismatch(
            span.clone(),
            resolved_type.clone(),
            SemanticType::Pair(
                Box::new(SemanticType::AnyType),
                Box::new(SemanticType::AnyType),
            ),
        ));
    }
}

impl Folder for TypeResolver {
    type N = RenamedName;
    type T = ();
    type OutputN = RenamedName;
    type OutputT = SemanticType;

    fn fold_name_sn(&mut self, name: SN<Self::N>) -> SN<Self::OutputN> {
        name
    }

    fn fold_type(&mut self, _ty: Self::T) -> Self::OutputT {
        unreachable!();
    }

    fn fold_func(&mut self, func: Func<Self::N, Self::T>) -> Func<Self::OutputN, Self::OutputT> {
        self.curr_func_ret_type = Some(func.return_type.inner().clone());
        let func = Func {
            return_type: func.return_type, // Type remains unchanged
            name: func.name,
            params: func.params.fold_with(|param| self.fold_func_param(param)),
            body: self.fold_stat_block_sn(func.body),
        };
        self.curr_func_ret_type = None;
        return func;
    }

    fn fold_funcname_sn(&mut self, name: SN<Ident>) -> SN<Ident> {
        name
    }

    #[inline]
    fn fold_expr(
        &mut self,
        expr: SN<Expr<Self::N, Self::T>>,
    ) -> SN<Expr<Self::OutputN, Self::OutputT>> {
        expr.map_inner(|inner| match inner {
            Expr::Liter(lit, _ty) => {
                let resolved_type = match lit {
                    Liter::IntLiter(_) => SemanticType::Int,
                    Liter::BoolLiter(_) => SemanticType::Bool,
                    Liter::CharLiter(_) => SemanticType::Char,
                    Liter::StrLiter(_) => SemanticType::String,
                    Liter::PairLiter => SemanticType::ErasedPair,
                };
                Expr::Liter(lit, resolved_type)
            }
            Expr::Ident(name, _ty) => {
                let resolved_type = self.renamer.lookup_symbol_table(&name);
                Expr::Ident(name, resolved_type)
            }
            Expr::ArrayElem(array_elem, ty) => {
                let resolved_type = self.renamer.lookup_symbol_table(&array_elem.array_name);
                Expr::ArrayElem(self.fold_array_elem(array_elem), resolved_type)
            }
            Expr::Unary(op, expr, ty) => {
                let resolved_expr = self.fold_expr(expr);
                let resolved_type = match op.inner() {
                    UnaryOper::Not => self.unary_expect(
                        resolved_expr.clone(),
                        SemanticType::Bool,
                        SemanticType::Bool,
                    ),
                    UnaryOper::Minus => self.unary_expect(
                        resolved_expr.clone(),
                        SemanticType::Int,
                        SemanticType::Int,
                    ),
                    UnaryOper::Len => match resolved_expr.get_type(&self.renamer) {
                        SemanticType::Array(_) => SemanticType::Int,
                        _ => {
                            self.add_error(TypeMismatch(
                                resolved_expr.span(),
                                resolved_expr.get_type(&self.renamer),
                                SemanticType::Bool,
                            ));
                            SemanticType::Error(resolved_expr.span())
                        }
                    },
                    UnaryOper::Ord => self.unary_expect(
                        resolved_expr.clone(),
                        SemanticType::Char,
                        SemanticType::Int,
                    ),
                    UnaryOper::Chr => self.unary_expect(
                        resolved_expr.clone(),
                        SemanticType::Int,
                        SemanticType::Char,
                    ),
                };
                Expr::Unary(op, resolved_expr, resolved_type)
            }
            Expr::Binary(lhs, op, rhs, _ty) => {
                let resolved_lhs = self.fold_expr(lhs);
                let resolved_rhs = self.fold_expr(rhs);

                let resolved_type = match op.inner() {
                    // Operations that expect (Int, Int) -> Int
                    BinaryOper::Mul
                    | BinaryOper::Div
                    | BinaryOper::Mod
                    | BinaryOper::Add
                    | BinaryOper::Sub => self.binary_expect(
                        resolved_lhs.clone(),
                        resolved_rhs.clone(),
                        SemanticType::Int,
                        SemanticType::Int,
                    ),

                    // Comparison ops (Int, Int) -> Bool or (char, char) -> Bool
                    BinaryOper::Lte | BinaryOper::Lt | BinaryOper::Gte | BinaryOper::Gt => {
                        if resolved_lhs.get_type(&self.renamer) != SemanticType::Int {
                            self.binary_expect(
                                resolved_lhs.clone(),
                                resolved_rhs.clone(),
                                SemanticType::Char,
                                SemanticType::Bool,
                            )
                        } else {
                            self.binary_expect(
                                resolved_lhs.clone(),
                                resolved_rhs.clone(),
                                SemanticType::Int,
                                SemanticType::Bool,
                            )
                        }
                    }
                    // Equality ops: (T, T) -> Bool (allow any matching type)
                    BinaryOper::Eq | BinaryOper::Neq => {
                        let lhs_type = resolved_lhs.get_type(&self.renamer);
                        let rhs_type = resolved_rhs.get_type(&self.renamer);
                        if lhs_type.can_coerce_into(&rhs_type)
                            || rhs_type.can_coerce_into(&lhs_type)
                        {
                            SemanticType::Bool
                        } else {
                            self.add_error(TypeMismatch(
                                resolved_lhs.span(),
                                resolved_rhs.get_type(&self.renamer),
                                resolved_lhs.get_type(&self.renamer),
                            ));
                            SemanticType::Error(resolved_lhs.span())
                        }
                    }
                    // Logical ops (Bool, Bool) -> Bool
                    BinaryOper::And | BinaryOper::Or => self.binary_expect(
                        resolved_lhs.clone(),
                        resolved_rhs.clone(),
                        SemanticType::Bool,
                        SemanticType::Bool,
                    ),
                };
                Expr::Binary(resolved_lhs, op, resolved_rhs, resolved_type)
            }
            Expr::Paren(expr, _ty) => {
                let resolved_expr = self.fold_expr(expr);
                let resolved_type = resolved_expr.get_type(&self.renamer);
                Expr::Paren(resolved_expr, resolved_type)
            }
            Expr::Error(span) => Expr::Error(span),
        })
    }

    #[inline]
    fn fold_lvalue(
        &mut self,
        lvalue: SN<LValue<Self::N, Self::T>>,
    ) -> SN<LValue<Self::OutputN, Self::OutputT>> {
        let span = lvalue.span().clone();
        lvalue.map_inner(|inner| match inner {
            LValue::Ident(name, _ty) => {
                let resolved_type = self.renamer.lookup_symbol_table(&name);
                LValue::Ident(self.fold_name_sn(name), resolved_type)
            }
            LValue::ArrayElem(elem, _ty) => {
                let resolved_elem = self.fold_array_elem(elem);
                let resolved_type = resolved_elem
                    .get_type(&self.renamer)
                    .unwrap_or(SemanticType::Error(span.clone()));

                LValue::ArrayElem(resolved_elem, resolved_type)
            }
            LValue::PairElem(elem, ty) => {
                let resolved_elem = self.fold_pair_elem(elem);
                let resolved_type = resolved_elem
                    .get_type(&self.renamer)
                    .unwrap_or(SemanticType::Error(span.clone()));

                LValue::PairElem(resolved_elem, resolved_type)
            }
        })
    }

    #[inline]
    fn fold_rvalue(
        &mut self,
        rvalue: SN<RValue<Self::N, Self::T>>,
    ) -> SN<RValue<Self::OutputN, Self::OutputT>> {
        rvalue.map_inner(|inner| match inner {
            RValue::Expr(expr, _ty) => {
                let resolved_expr = self.fold_expr(expr);
                let resolved_type = resolved_expr.get_type(&self.renamer);
                RValue::Expr(resolved_expr, resolved_type)
            }
            RValue::ArrayLiter(exprs, ty) => {
                let resolved_exprs = exprs.fold_with(|expr| self.fold_expr(expr));
                if resolved_exprs.is_empty() {
                    return RValue::ArrayLiter(
                        resolved_exprs,
                        // TODO: This should work for now
                        SemanticType::Array(Box::new(SemanticType::AnyType)),
                    );
                }
                let mut resolved_type = resolved_exprs.clone()[0].get_type(&self.renamer);
                for expr in resolved_exprs.clone() {
                    let curr_expr_type = expr.get_type(&self.renamer);
                    if curr_expr_type != resolved_type {
                        if !curr_expr_type.can_coerce_into(&resolved_type) {
                            if resolved_type.can_coerce_into(&curr_expr_type) {
                                resolved_type = curr_expr_type;
                            } else {
                                self.add_error(TypeMismatch(
                                    expr.span(),
                                    expr.get_type(&self.renamer),
                                    resolved_type.clone(),
                                ));
                            }
                        }
                    }
                }
                RValue::ArrayLiter(resolved_exprs, SemanticType::Array(Box::new(resolved_type)))
            }
            RValue::NewPair(fst, snd, ty) => {
                let resolved_fst = self.fold_expr(fst);
                let resolved_snd = self.fold_expr(snd);
                let resolved_type = SemanticType::Pair(
                    Box::new(resolved_fst.get_type(&self.renamer)),
                    Box::new(resolved_snd.get_type(&self.renamer)),
                );

                RValue::NewPair(resolved_fst, resolved_snd, resolved_type)
            }
            RValue::PairElem(pair_elem) => RValue::PairElem(self.fold_pair_elem(pair_elem)),
            RValue::Call {
                func_name,
                args,
                return_type,
            } => {
                let resolved_return_type = self.renamer.lookup_func_return_type(&func_name.clone());
                let expected_args_types = self.renamer.lookup_func_args(&func_name.clone());
                let resolved_args = args.fold_with(|arg| self.fold_expr(arg));
                if expected_args_types.len() != resolved_args.len() {
                    self.add_error(SemanticError::MismatchedArgCount(
                        func_name.span(),
                        expected_args_types.len(),
                        resolved_args.len(),
                    ));
                }
                for (arg, expected_type) in resolved_args.clone().iter().zip(expected_args_types) {
                    if !arg.get_type(&self.renamer).can_coerce_into(&expected_type) {
                        self.add_error(TypeMismatch(
                            arg.span(),
                            arg.get_type(&self.renamer),
                            expected_type,
                        ));
                    }
                }
                RValue::Call {
                    func_name: self.fold_funcname_sn(func_name),
                    args: resolved_args,
                    return_type: resolved_return_type,
                }
            }
        })
    }

    #[inline]
    fn fold_var_definition(
        &mut self,
        r#type: SN<Type>,
        name: SN<Self::N>,
        rvalue: SN<RValue<Self::N, Self::T>>,
    ) -> Stat<Self::OutputN, Self::OutputT> {
        let expected_type = r#type.to_semantic_type();
        let resolved_rvalue = self.fold_rvalue(rvalue);
        let resolved_type = resolved_rvalue.get_type(&self.renamer);
        if !resolved_type.can_coerce_into(&expected_type) {
            self.add_error(TypeMismatch(name.span(), expected_type, resolved_type))
        }
        Stat::VarDefinition {
            r#type,
            name: self.fold_name_sn(name),
            rvalue: resolved_rvalue,
        }
    }

    #[inline]
    fn fold_stat(&mut self, stat: Stat<Self::N, Self::T>) -> Stat<Self::OutputN, Self::OutputT> {
        match stat {
            Stat::Skip => Stat::Skip, // need this to avoid type inference error
            Stat::VarDefinition {
                r#type,
                name,
                rvalue,
            } => self.fold_var_definition(r#type, name, rvalue),
            Stat::Assignment { lvalue, rvalue } => {
                let resolved_lvalue = self.fold_lvalue(lvalue);
                let resolved_rvalue = self.fold_rvalue(rvalue);
                let resolved_lval_type = resolved_lvalue.get_type(&self.renamer);
                let resolved_rval_type = resolved_rvalue.get_type(&self.renamer);
                if resolved_lval_type == SemanticType::AnyType
                    && resolved_rval_type == SemanticType::AnyType
                {
                    self.add_error(SemanticError::AssignmentWithBothSidesUnknown(
                        resolved_lvalue.span(),
                    ));
                } else if !&resolved_rval_type.can_coerce_into(&resolved_lval_type) {
                    self.add_error(TypeMismatch(
                        resolved_rvalue.span(),
                        resolved_lvalue.get_type(&self.renamer),
                        resolved_rvalue.get_type(&self.renamer),
                    ));
                }
                Stat::Assignment {
                    lvalue: resolved_lvalue,
                    rvalue: resolved_rvalue,
                }
            }
            Stat::Read(lvalue) => {
                let resolved_lvalue = self.fold_lvalue(lvalue);
                match resolved_lvalue.get_type(&self.renamer) {
                    SemanticType::Int | SemanticType::Char => (),
                    _ => self.add_error(TypeMismatch(
                        resolved_lvalue.span(),
                        resolved_lvalue.get_type(&self.renamer),
                        SemanticType::Int,
                    )),
                }
                Stat::Read(resolved_lvalue)
            }
            Stat::Free(expr) => {
                let resolved_expr = self.fold_expr(expr);
                match resolved_expr.get_type(&self.renamer) {
                    SemanticType::Array(_) | SemanticType::Pair(_, _) => (),
                    _ => self.add_error(TypeMismatch(
                        resolved_expr.span(),
                        resolved_expr.get_type(&self.renamer),
                        SemanticType::Array(Box::new(SemanticType::AnyType)),
                    )),
                }
                Stat::Free(resolved_expr)
            }
            Stat::Return(expr) => {
                let resolved_expr = self.fold_expr(expr);
                if let Some(expected_ret_type) = self.curr_func_ret_type.as_ref() {
                    let resolved_ret_value = resolved_expr.get_type(&self.renamer);
                    let expected_ret_type = expected_ret_type.to_semantic_type();
                    if !&resolved_ret_value.can_coerce_into(&expected_ret_type) {
                        self.add_error(TypeMismatch(
                            resolved_expr.span(),
                            expected_ret_type,
                            resolved_ret_value,
                        ))
                    }
                } // otherwise we're in the main body, where the renaming stage will have emitted an error if there's a return statement

                Stat::Return(resolved_expr)
            }
            Stat::Exit(expr) => {
                let resolved_expr = self.fold_expr(expr);
                if resolved_expr.get_type(&self.renamer) != SemanticType::Int {
                    self.add_error(TypeMismatch(
                        resolved_expr.span(),
                        resolved_expr.get_type(&self.renamer),
                        SemanticType::Int,
                    ))
                }
                Stat::Exit(resolved_expr)
            }
            Stat::Print(expr) => Stat::Print(self.fold_expr(expr)),
            Stat::Println(expr) => Stat::Println(self.fold_expr(expr)),
            Stat::IfThenElse {
                if_cond,
                then_body,
                else_body,
            } => {
                let resolved_if_cond = self.fold_expr(if_cond);
                if resolved_if_cond.get_type(&self.renamer) != SemanticType::Bool {
                    self.add_error(TypeMismatch(
                        resolved_if_cond.span(),
                        resolved_if_cond.get_type(&self.renamer),
                        SemanticType::Bool,
                    ));
                }
                Stat::IfThenElse {
                    if_cond: resolved_if_cond,
                    then_body: self.fold_stat_block_sn(then_body),
                    else_body: self.fold_stat_block_sn(else_body),
                }
            }
            Stat::WhileDo { while_cond, body } => {
                let resolved_while_cond = self.fold_expr(while_cond);
                if resolved_while_cond.get_type(&self.renamer) != SemanticType::Bool {
                    self.add_error(TypeMismatch(
                        resolved_while_cond.span(),
                        resolved_while_cond.get_type(&self.renamer),
                        SemanticType::Bool,
                    ));
                }
                Stat::WhileDo {
                    while_cond: resolved_while_cond,
                    body: self.fold_stat_block_sn(body),
                }
            }
            Stat::Scoped(body) => Stat::Scoped(self.fold_stat_block_sn(body)),
        }
    }

    #[inline]
    fn fold_pair_elem(
        &mut self,
        elem: SN<PairElem<Self::N, Self::T>>,
    ) -> SN<PairElem<Self::OutputN, Self::OutputT>> {
        let span = elem.span().clone();
        elem.map_inner(|inner| match inner {
            PairElem::Fst(expr) => {
                let lval = self.fold_lvalue(expr);
                err_if_not_pair(&lval.get_type(&self.renamer), &span, self);

                PairElem::Fst(lval)
            }
            PairElem::Snd(expr) => {
                let lval = self.fold_lvalue(expr);
                err_if_not_pair(&lval.get_type(&self.renamer), &span, self);

                PairElem::Snd(lval)
            }
        })
    }

    #[inline]
    fn fold_array_elem(
        &mut self,
        elem: SN<ArrayElem<Self::N, Self::T>>,
    ) -> SN<ArrayElem<Self::OutputN, Self::OutputT>> {
        let resolved_indices = (*elem)
            .clone()
            .indices
            .map_with(|index| self.fold_expr(index));
        let arr_type = self.renamer.lookup_symbol_table(&elem.array_name);

        let mut curr_arr = arr_type.clone();
        for i in resolved_indices.iter() {
            let i_type = i.get_type(&self.renamer);
            if !matches!(i_type, SemanticType::Int | SemanticType::Error(_)) {
                self.add_error(InvalidIndexType(
                    resolved_indices.first().span(),
                    resolved_indices.first().get_type(&self.renamer),
                ));
            }

            match curr_arr {
                SemanticType::Array(inner) => {
                    curr_arr = *inner;
                }
                SemanticType::AnyType | SemanticType::Error(_) => {}
                _ => {
                    self.add_error(SemanticError::TypeMismatch(
                        elem.span().clone(),
                        curr_arr.clone(),
                        SemanticType::Array(Box::new(SemanticType::AnyType)),
                    ));
                }
            }
        }

        elem.map_inner(|elem| ArrayElem {
            array_name: self.fold_name_sn(elem.array_name),
            indices: { resolved_indices },
        })
    }
}

pub fn typecheck(renamer: Renamer, renamed_ast: RenamedAst) -> (TypedAst, TypeResolver) {
    let mut type_resolver = TypeResolver::new(renamer);
    let typed_ast = type_resolver.fold_program(renamed_ast);
    (typed_ast, type_resolver)
}
