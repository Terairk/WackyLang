use crate::ast::*;
use crate::types::{SemanticType, Type};
use crate::fold_program::{Folder, NonEmptyFold};
use crate::source::{SourcedNode, SourcedSpan};
use crate::fold_program::{BoxedSliceFold, };
use crate::rename::{RenamedName, SemanticError};
use crate::rename::SemanticError::{InvalidIndexType, InvalidNumberOfIndexes, SimpleTypeMismatch, TypeMismatch};
use crate::rename::Renamer;

type SN<T> = SourcedNode<T>;
type RenamedAst = Program<RenamedName, ()>;
type TypedAst = Program<RenamedName, SemanticType>;
impl Expr<RenamedName, SemanticType>{
    pub fn get_type(&self, renamer: &Renamer) -> SemanticType {
        match self {
            Expr::Liter(_, t) => t.clone(),
            Expr::Ident(_, t) => t.clone(),
            Expr::ArrayElem(arr_elem, _) => arr_elem.get_type(&renamer),
            Expr::Unary(_, _, t) => t.clone(),
            Expr::Binary(_, _, _, t) => t.clone(),
            Expr::Paren(_, t) => t.clone(),
            Expr::Error(_) => unreachable!(), // Parser errors can't be possible here
        }
    }
}

impl LValue<RenamedName, SemanticType> {
    pub fn get_type(&self, renamer: &Renamer) -> SemanticType {
        match self {
            LValue::ArrayElem(arr_elem, _t) => arr_elem.get_type(&renamer),
            LValue::PairElem(pair_elem, _t) => pair_elem.get_type(&renamer),
            LValue::Ident(_n, t) => t.clone(),
        }
    }
}

impl RValue<RenamedName, SemanticType> {
    pub fn get_type(&self, renamer: &Renamer) -> SemanticType {
        match self {
            RValue::Expr(expr, t) => expr.get_type(&renamer),
            RValue::ArrayLiter(_, t) => t.clone(),
            RValue::NewPair(_, _, t) => t.clone(),
            RValue::PairElem(pelem) => {
                pelem.get_type(renamer)
            },
            RValue::Call { return_type, .. } => return_type.clone(),
        }
    }
}

impl PairElem<RenamedName, SemanticType> {
    pub fn get_type(&self, renamer: &Renamer) -> SemanticType {
        match self {
            PairElem::Fst(lvalue_sn) => {
                match lvalue_sn.inner().get_type(renamer) {
                    SemanticType::Pair(fst, _) => *fst.clone(),
                    _ => SemanticType::Unknown // TODO: Replace with smth else
                }
            },
            PairElem::Snd(lvalue_sn) => {
                match lvalue_sn.inner().get_type(renamer) {
                    SemanticType::Pair(_, snd) => *snd.clone(),
                    _ => SemanticType::Unknown // TODO: Replace with smth else
                }
            },
        }
    }
}

impl ArrayElem<RenamedName, SemanticType> {
    pub fn get_type(&self, renamer: &Renamer) -> SemanticType {
        let resolved_type = renamer.lookup_symbol_table(&self.array_name);
        match resolved_type {
            SemanticType::Array(elem_type) => *elem_type.clone(),
            _ => SemanticType::Unknown // TODO: replase with smth else
        }
    }
}

pub struct TypeResolver {
    pub type_errors: Vec<SemanticError>,
    pub renamer: Renamer,
}

impl TypeResolver {

    fn new(renamer: Renamer) -> Self {
        TypeResolver{
            type_errors: Vec::new(),
            renamer
        }
    }
    fn add_error(&mut self, error: SemanticError) {
        self.type_errors.push(error);
    }

    fn unary_expect(&mut self, expr: SN<Expr<RenamedName, SemanticType>>,
                    expected_type: SemanticType, result_type: SemanticType) -> SemanticType {
        if expr.get_type(&self.renamer) == expected_type {
            result_type
        } else {
            self.add_error(TypeMismatch(expr.span(), expr.get_type(&self.renamer), expected_type));
            SemanticType::Error(expr.span())
        }
    }
    fn binary_expect(&mut self, lhs: SN<Expr<RenamedName, SemanticType>>,
                     rhs: SN<Expr<RenamedName, SemanticType>>,
                     expected_type: SemanticType,
                     result_type: SemanticType) -> SemanticType {
        if lhs.get_type(&self.renamer) != expected_type {
            self.add_error(TypeMismatch(lhs.span(), lhs.get_type(&self.renamer), expected_type));
            SemanticType::Error(lhs.span())
        } else if rhs.get_type(&self.renamer) != expected_type {
            self.add_error(TypeMismatch(rhs.span(), rhs.get_type(&self.renamer), expected_type));
            SemanticType::Error(rhs.span())
        } else {
            result_type
        }
    }
}

impl Folder for TypeResolver {
    type N = RenamedName;
    type T = ();
    type OutputN = RenamedName;
    type OutputT = SemanticType;

    fn fold_name_sn(&mut self, name: SN<Self::N>) -> SN<Self::OutputN> { name }

    fn fold_type(&mut self, _ty: Self::T) -> Self::OutputT {
        println!("fold_type is called, which shouldn't be called!");
        SemanticType::Int
    }

    fn fold_funcname_sn(&mut self, name: SN<Ident>) -> SN<Ident> { name }

    #[inline]
    fn fold_expr(&mut self, expr: Expr<Self::N, Self::T>) -> Expr<Self::OutputN, Self::OutputT> {
        match expr {
            Expr::Liter(lit, _ty) => {
                let resolved_type = match lit {
                    Liter::IntLiter(_) => SemanticType::Int,
                    Liter::BoolLiter(_) => SemanticType::Bool,
                    Liter::CharLiter(_) => SemanticType::Char,
                    Liter::StrLiter(_) => SemanticType::String,
                    Liter::PairLiter => {
                        let fst = SemanticType::AnyType;
                        let snd = SemanticType::AnyType;
                        SemanticType::Pair(Box::from(fst), Box::from(snd))
                    }
                };
                Expr::Liter(lit, resolved_type)
            },
            Expr::Ident(name, _ty) => {
                let resolved_type = self.renamer.lookup_symbol_table(&name);
                Expr::Ident(name, resolved_type)
            },
            Expr::ArrayElem(array_elem, ty) => {
                let resolved_type = self.renamer.lookup_symbol_table(&array_elem.array_name);
                Expr::ArrayElem(self.fold_array_elem(array_elem), resolved_type)
            }
            Expr::Unary(op, expr, ty) => {
                let resolved_expr = self.fold_expr_sn(expr);
                let resolved_type = match op.inner() {
                    UnaryOper::Not => {
                        self.unary_expect(resolved_expr.clone(), SemanticType::Bool, SemanticType::Bool)
                    }
                    UnaryOper::Minus => {
                        self.unary_expect(resolved_expr.clone(), SemanticType::Int, SemanticType::Int)
                    }
                    UnaryOper::Len => {
                        match resolved_expr.get_type(&self.renamer) {
                            SemanticType::Array(_) => SemanticType::Int,
                            _ => {
                                self.add_error(TypeMismatch(resolved_expr.span(), resolved_expr.get_type(&self.renamer), SemanticType::Bool));
                                SemanticType::Error(resolved_expr.span())
                            }
                        }
                    }
                    UnaryOper::Ord => {
                        self.unary_expect(resolved_expr.clone(), SemanticType::Char, SemanticType::Int)
                    }
                    UnaryOper::Chr => {
                        self.unary_expect(resolved_expr.clone(), SemanticType::Int, SemanticType::Char)
                    }
                };
                Expr::Unary(op, resolved_expr, resolved_type)
            }
            Expr::Binary(lhs, op, rhs, _ty) => {
                let resolved_lhs = self.fold_expr_sn(lhs);
                let resolved_rhs = self.fold_expr_sn(rhs);

                let resolved_type = match op.inner() {
                    // Operations that expect (Int, Int) -> Int
                    BinaryOper::Mul | BinaryOper::Div | BinaryOper::Mod | BinaryOper::Add | BinaryOper::Sub => {
                        self.binary_expect(resolved_lhs.clone(), resolved_rhs.clone(), SemanticType::Int, SemanticType::Int)
                    }
                    // Comparison ops (Int, Int) -> Bool or (char, char) -> Bool
                    BinaryOper::Lte | BinaryOper::Lt | BinaryOper::Gte | BinaryOper::Gt => {
                        if resolved_lhs.get_type(&self.renamer) != SemanticType::Int {
                            self.binary_expect(resolved_lhs.clone(), resolved_rhs.clone(), SemanticType::Char, SemanticType::Bool)
                        } else {
                            self.binary_expect(resolved_lhs.clone(), resolved_rhs.clone(), SemanticType::Int, SemanticType::Bool)
                        }
                    }
                    // Equality ops: (T, T) -> Bool (allow any matching type)
                    BinaryOper::Eq | BinaryOper::Neq => {
                        if resolved_lhs.get_type(&self.renamer) == resolved_rhs.get_type(&self.renamer) {
                            SemanticType::Bool
                        } else {
                            self.add_error(TypeMismatch(resolved_lhs.span(), resolved_rhs.get_type(&self.renamer), resolved_lhs.get_type(&self.renamer)));
                            SemanticType::Error(resolved_lhs.span())
                        }
                    }
                    // Logical ops (Bool, Bool) -> Bool
                    BinaryOper::And | BinaryOper::Or => {
                        self.binary_expect(resolved_lhs.clone(), resolved_rhs.clone(), SemanticType::Bool, SemanticType::Bool)
                    }
                };
                Expr::Binary(resolved_lhs, op, resolved_rhs, resolved_type)
            },
            Expr::Paren(expr, _ty) => {
                let resolved_expr = self.fold_expr_sn(expr);
                let resolved_type = resolved_expr.get_type(&self.renamer);
                Expr::Paren(resolved_expr, resolved_type)
            },
            Expr::Error(span) => Expr::Error(span),
        }
    }

    #[inline]
    fn fold_lvalue(
        &mut self,
        lvalue: LValue<Self::N, Self::T>,
    ) -> LValue<Self::OutputN, Self::OutputT> {
        match lvalue {
            LValue::Ident(name, _ty) => {
                let resolved_type = self.renamer.lookup_symbol_table(&name);
                LValue::Ident(self.fold_name_sn(name), resolved_type)
            },
            LValue::ArrayElem(elem, _ty) => {
                let resolved_type = self.renamer.lookup_symbol_table(&elem.array_name);
                LValue::ArrayElem(self.fold_array_elem(elem), resolved_type)
            }
            LValue::PairElem(elem, ty) => {
                let resolved_elem = self.fold_pair_elem_sn(elem);
                let resolved_type = resolved_elem.inner().get_type(&self.renamer);
                LValue::PairElem(resolved_elem, resolved_type)
            }
        }
    }

    #[inline]
    fn fold_rvalue(
        &mut self,
        rvalue: RValue<Self::N, Self::T>,
    ) -> RValue<Self::OutputN, Self::OutputT> {
        match rvalue {
            RValue::Expr(expr, _ty) => {
                let resolved_expr = self.fold_expr_sn(expr);
                let resolved_type = resolved_expr.get_type(&self.renamer);
                RValue::Expr(resolved_expr, resolved_type)
            },
            RValue::ArrayLiter(exprs, ty) => {
                let resolved_exprs = exprs.fold_with(|expr| self.fold_expr_sn(expr));
                if resolved_exprs.is_empty() {
                    return RValue::ArrayLiter(
                        resolved_exprs,
                        // TODO: This should work for now
                        SemanticType::Array(Box::new(SemanticType::AnyType)),
                    )
                }
                let resolved_type = resolved_exprs.clone()[0].get_type(&self.renamer);
                for expr in resolved_exprs.clone() {
                    if expr.get_type(&self.renamer) != resolved_type {
                        self.add_error(TypeMismatch(expr.span(), expr.get_type(&self.renamer), resolved_type.clone()));
                    }
                }
                RValue::ArrayLiter(
                    resolved_exprs,
                    SemanticType::Array(Box::new(resolved_type)),
                )
            },
            RValue::NewPair(fst, snd, ty) => {
                let resolved_fst = self.fold_expr_sn(fst);
                let resolved_snd = self.fold_expr_sn(snd);
                let resolved_type = SemanticType::Pair(Box::new(resolved_fst.get_type(&self.renamer)), Box::new(resolved_snd.get_type(&self.renamer)));

                RValue::NewPair(
                    resolved_fst,
                    resolved_snd,
                    resolved_type,
                )
            },
            RValue::PairElem(pair_elem) => RValue::PairElem(self.fold_pair_elem(pair_elem)),
            RValue::Call {
                func_name,
                args,
                return_type,
            } => {
                let resolved_return_type = self.renamer.lookup_func_return_type(&func_name.clone());
                let expected_args_types = self.renamer.lookup_func_args(&func_name.clone());
                let resolved_args = args.fold_with(|arg| self.fold_expr_sn(arg));
                if expected_args_types.len() != resolved_args.len() {
                    self.add_error(SemanticError::MismatchedArgCount(expected_args_types.len(), resolved_args.len()));
                }
                for (arg, expected_type) in resolved_args.clone().iter().zip(expected_args_types) {
                    if arg.get_type(&self.renamer) != expected_type {
                        self.add_error(TypeMismatch(arg.span(), arg.get_type(&self.renamer), expected_type));
                    }
                }
                RValue::Call {
                    func_name: self.fold_funcname_sn(func_name),
                    args: resolved_args,
                    return_type: resolved_return_type,
                }
            },
        }
    }

    #[inline]
    fn fold_var_definition(
        &mut self,
        r#type: SN<Type>,
        name: SN<Self::N>,
        rvalue: RValue<Self::N, Self::T>,
    ) -> Stat<Self::OutputN, Self::OutputT> {
        let given_type = r#type.to_semantic_type();
        let resolved_rvalue = self.fold_rvalue(rvalue);
        let resolved_type = resolved_rvalue.get_type(&self.renamer);
        if given_type != resolved_type {
            self.add_error(TypeMismatch(name.span(), given_type, resolved_type))
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
                if resolved_rvalue.get_type(&self.renamer) != resolved_lvalue.get_type(&self.renamer) {
                    println!("Type mismatch in Assignment!");
                    self.add_error(SimpleTypeMismatch(resolved_lvalue.get_type(&self.renamer), resolved_rvalue.get_type(&self.renamer)));
                }
                Stat::Assignment {
                    lvalue: resolved_lvalue,
                    rvalue: resolved_rvalue,
                }
            },
            Stat::Read(lvalue) => {
                let resolved_lvalue = self.fold_lvalue(lvalue);
                match resolved_lvalue.get_type(&self.renamer) {
                    SemanticType::Int | SemanticType::Char => (),
                    _ => {
                        println!("Type mismatch in Read! {:?}", resolved_lvalue.get_type(&self.renamer));
                        self.add_error(SimpleTypeMismatch(resolved_lvalue.get_type(&self.renamer), SemanticType::Int))
                    }
                }
                Stat::Read(resolved_lvalue)
            },
            Stat::Free(expr) => {
                let resolved_expr = self.fold_expr_sn(expr);
                match resolved_expr.get_type(&self.renamer) {
                    SemanticType::Array(_) | SemanticType::Pair(_, _) => (),
                    _ => self.add_error(TypeMismatch(resolved_expr.span(),
                                                     resolved_expr.get_type(&self.renamer),
                                                     SemanticType::Array(Box::new(SemanticType::AnyType)))),
                }
                Stat::Free(resolved_expr)
            },
            Stat::Return(expr) => Stat::Return(self.fold_expr_sn(expr)),
            Stat::Exit(expr) => {
                let resolved_expr = self.fold_expr_sn(expr);
                if resolved_expr.get_type(&self.renamer) != SemanticType::Int {
                    self.add_error(TypeMismatch(resolved_expr.span(), resolved_expr.get_type(&self.renamer), SemanticType::Int))
                }
                Stat::Exit(resolved_expr)
            },
            Stat::Print(expr) => Stat::Print(self.fold_expr_sn(expr)),
            Stat::Println(expr) => Stat::Println(self.fold_expr_sn(expr)),
            Stat::IfThenElse {
                if_cond,
                then_body,
                else_body,
            } => {
                let resolved_if_cond = self.fold_expr_sn(if_cond);
                if resolved_if_cond.get_type(&self.renamer) != SemanticType::Bool {
                    self.add_error(TypeMismatch(resolved_if_cond.span() ,resolved_if_cond.get_type(&self.renamer), SemanticType::Bool));
                }
                Stat::IfThenElse {
                    if_cond: resolved_if_cond,
                    then_body: self.fold_stat_block_sn(then_body),
                    else_body: self.fold_stat_block_sn(else_body),
                }
            },
            Stat::WhileDo { while_cond, body } => {
                let resolved_while_cond = self.fold_expr_sn(while_cond);
                if resolved_while_cond.get_type(&self.renamer) != SemanticType::Bool {
                    self.add_error(TypeMismatch(resolved_while_cond.span() ,resolved_while_cond.get_type(&self.renamer), SemanticType::Bool));
                }
                Stat::WhileDo {
                    while_cond: resolved_while_cond,
                    body: self.fold_stat_block_sn(body),
                }
            },
            Stat::Scoped(body) => Stat::Scoped(self.fold_stat_block_sn(body)),
        }
    }

    #[inline]
    fn fold_pair_elem(
        &mut self,
        elem: PairElem<Self::N, Self::T>,
    ) -> PairElem<Self::OutputN, Self::OutputT> {
        match elem {
            PairElem::Fst(expr) => PairElem::Fst(self.fold_lvalue_sn(expr)),
            PairElem::Snd(expr) => PairElem::Snd(self.fold_lvalue_sn(expr)),
        }
    }
    #[inline]
    fn fold_array_elem(
        &mut self,
        elem: ArrayElem<Self::N, Self::T>,
    ) -> ArrayElem<Self::OutputN, Self::OutputT> {
        let resolved_indices = elem.indices.map_with(|index| self.fold_expr_sn(index));
        // if resolved_indices.len() != 1 {
        //     self.add_error(InvalidNumberOfIndexes(resolved_indices.len()));
        // }
        if resolved_indices.first().get_type(&self.renamer) != SemanticType::Int {
            self.add_error(InvalidIndexType(resolved_indices.first().get_type(&self.renamer)));
        }
        ArrayElem {
            array_name: self.fold_name_sn(elem.array_name),
            indices: { resolved_indices },
        }
    }
}

pub fn typecheck(renamer: Renamer, renamed_ast: RenamedAst) -> (TypedAst, TypeResolver) {
    let mut type_resolver = TypeResolver::new(renamer);
    let typed_ast = type_resolver.fold_program(renamed_ast);
    (typed_ast, type_resolver)
}
