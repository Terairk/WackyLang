use chumsky::span::SimpleSpan;
use crate::ast;
use crate::ast::{Liter, UnaryOper, Expr, Func, Ident, StatBlock, LValue, RValue, Stat, FuncParam};
use crate::ast::BinaryOper;
use crate::ast::PairElem;
use crate::types::{SemanticType, Type};
use crate::fold_program::{Folder, NonEmptyFold};
use crate::source::{SourcedNode, SourcedSpan};
use crate::fold_program::{BoxedSliceFold, };
use crate::rename::SemanticError;
use crate::rename::SemanticError::{TypeMismatch, SimpleTypeMismatch};

type SN<T> = SourcedNode<T>;

// Function template to look up type from symbol table
pub fn lookup_symbol_table(ident: &Ident) -> SemanticType {
    todo!()
}

pub fn lookup_func_args(ident: &Ident) -> Box<[SemanticType]> {
    todo!()
}

pub fn lookup_func_return_type(ident: &Ident) -> SemanticType {
    todo!()
}

impl<N, T: Clone> Expr<N, T> {
    pub fn get_type(&self) -> T {
        match self {
            Expr::Liter(_, t) => t.clone(),
            Expr::Ident(_, t) => t.clone(),
            Expr::ArrayElem(_, t) => t.clone(),
            Expr::Unary(_, _, t) => t.clone(),
            Expr::Binary(_, _, _, t) => t.clone(),
            Expr::Paren(_, t) => t.clone(),
            Expr::Error(_) => unreachable!(), // Parser errors can't be possible here
        }
    }
}

impl<N, T: Clone> LValue<N, T> {
    pub fn get_type(&self) -> T {
        match self {
            LValue::ArrayElem(_, t) => t.clone(),
            LValue::PairElem(_, t) => t.clone(),
            LValue::Ident(n) => todo!(), // TODO: Make Ident less generic?
        }
    }
}

impl<N, T: Clone> RValue<N, T> {
    pub fn get_type(&self) -> T {
        match self {
            RValue::Expr(_, t) => t.clone(),
            RValue::ArrayLiter(_, t) => t.clone(),
            RValue::NewPair(_, _, t) => t.clone(),
            RValue::PairElem(pelem) => todo!(),
            RValue::Call {return_type, .. } => return_type.clone(),
        }
    }
}


/*
impl Expr<ast::Ident, ()> {
    pub fn resolve_type(self) -> Expr<ast::Ident, SemanticType> {
        match self {
            // Resolve literals to their corresponding types
            Expr::Liter(literal, _) => {
                let resolved_type = match literal {
                    Liter::IntLiter(_) => SemanticType::Int,
                    Liter::BoolLiter(_) => SemanticType::Bool,
                    Liter::CharLiter(_) => SemanticType::Char,
                    Liter::StrLiter(_) => SemanticType::String,
                    Liter::PairLiter => {
                        let fst = SemanticType::Int; // TODO: Type of the first elem of pair
                        let snd = SemanticType::Int; // TODO: Type of the second elem of pair
                        SemanticType::Pair(Box::from(fst), Box::from(snd))
                    }
                };
                Expr::Liter(literal, resolved_type)
            }

            // Lookup identifier types in the symbol table
            Expr::Ident(ident, _) => {
                let resolved_type = lookup_symbol_table(&ident);
                Expr::Ident(ident, resolved_type)
            }
            // Resolve unary operations
            Expr::Unary(op, expr, _) => {
                let resolved_expr = expr
                    .map_inner(|inner_expr| inner_expr.resolve_type());
                let resolved_type = match op.inner() {
                    UnaryOper::Not => {
                        if resolved_expr.get_type() == SemanticType::Bool {
                            SemanticType::Bool
                        } else {
                            SemanticType::Error(resolved_expr.span())
                        }
                    }
                    UnaryOper::Minus => {
                        if resolved_expr.get_type() == SemanticType::Int {
                            SemanticType::Int
                        } else {
                            SemanticType::Error(resolved_expr.span())
                        }
                    }
                    UnaryOper::Len => {
                        if resolved_expr.get_type() == SemanticType::String {
                            SemanticType::Int
                        } else {
                            SemanticType::Error(resolved_expr.span())
                        }
                    }
                    UnaryOper::Ord => {
                        if resolved_expr.get_type() == SemanticType::Char {
                            SemanticType::Int
                        } else {
                            SemanticType::Error(resolved_expr.span())
                        }
                    }
                    UnaryOper::Chr => {
                        if resolved_expr.get_type() == SemanticType::Int {
                            SemanticType::Char
                        } else {
                            SemanticType::Error(resolved_expr.span())
                        }
                    }
                };
                Expr::Unary(op, resolved_expr, resolved_type)
            }

            // Resolve binary operations
            Expr::Binary(lhs, op, rhs, _) => {
                let resolved_lhs = lhs.map_inner(|e| e.resolve_type());
                let resolved_rhs = rhs.map_inner(|e| e.resolve_type());

                let resolved_type = match op.inner() {
                    // Operations that expect (Int, Int) -> Int
                    BinaryOper::Mul | BinaryOper::Div | BinaryOper::Mod | BinaryOper::Add | BinaryOper::Sub => {
                        if resolved_lhs.get_type() == SemanticType::Int
                            && resolved_rhs.get_type() == SemanticType::Int {
                            SemanticType::Int
                        } else {
                            SemanticType::Error(resolved_lhs.span())
                        }
                    }
                    // Comparison ops (Int, Int) -> Bool
                    BinaryOper::Lte | BinaryOper::Lt | BinaryOper::Gte | BinaryOper::Gt => {
                        if resolved_lhs.get_type() == SemanticType::Int
                            && resolved_rhs.get_type() == SemanticType::Int {
                            SemanticType::Bool
                        } else {
                            SemanticType::Error(resolved_lhs.span())
                        }
                    }
                    // Equality ops: (T, T) -> Bool (allow any matching type)
                    BinaryOper::Eq | BinaryOper::Neq => {
                        if resolved_lhs.get_type() == resolved_rhs.get_type() {
                            SemanticType::Bool
                        } else {
                            SemanticType::Error(resolved_lhs.span())
                        }
                    }
                    // Logical ops (Bool, Bool) -> Bool
                    BinaryOper::And | BinaryOper::Or => {
                        if resolved_lhs.get_type() == SemanticType::Bool
                            && resolved_rhs.get_type() == SemanticType::Bool {
                            SemanticType::Bool
                        } else {
                            SemanticType::Error(resolved_lhs.span())
                        }
                    }
                };

                Expr::Binary(resolved_lhs, op, resolved_rhs, resolved_type)
            }

            // Resolve Paren
            Expr::Paren(inner_expr, _) => {
                let resolved_expr = inner_expr.map_inner(|inner_expr| inner_expr.resolve_type());
                let resolved_type = resolved_expr.get_type();
                Expr::Paren(resolved_expr, resolved_type)
            }


            // Resolve ArrayElem
            Expr::ArrayElem(arr_elem, _) => {
                todo!();
                let array_type = lookup_symbol_table(arr_elem.array_name.inner());
                let resolved_elem = arr_elem.resolve_type();

                // Step 2: Ensure it's actually an array
                match array_type {
                    SemanticType::Array(_) => (),
                    _ => todo!(), // Not an array, TODO: return correct error span
                }
                Expr::ArrayElem(resolved_elem, array_type)
            }

            // Preserve errors
            Expr::Error(span) => Expr::Error(span),

        }
    }
}
*/
struct TypeResolver {
    type_errors: Vec<SemanticError>
}

impl TypeResolver {
    fn add_error(&mut self, error: SemanticError) {
        self.type_errors.push(error);
    }

    fn unary_expect(&mut self, expr: SN<Expr<Ident, SemanticType>>,
                    expected_type: SemanticType, result_type: SemanticType) -> SemanticType {
        if expr.get_type() == expected_type {
            SemanticType::Int
        } else {
            self.add_error(SimpleTypeMismatch(expr.get_type(), expected_type));
            SemanticType::Error(expr.span())
        }
    }
    fn binary_expect(&mut self, lhs: SN<Expr<Ident, SemanticType>>,
                     rhs: SN<Expr<Ident, SemanticType>>,
                     expected_type: SemanticType,
                     result_type: SemanticType) -> SemanticType {
        if lhs.get_type() != expected_type {
            self.add_error(SimpleTypeMismatch(lhs.get_type(), expected_type));
            SemanticType::Error(lhs.span())
        } else if rhs.get_type() != expected_type {
            self.add_error(SimpleTypeMismatch(rhs.get_type(), expected_type));
            SemanticType::Error(rhs.span())
        } else {
            result_type
        }
    }
}

impl Folder for TypeResolver {
    type N = Ident;
    type T = ();
    type OutputN = Ident;
    type OutputT = SemanticType;
    fn fold_name_sn(&mut self, name: SN<Self::N>) -> SN<Self::OutputN> { name }
    fn fold_type(&mut self, _ty: Self::T) -> Self::OutputT {
        println!("fold_type is called, which shouldn't be called!");
        SemanticType::Int
    }
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
                let resolved_type = lookup_symbol_table(&name);
                Expr::Ident(name, resolved_type)
            },
            Expr::ArrayElem(array_elem, ty) => {
                let resolved_type = lookup_symbol_table(&array_elem.array_name.inner());
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
                        match resolved_expr.get_type() {
                            SemanticType::Array(_) => SemanticType::Int,
                            _ => {
                                self.add_error(SimpleTypeMismatch(resolved_expr.get_type(), SemanticType::Bool));
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
                    // Comparison ops (Int, Int) -> Bool
                    BinaryOper::Lte | BinaryOper::Lt | BinaryOper::Gte | BinaryOper::Gt => {
                        self.binary_expect(resolved_lhs.clone(), resolved_rhs.clone(), SemanticType::Int, SemanticType::Bool)
                    }
                    // Equality ops: (T, T) -> Bool (allow any matching type)
                    BinaryOper::Eq | BinaryOper::Neq => {
                        if resolved_lhs.get_type() == resolved_rhs.get_type() {
                            SemanticType::Bool
                        } else {
                            self.add_error(SimpleTypeMismatch(resolved_rhs.get_type(), resolved_lhs.get_type()));
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
                let resolved_type = resolved_expr.get_type();
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
            LValue::Ident(name) => LValue::Ident(self.fold_name_sn(name)),
            LValue::ArrayElem(elem, _ty) => {
                let resolved_type = lookup_symbol_table(&elem.array_name.inner());
                LValue::ArrayElem(self.fold_array_elem(elem), resolved_type)
            }
            LValue::PairElem(elem, ty) => {
                // TODO: Get type of PairElem?
                // let resolved_inner = self.fold_pair_elem_sn(elem.clone());
                //
                // // Get the pair type
                // let resolved_type = match resolved_inner.clone().map_inner(|x| x.get_type()) {
                //     SemanticType::Pair(fst, snd) => {
                //         match elem.inner() {
                //             PairElem::Fst(_) => *fst,  // Extract first element type
                //             PairElem::Snd(_) => *snd,  // Extract second element type
                //         }
                //     }
                //     _ => SemanticType::Error(resolved_inner.span()), // Handle incorrect pair usage
                // };
                //
                // LValue::PairElem(resolved_inner, resolved_type)
                LValue::PairElem(self.fold_pair_elem_sn(elem), self.fold_type(ty))
            }
        }
    }

    #[inline]
    fn fold_rvalue(
        &mut self,
        rvalue: RValue<Self::N, Self::T>,
    ) -> RValue<Self::OutputN, Self::OutputT> {
        match rvalue {
            RValue::Expr(expr, ty) => {
                RValue::Expr(expr.map_inner(|inner| self.fold_expr(inner)), self.fold_type(ty))
            },
            RValue::ArrayLiter(exprs, ty) => RValue::ArrayLiter(
                exprs.fold_with(|expr| self.fold_expr_sn(expr)),
                self.fold_type(ty),
            ),
            RValue::NewPair(fst, snd, ty) => RValue::NewPair(
                self.fold_expr_sn(fst),
                self.fold_expr_sn(snd),
                self.fold_type(ty),
            ),
            RValue::PairElem(pair_elem) => RValue::PairElem(self.fold_pair_elem(pair_elem)),
            RValue::Call {
                func_name,
                args,
                return_type,
            } => {
                let resolved_return_type = lookup_func_return_type(&*func_name.clone());
                let expected_args_types = lookup_func_args(&*func_name.clone());
                let resolved_args = args.fold_with(|arg| self.fold_expr_sn(arg));
                if expected_args_types.len() != resolved_args.len() {
                    self.add_error(SemanticError::MismatchedArgCount(expected_args_types.len(), resolved_args.len()));
                }
                RValue::Call {
                    func_name: self.fold_name_sn(func_name),
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
        let given_type = lookup_symbol_table(&name.inner());
        let resolved_rvalue = self.fold_rvalue(rvalue);
        let resolved_type = resolved_rvalue.get_type();
        if given_type != resolved_type {
            self.add_error(SimpleTypeMismatch(given_type, resolved_type))
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
                if resolved_rvalue.get_type() != resolved_lvalue.get_type() {
                    self.add_error(SimpleTypeMismatch(resolved_rvalue.get_type(), resolved_lvalue.get_type()))
                }
                Stat::Assignment {
                    lvalue: resolved_lvalue,
                    rvalue: resolved_rvalue,
                }
            },
            Stat::Read(lvalue) => Stat::Read(self.fold_lvalue(lvalue)),
            Stat::Free(expr) => Stat::Free(self.fold_expr_sn(expr)),
            Stat::Return(expr) => Stat::Return(self.fold_expr_sn(expr)),
            Stat::Exit(expr) => Stat::Exit(self.fold_expr_sn(expr)),
            Stat::Print(expr) => Stat::Print(self.fold_expr_sn(expr)),
            Stat::Println(expr) => Stat::Println(self.fold_expr_sn(expr)),
            Stat::IfThenElse {
                if_cond,
                then_body,
                else_body,
            } => Stat::IfThenElse {
                if_cond: self.fold_expr_sn(if_cond),
                then_body: self.fold_stat_block_sn(then_body),
                else_body: self.fold_stat_block_sn(else_body),
            },
            Stat::WhileDo { while_cond, body } => Stat::WhileDo {
                while_cond: self.fold_expr_sn(while_cond),
                body: self.fold_stat_block_sn(body),
            },
            Stat::Scoped(body) => Stat::Scoped(self.fold_stat_block_sn(body)),
        }
    }
}
