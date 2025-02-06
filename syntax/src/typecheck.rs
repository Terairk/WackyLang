use crate::ast;
use crate::ast::{Expr, Ident};
use crate::ast::Liter;
use crate::ast::UnaryOper;
use crate::ast::BinaryOper;
use crate::ast::Program;
use crate::source::{SourcedNode, SourcedSpan};
use crate::types::SemanticType;

// Resolving types in AST
mod sealed {
    pub trait IdRepr {}
    pub trait TyRepr {}
}

#[repr(transparent)]
struct BeforeRenaming(String);

impl sealed::IdRepr for BeforeRenaming {}

#[repr(transparent)]
struct AfterRenaming(usize);

impl sealed::IdRepr for AfterRenaming {}

struct UnresolvedType;

impl sealed::TyRepr for UnresolvedType {}

impl sealed::TyRepr for SemanticType {}

// enum Expr<Id: sealed::IdRepr, Ty: sealed::TyRepr> {
//     Ident { name: Id },
//     IntLiteral(i32),
//     Add(TypedExpr<Id, Ty>, TypedExpr<Id, Ty>),
//     Liter,
// }
//
// struct TypedExpr<Id: sealed::IdRepr, Ty: sealed::TyRepr> {
//     ty: Ty,
//     expr: SourcedNode<Expr<Id, Ty>>,
// }

// STATE TRANSITION: BeforeRenaming -> AfterRenaming
// + other shared BeforeRenaming code
impl<Ty: sealed::TyRepr> Expr<BeforeRenaming, Ty> {
    pub fn do_renaming(self) -> Expr<AfterRenaming, Ty> { todo!() }
}
// impl<Ty: sealed::TyRepr> TypedExpr<BeforeRenaming, Ty> {
//     pub fn do_renaming(self) -> TypedExpr<AfterRenaming, Ty> { todo!() }
// }

// STATE TRANSITION: UnresolvedType -> SemanticType
// + other shared UnresolvedType code

// Some kind of function to look up symbol table
pub fn lookup_symbol_table(ident: &ast::Ident) -> SemanticType {
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
impl<Id: sealed::IdRepr> Expr<Id, ()> {
    pub fn resolve_type(self) -> Expr<Id, SemanticType> {
        match self {
            // Resolve literals to their corresponding types
            Expr::Liter(literal, _) => {
                let resolved_type = match literal {
                    Liter::IntLiter(_) => SemanticType::Int,
                    Liter::BoolLiter(_) => SemanticType::Bool,
                    Liter::CharLiter(_) => SemanticType::Char,
                    Liter::StrLiter(_) => SemanticType::String,
                    Liter::PairLiter => {
                        let fst = SemanticType::Int; // Type of the first elem of pair
                        let snd = SemanticType::Int; // Type of the second elem of pair
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

            // Preserve errors
            Expr::Error(span) => Expr::Error(span),

            // TODO: ArrayELem case
            // Temp solution for compiler, remove when all cases are matched correctly
            _ => { unreachable!() }
        }
    }
}

impl<Id> Program<Id, UnresolvedType> {
    pub fn resolve_type(self) -> Program<Id, SemanticType> {
        todo!()
        // Program {
        //     funcs: self.funcs.into_iter().map(|f| f.resolve_type()).collect(),
        //     body: self.body.map_inner(|stat| stat.resolve_type()),
        // }
    }
}


// impl<Id: sealed::IdRepr> TypedExpr<Id, UnresolvedType> {
//     pub fn resolve_type(self) -> TypedExpr<Id, SemanticType> {
//         todo!()
//     }
// }

// OR MAYBE YOU CAN COMBINE THE STATE TRANSITIONS IF YOU WANNA
// who knows whats good???

// SHARED LOGIC:
// impl<Id: sealed::IdRepr, Ty: sealed::TyRepr> Expr<Id, Ty> {
//     pub fn this(self) -> bool { todo!() }
// }
// impl<Id: sealed::IdRepr, Ty: sealed::TyRepr> TypedExpr<Id, Ty> {
//     pub fn that(self) -> bool { todo!() }
// }