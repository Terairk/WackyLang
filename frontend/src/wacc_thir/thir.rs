use crate::parsing::ast;
use crate::wacc_hir::hir;
pub use crate::wacc_thir::types::Type;
use thiserror::Error;
use util::nonempty::NonemptyArray;

#[derive(Clone, Debug)]
pub struct Program {
    pub funcs: Box<[Func]>,
    pub body: StatBlock,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub return_type: Type,
    pub name: ast::Ident,
    pub params: Box<[Ident]>,
    pub body: StatBlock,
}

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct StatBlock(pub NonemptyArray<Stat>);

#[derive(Error, Debug)]
#[error("Cannot create to `StatBlock` because the supplied `Vec<Stat>` is empty")]
pub struct EmptyStatVecError;

#[derive(Clone, Debug)]
pub enum Stat {
    Skip,
    VarDefinition {
        name: Ident,
        rvalue: RValue,
    },
    Assignment {
        lvalue: LValue,
        rvalue: RValue,
    },
    Read(LValue),
    Free(Expr),
    Return(Expr),
    Exit(Expr),
    Print(Expr),
    Println(Expr),
    IfThenElse {
        if_cond: Expr,
        then_body: StatBlock,
        else_body: StatBlock,
    },
    LoopDo {
        label: hir::LoopLabel,
        body: StatBlock,
    },
    Break(hir::LoopLabel),
    Continue(hir::LoopLabel),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum LValue {
    Ident(Ident),
    ArrayElem(ArrayElem),
    PairElem(PairElem),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum RValue {
    Expr(Expr),
    ArrayLiter(ArrayLiter),
    NewPair(NewPair),
    PairElem(PairElem),
    Call {
        return_type: Type,
        func_name: ast::Ident,
        args: Box<[Expr]>,
    },
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ArrayLiter {
    pub liter_values: Box<[Expr]>,
    pub r#type: Type,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct NewPair {
    pub fst: Expr,
    pub snd: Expr,
    pub r#type: Type,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct PairElem {
    pub pair_elem: (hir::PairElemSelector, Box<LValue>),
    pub r#type: Type,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ArrayElem {
    FirstAccess {
        array_name: Ident,
        index: Expr,
        r#type: Type,
    },
    NestedAccess {
        array_elem: Box<ArrayElem>,
        index: Expr,
        r#type: Type,
    },
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Expr {
    Liter(Liter),
    Ident(Ident),
    ArrayElem(Box<ArrayElem>),
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    IfThenElse {
        if_cond: Box<Self>,
        then_val: Box<Self>,
        else_val: Box<Self>,
        r#type: Type,
    },
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
#[allow(clippy::enum_variant_names)]
pub struct Liter {
    pub liter: hir::Liter,
    pub r#type: Type,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct UnaryExpr {
    pub expr: (hir::UnaryOper, Expr),
    pub r#type: Type,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct BinaryExpr {
    pub expr: (Expr, hir::BinaryOper, Expr),
    pub r#type: Type,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub ident: hir::Ident,
    pub r#type: Type,
}

// all implementation blocks live here
mod impls {
    use crate::parsing::ast;
    use crate::wacc_thir::thir::{
        ArrayElem, ArrayLiter, BinaryExpr, EmptyStatVecError, Expr, Func, Ident, LValue, Liter,
        NewPair, PairElem, Program, RValue, Stat, StatBlock, UnaryExpr,
    };
    use crate::wacc_thir::types::Type;
    use delegate::delegate;
    use std::fmt;
    use util::nonempty::NonemptyArray;

    impl Expr {
        #[must_use]
        #[inline]
        pub const fn if_then_else(
            if_cond: Box<Self>,
            then_val: Box<Self>,
            else_val: Box<Self>,
            r#type: Type,
        ) -> Self {
            Self::IfThenElse {
                if_cond,
                then_val,
                else_val,
                r#type,
            }
        }

        #[must_use]
        #[inline]
        pub fn r#type(&self) -> Type {
            match *self {
                Self::Liter(Liter { ref r#type, .. })
                | Self::Ident(Ident { ref r#type, .. })
                | Self::IfThenElse { ref r#type, .. } => r#type.clone(),
                Self::ArrayElem(ref boxed) => (&*boxed).r#type(),
                Self::Unary(ref boxed) => (&*boxed).r#type.clone(),
                Self::Binary(ref boxed) => (&*boxed).r#type.clone(),
            }
        }

        #[must_use]
        #[inline]
        pub fn map_type<F>(self, f: F) -> Self
        where
            F: FnOnce(&Self) -> Type,
        {
            let new_type = f(&self);
            match self {
                Expr::Liter(Liter { liter, .. }) => Expr::Liter(Liter {
                    liter,
                    r#type: new_type,
                }),
                Expr::Ident(Ident { ident, .. }) => Expr::Ident(Ident {
                    ident,
                    r#type: new_type,
                }),
                Expr::ArrayElem(a) => Expr::ArrayElem(Box::new((*a).map_type(|_| new_type))),
                Expr::Unary(boxed) => {
                    let UnaryExpr { expr, .. } = *boxed;
                    Expr::Unary(Box::new(UnaryExpr {
                        expr,
                        r#type: new_type,
                    }))
                }
                Expr::Binary(boxed) => {
                    let BinaryExpr { expr, .. } = *boxed;
                    Expr::Binary(Box::new(BinaryExpr {
                        expr,
                        r#type: new_type,
                    }))
                }
                Expr::IfThenElse {
                    if_cond,
                    then_val,
                    else_val,
                    ..
                } => Expr::IfThenElse {
                    if_cond,
                    then_val,
                    else_val,
                    r#type: new_type,
                },
            }
        }
    }

    impl Program {
        #[must_use]
        #[inline]
        pub const fn new(funcs: Box<[Func]>, body: StatBlock) -> Self {
            Self { funcs, body }
        }
    }

    impl Func {
        #[must_use]
        #[inline]
        pub const fn new(
            return_type: Type,
            name: ast::Ident,
            params: Box<[Ident]>,
            body: StatBlock,
        ) -> Self {
            Self {
                return_type,
                name,
                params,
                body,
            }
        }
    }

    impl StatBlock {
        #[must_use]
        #[inline]
        pub fn singleton(spanned_stat: Stat) -> Self {
            Self(NonemptyArray::singleton(spanned_stat))
        }

        #[allow(clippy::missing_errors_doc)]
        #[inline]
        pub fn try_new(spanned_stats: Vec<Stat>) -> Result<Self, EmptyStatVecError> {
            NonemptyArray::try_from_boxed_slice(spanned_stats)
                .map(Self)
                .map_err(|_| EmptyStatVecError)
        }

        // A block of statements is called ‘returning’ if the last statement in the block is either:
        //
        //     1. a ‘return’ statement
        //     2. an ‘exit’ statement
        //     3. an ‘if’ statement with two returning blocks.
        //
        // All function bodies **MUST** be returning blocks.
        #[inline]
        pub fn is_return_block(&self) -> bool {
            match &*self.last() {
                Stat::Return(_) | Stat::Exit(_) => true,
                Stat::IfThenElse {
                    then_body,
                    else_body,
                    ..
                } => then_body.is_return_block() && else_body.is_return_block(),
                _ => false,
            }
        }

        delegate! {
            to self.0 {
                #[inline]
                pub fn first(&self) -> &Stat;
                #[inline]
                pub fn last(&self) -> &Stat;
            }
        }
    }

    impl From<Stat> for StatBlock {
        #[inline]
        fn from(spanned_stat: Stat) -> Self {
            Self::singleton(spanned_stat)
        }
    }
    impl TryFrom<Vec<Stat>> for StatBlock {
        type Error = EmptyStatVecError;

        #[inline]
        fn try_from(spanned_stats: Vec<Stat>) -> Result<Self, Self::Error> {
            Self::try_new(spanned_stats)
        }
    }

    impl Stat {
        #[must_use]
        #[inline]
        pub const fn var_definition(name: Ident, rvalue: RValue) -> Self {
            Self::VarDefinition { name, rvalue }
        }

        #[must_use]
        #[inline]
        pub const fn assignment(lvalue: LValue, rvalue: RValue) -> Self {
            Self::Assignment { lvalue, rvalue }
        }

        #[must_use]
        #[inline]
        pub const fn if_then_else(
            if_cond: Expr,
            then_body: StatBlock,
            else_body: StatBlock,
        ) -> Self {
            Self::IfThenElse {
                if_cond,
                then_body,
                else_body,
            }
        }
    }

    impl LValue {
        pub fn r#type(&self) -> Type {
            match *self {
                LValue::ArrayElem(ref elem) => elem.r#type(),
                LValue::Ident(Ident { ref r#type, .. })
                | LValue::PairElem(PairElem { ref r#type, .. }) => r#type.clone(),
            }
        }
    }

    impl RValue {
        #[must_use]
        #[inline]
        pub const fn call(return_type: Type, func_name: ast::Ident, args: Box<[Expr]>) -> Self {
            Self::Call {
                return_type,
                func_name,
                args,
            }
        }

        pub fn r#type(&self) -> Type {
            match *self {
                RValue::Expr(ref e) => e.r#type(),
                RValue::PairElem(PairElem { ref r#type, .. })
                | RValue::ArrayLiter(ArrayLiter { ref r#type, .. })
                | RValue::NewPair(NewPair { ref r#type, .. })
                | RValue::Call {
                    return_type: ref r#type,
                    ..
                } => r#type.clone(),
            }
        }
    }

    impl ArrayElem {
        #[must_use]
        #[inline]
        pub const fn first_access(array_name: Ident, index: Expr, r#type: Type) -> Self {
            Self::FirstAccess {
                array_name,
                index,
                r#type,
            }
        }

        #[must_use]
        #[inline]
        pub const fn nested_access(array_elem: Box<Self>, index: Expr, r#type: Type) -> Self {
            Self::NestedAccess {
                array_elem,
                index,
                r#type,
            }
        }

        #[inline]
        #[must_use]
        pub fn r#type(&self) -> Type {
            match *self {
                Self::FirstAccess { ref r#type, .. } | Self::NestedAccess { ref r#type, .. } => {
                    r#type.clone()
                }
            }
        }

        #[must_use]
        #[inline]
        pub fn map_type<F>(self, f: F) -> Self
        where
            F: FnOnce(&Self) -> Type,
        {
            let new_type = f(&self);
            match self {
                ArrayElem::FirstAccess {
                    array_name, index, ..
                } => ArrayElem::FirstAccess {
                    array_name,
                    index,
                    r#type: new_type,
                },
                ArrayElem::NestedAccess {
                    array_elem, index, ..
                } => ArrayElem::NestedAccess {
                    array_elem,
                    index,
                    r#type: new_type,
                },
            }
        }
    }

    // Customised debug/display for prettier debugging
    impl fmt::Debug for Ident {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "<{:?}:{:?}>", self.ident, self.r#type)
        }
    }
    impl fmt::Display for Ident {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "<{:?}:{:?}>", self.ident, self.r#type)
        }
    }
}
