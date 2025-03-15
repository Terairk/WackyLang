use crate::parsing::ast;
use crate::wacc_hir::hir;
pub use crate::wacc_thir::types::Type;
use thiserror::Error;
use util::nonempty::NonemptyArray;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Program {
    pub funcs: Box<[Func]>,
    pub body: StatBlock,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Func {
    pub return_type: Type,
    pub name: ast::Ident,
    pub params: Box<[Ident]>,
    pub body: StatBlock,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
#[repr(transparent)]
pub struct StatBlock(pub NonemptyArray<Stat>);

#[derive(Error, Debug)]
#[error("Cannot create to `StatBlock` because the supplied `Vec<Stat>` is empty")]
pub struct EmptyStatVecError;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
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
    NextLoop(hir::LoopLabel),
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

pub trait RenameIdent {
    /// Renames all occurrences of an identifier within this item, based on a renaming function.
    ///
    /// # Panics
    /// If the types of the old and new identifiers mismatch, the function will panic.
    #[must_use]
    fn rename_ident<R>(self, renamer: &mut R) -> Self
    where
        Self: Sized,
        R: FnMut(Ident) -> Option<Ident>;
}

// all implementation blocks live here
mod impls {
    use crate::parsing::ast;
    use crate::wacc_thir::thir::{
        ArrayElem, ArrayLiter, BinaryExpr, EmptyStatVecError, Expr, Func, Ident, LValue, Liter,
        NewPair, PairElem, Program, RValue, RenameIdent, Stat, StatBlock, UnaryExpr,
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

    impl RenameIdent for Expr {
        #[inline]
        fn rename_ident<R>(self, renamer: &mut R) -> Self
        where
            Self: Sized,
            R: FnMut(Ident) -> Option<Ident>,
        {
            match self {
                Expr::Liter(_) => self,
                Expr::Ident(i) => Expr::Ident(i.rename_ident(renamer)),
                Expr::ArrayElem(a) => Expr::ArrayElem(Box::new((*a).rename_ident(renamer))),
                Expr::Unary(e) => Expr::Unary(Box::new((*e).rename_ident(renamer))),
                Expr::Binary(e) => Expr::Binary(Box::new((*e).rename_ident(renamer))),
                Expr::IfThenElse {
                    if_cond,
                    then_val,
                    else_val,
                    r#type,
                } => Expr::IfThenElse {
                    if_cond: Box::new((*if_cond).rename_ident(renamer)),
                    then_val: Box::new((*then_val).rename_ident(renamer)),
                    else_val: Box::new((*else_val).rename_ident(renamer)),
                    r#type,
                },
            }
        }
    }

    impl RenameIdent for UnaryExpr {
        #[inline]
        fn rename_ident<R>(self, renamer: &mut R) -> Self
        where
            Self: Sized,
            R: FnMut(Ident) -> Option<Ident>,
        {
            let UnaryExpr {
                expr: (op, expr),
                r#type,
            } = self;
            UnaryExpr {
                expr: (op, expr.rename_ident(renamer)),
                r#type,
            }
        }
    }

    impl RenameIdent for BinaryExpr {
        #[inline]
        fn rename_ident<R>(self, renamer: &mut R) -> Self
        where
            Self: Sized,
            R: FnMut(Ident) -> Option<Ident>,
        {
            let BinaryExpr {
                expr: (l, op, r),
                r#type,
            } = self;
            BinaryExpr {
                expr: (l.rename_ident(renamer), op, r.rename_ident(renamer)),
                r#type,
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
        pub fn singleton(stat: Stat) -> Self {
            Self(NonemptyArray::singleton(stat))
        }

        #[allow(clippy::missing_errors_doc)]
        #[inline]
        pub fn try_new(stats: Vec<Stat>) -> Result<Self, EmptyStatVecError> {
            NonemptyArray::try_from_boxed_slice(stats)
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

        /// Checks if all calls to a specified function are  tailcalls.
        /// A tailcalls are defined as:
        /// 1) A call to the specified function is assigned to a new variable
        /// 2) The very next statement is a return statement that returns _precisely_ this new variable
        #[must_use]
        #[inline]
        pub fn all_calls_are_tail_calls(&self, func_name: &ast::Ident) -> bool {
            for (i, s) in self.0.iter().enumerate() {
                match *s {
                    // All tail-calls must be assigning variable-definitions, since OTHER assignment
                    // may lead to stateful side effects (e.g. assigning to array-element), and
                    // optimizing away such stateful side effects (tail-call optimizations) will alter
                    // semantics of the program
                    Stat::Assignment {
                        rvalue:
                            RValue::Call {
                                func_name: ref rvalue_func_name,
                                ..
                            },
                        ..
                    } => {
                        if func_name == rvalue_func_name {
                            return false;
                        }
                    }

                    // All tail-call variable-definitions must happen as the penultimate statement
                    // of the block - before a final "return" statement that contains exactly the
                    // identifier of the variable that was just assigned to
                    Stat::VarDefinition {
                        ref name,
                        rvalue:
                            RValue::Call {
                                func_name: ref rvalue_func_name,
                                ..
                            },
                        ..
                    } => {
                        // we only care about tail-calls of the specified function
                        if func_name != rvalue_func_name {
                            continue;
                        }

                        // if this is the last statement of the block, its not a tail-call
                        let Some(next_stat) = &self.0.get(i + 1) else {
                            return false;
                        };

                        // if the next statement is not return-statement, its not a tail-call
                        let Stat::Return(return_val) = next_stat else {
                            return false;
                        };

                        // if the return-value isn't PRECISELY the identifier just defined, then
                        // it isn't a tail-call
                        if return_val != &Expr::Ident(name.clone()) {
                            return false;
                        }
                    }

                    // apply the same rules recursively to any inner-statement blocks
                    Stat::IfThenElse {
                        ref then_body,
                        ref else_body,
                        ..
                    } => {
                        if !then_body.all_calls_are_tail_calls(func_name)
                            || !else_body.all_calls_are_tail_calls(func_name)
                        {
                            return false;
                        }
                    }
                    Stat::LoopDo { ref body, .. } => {
                        if !body.all_calls_are_tail_calls(func_name) {
                            return false;
                        }
                    }

                    _ => {}
                }
            }

            // if we checked all (possibly-recursively) statements, and all calls are tail-calls
            // then return true to indicate this
            true
        }

        #[inline]
        pub fn any<F: FnMut(&Stat) -> bool>(&self, f: F) -> bool {
            self.0.iter().any(f)
        }

        #[inline]
        pub fn all<F: FnMut(&Stat) -> bool>(&self, f: F) -> bool {
            self.0.iter().all(f)
        }

        /// Checks that the conditional `P => Q` holds on statements in this statement block.
        #[inline]
        pub fn forall_if_then<P: FnMut(&Stat) -> bool, Q: FnMut(&Stat) -> bool>(
            &self,
            mut if_pred: P,
            then_pred: Q,
        ) -> bool {
            self.0.iter().filter(|s| if_pred(s)).all(then_pred)
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

        /// Check if this statement contains a call to the specified function.
        #[inline]
        #[must_use]
        pub fn has_call_to_func(&self, func_name: &ast::Ident) -> bool {
            match *self {
                Self::VarDefinition {
                    rvalue:
                        RValue::Call {
                            func_name: ref call_func_name,
                            ..
                        },
                    ..
                }
                | Self::Assignment {
                    rvalue:
                        RValue::Call {
                            func_name: ref call_func_name,
                            ..
                        },
                    ..
                } => call_func_name == func_name,
                Self::IfThenElse {
                    ref then_body,
                    ref else_body,
                    ..
                } => {
                    then_body.any(|s| s.has_call_to_func(func_name))
                        || else_body.any(|s| s.has_call_to_func(func_name))
                }
                Self::LoopDo { ref body, .. } => body.any(|s| s.has_call_to_func(func_name)),
                _ => false,
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

    impl RenameIdent for ArrayElem {
        #[inline]
        fn rename_ident<R>(self, renamer: &mut R) -> Self
        where
            Self: Sized,
            R: FnMut(Ident) -> Option<Ident>,
        {
            match self {
                ArrayElem::FirstAccess {
                    array_name,
                    index,
                    r#type,
                } => ArrayElem::FirstAccess {
                    array_name: array_name.rename_ident(renamer),
                    index: index.rename_ident(renamer),
                    r#type,
                },
                ArrayElem::NestedAccess {
                    array_elem,
                    index,
                    r#type,
                } => ArrayElem::NestedAccess {
                    array_elem: Box::new((*array_elem).rename_ident(renamer)),
                    index: index.rename_ident(renamer),
                    r#type,
                },
            }
        }
    }

    impl RenameIdent for Ident {
        #[inline]
        fn rename_ident<R>(self, renamer: &mut R) -> Self
        where
            Self: Sized,
            R: FnMut(Ident) -> Option<Ident>,
        {
            match renamer(self.clone()) {
                None => self,
                Some(new) => {
                    assert_eq!(
                        &self.r#type, &new.r#type,
                        "replacement identifier types should never mismatch"
                    );
                    new
                }
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
