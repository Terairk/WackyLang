#![allow(clippy::arbitrary_source_item_ordering)]

use crate::alias::InternStr;
use crate::source::{SourcedBoxedNode, SourcedNode, SourcedSpan};
use std::fmt::Debug;
use thiserror::Error;
use util::nonempty::NonemptyArray;

// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;
type SBN<T> = SourcedBoxedNode<T>;

#[derive(Clone, Debug)]
pub struct Program {
    pub funcs: Box<[Func]>,
    pub body: StatBlock,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub is_tailrec: bool,
    pub return_type: SN<Type>,
    pub name: SN<Ident>,
    pub params: Box<[FuncParam]>,
    pub body: StatBlock,
}

#[derive(Clone, Debug)]
pub struct FuncParam {
    pub r#type: SN<Type>,
    pub name: SN<Ident>,
}

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct StatBlock(pub NonemptyArray<SN<Stat>>);

#[derive(Error, Debug)]
#[error("Cannot create to `StatBlock` because the supplied `Vec<Stat>` is empty")]
pub struct EmptyStatVecError;

#[derive(Clone, Debug)]
pub enum Stat {
    Skip,
    VarDefinition {
        r#type: SN<Type>,
        name: SN<Ident>,
        rvalue: SN<RValue>,
    },
    Assignment {
        lvalue: SN<LValue>,
        rvalue: SN<RValue>,
    },
    Read(SN<LValue>),
    Free(SN<Expr>),
    Return(SN<Expr>),
    Exit(SN<Expr>),
    Print(SN<Expr>),
    Println(SN<Expr>),
    Scoped(StatBlock),
    IfElifOnly {
        r#if: ConditionalStatFragment,
        elifs: Vec<ConditionalStatFragment>,
    },
    IfElifElse {
        r#if: ConditionalStatFragment,
        elifs: Vec<ConditionalStatFragment>,
        else_body: StatBlock,
    },
    WhileDo {
        label: Option<SN<Ident>>,
        while_cond: SN<Expr>,
        body: StatBlock,
    },
    DoWhile {
        label: Option<SN<Ident>>,
        body: StatBlock,
        while_cond: SN<Expr>,
    },
    LoopDo {
        label: Option<SN<Ident>>,
        body: StatBlock,
    },
    Break(Option<SN<Ident>>),
    NextLoop(Option<SN<Ident>>),
}

#[derive(Clone, Debug)]
pub struct ConditionalStatFragment {
    pub cond: SN<Expr>,
    pub body: StatBlock,
}

#[derive(Clone, Debug)]
pub enum LValue {
    Ident(SN<Ident>),
    ArrayElem(SN<ArrayElem>),
    PairElem(SN<PairElem>),
}

#[derive(Clone, Debug)]
pub enum RValue {
    Expr(SN<Expr>),
    ArrayLiter(Box<[SN<Expr>]>),
    NewPair(SN<Expr>, SN<Expr>),
    PairElem(SN<PairElem>),
    Call {
        func_name: SN<Ident>,
        args: Box<[SN<Expr>]>,
    },
}

#[derive(Clone, Debug)]
#[repr(u8)]
pub enum PairElemSelector {
    Fst,
    Snd,
}

#[derive(Clone, Debug)]
pub struct PairElem(pub PairElemSelector, pub SBN<LValue>);

#[derive(Clone, Debug)]
pub struct ArrayElem {
    pub array_name: SN<Ident>,
    pub indices: NonemptyArray<SN<Expr>>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Liter(SN<Liter>),
    Ident(SN<Ident>),
    ArrayElem(SBN<ArrayElem>),
    Unary(SN<UnaryOper>, SBN<Self>),
    Binary(SBN<Self>, SN<BinaryOper>, SBN<Self>),
    Paren(SBN<Self>),
    IfElifElse {
        r#if: ConditionalExprFragment,
        elifs: Vec<ConditionalExprFragment>,
        else_val: SBN<Self>,
    },

    // Generated only by parser errors.
    Error(SourcedSpan),
}

#[derive(Clone, Debug)]
pub struct ConditionalExprFragment {
    pub cond: SBN<Expr>,
    pub val: SBN<Expr>,
}

#[derive(Clone, Debug)]
pub enum Liter {
    IntLiter(i32),
    BoolLiter(bool),
    CharLiter(char),
    StrLiter(InternStr),
    PairLiter,
}

#[derive(Clone, Debug)]
pub enum UnaryOper {
    LNot,
    Minus,
    Len,
    Ord,
    Chr,
}

#[derive(Clone, Debug)]
pub enum BinaryOper {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Lte,
    Lt,
    Gte,
    Gt,
    Eq,
    Neq,
    LAnd,
    LOr,
}

#[derive(Clone, Debug)]
pub enum Type {
    BaseType(SN<BaseType>),
    ArrayType(SBN<ArrayType>),
    PairType(PairElemType, PairElemType),

    // Generated only by parser errors.
    Error(SourcedSpan),
}

#[derive(Clone, Debug)]
pub enum BaseType {
    Int,
    Bool,
    Char,
    String,
}

#[derive(Clone, Debug)]
pub struct ArrayType {
    pub elem_type: Type,
}

#[derive(Clone, Debug)]
pub enum PairElemType {
    ArrayType(SBN<ArrayType>),
    BaseType(SN<BaseType>),
    Pair(SourcedSpan),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Ident(InternStr);

// all implementation blocks live here
mod impls {
    use crate::alias::InternStr;
    use crate::parsing::ast::{
        ArrayElem, ArrayType, ConditionalExprFragment, ConditionalStatFragment, EmptyStatVecError,
        Expr, Func, FuncParam, Ident, LValue, Program, RValue, Stat, StatBlock, Type, SBN, SN,
    };
    use delegate::delegate;
    use std::fmt;
    use std::ops::Deref;
    use util::nonempty::NonemptyArray;

    impl Expr {
        #[must_use]
        #[inline]
        pub const fn if_elif_else(
            r#if: ConditionalExprFragment,
            elifs: Vec<ConditionalExprFragment>,
            else_val: SBN<Self>,
        ) -> Self {
            Self::IfElifElse {
                r#if,
                elifs,
                else_val,
            }
        }
    }

    impl ConditionalExprFragment {
        #[must_use]
        #[inline]
        pub const fn new(cond: SBN<Expr>, val: SBN<Expr>) -> Self {
            Self { cond, val }
        }
    }

    impl ArrayType {
        #[must_use]
        #[inline]
        pub const fn new(elem_type: Type) -> Self {
            Self { elem_type }
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
            is_tailrec: bool,
            return_type: SN<Type>,
            name: SN<Ident>,
            params: Box<[FuncParam]>,
            body: StatBlock,
        ) -> Self {
            Self {
                is_tailrec,
                return_type,
                name,
                params,
                body,
            }
        }
    }

    impl FuncParam {
        #[must_use]
        #[inline]
        pub const fn new(r#type: SN<Type>, name: SN<Ident>) -> Self {
            Self { r#type, name }
        }
    }

    impl StatBlock {
        #[must_use]
        #[inline]
        pub fn singleton(spanned_stat: SN<Stat>) -> Self {
            Self(NonemptyArray::singleton(spanned_stat))
        }

        #[allow(clippy::missing_errors_doc)]
        #[inline]
        pub fn try_new(spanned_stats: Vec<SN<Stat>>) -> Result<Self, EmptyStatVecError> {
            NonemptyArray::try_from_boxed_slice(spanned_stats)
                .map(Self)
                .map_err(|_| EmptyStatVecError)
        }

        // A block of statements is called ‘returning’ if the last statement in the block is either:
        //
        //     1. a ‘return’ statement
        //     2. an ‘exit’ statement
        //     3. an ‘if-(elif)-else’ statement with all returning blocks.
        //
        // All function bodies **MUST** be returning blocks.
        #[inline]
        pub fn is_return_block(&self) -> bool {
            match &**self.last() {
                Stat::Return(_) | Stat::Exit(_) => true,
                Stat::IfElifElse {
                    r#if,
                    elifs,
                    else_body,
                } => {
                    let if_is_return = r#if.body.is_return_block();
                    let elifs_are_return = elifs
                        .iter()
                        .fold(true, |accum, elif| accum && elif.body.is_return_block());
                    let else_is_return = r#else_body.is_return_block();

                    if_is_return && elifs_are_return && else_is_return
                }
                Stat::Scoped(stat_block) => stat_block.is_return_block(),
                _ => false,
            }
        }

        delegate! {
            to self.0 {
                #[inline]
                pub fn first(&self) -> &SN<Stat>;
                #[inline]
                pub fn last(&self) -> &SN<Stat>;
            }
        }
    }

    impl From<SN<Stat>> for StatBlock {
        #[inline]
        fn from(spanned_stat: SN<Stat>) -> Self {
            Self::singleton(spanned_stat)
        }
    }
    impl TryFrom<Vec<SN<Stat>>> for StatBlock {
        type Error = EmptyStatVecError;

        #[inline]
        fn try_from(spanned_stats: Vec<SN<Stat>>) -> Result<Self, Self::Error> {
            Self::try_new(spanned_stats)
        }
    }

    impl Stat {
        #[must_use]
        #[inline]
        pub const fn var_definition(r#type: SN<Type>, name: SN<Ident>, rvalue: SN<RValue>) -> Self {
            Self::VarDefinition {
                r#type,
                name,
                rvalue,
            }
        }

        #[must_use]
        #[inline]
        pub const fn assignment(lvalue: SN<LValue>, rvalue: SN<RValue>) -> Self {
            Self::Assignment { lvalue, rvalue }
        }

        #[must_use]
        #[inline]
        pub const fn if_elif_only(
            r#if: ConditionalStatFragment,
            elifs: Vec<ConditionalStatFragment>,
        ) -> Self {
            Self::IfElifOnly { r#if, elifs }
        }

        #[must_use]
        #[inline]
        pub const fn if_elif_else(
            r#if: ConditionalStatFragment,
            elifs: Vec<ConditionalStatFragment>,
            else_body: StatBlock,
        ) -> Self {
            Self::IfElifElse {
                r#if,
                elifs,
                else_body,
            }
        }

        #[must_use]
        #[inline]
        pub const fn while_do(
            label: Option<SN<Ident>>,
            while_cond: SN<Expr>,
            body: StatBlock,
        ) -> Self {
            Self::WhileDo {
                label,
                while_cond,
                body,
            }
        }

        #[must_use]
        #[inline]
        pub const fn do_while(
            label: Option<SN<Ident>>,
            body: StatBlock,
            while_cond: SN<Expr>,
        ) -> Self {
            Self::DoWhile {
                label,
                body,
                while_cond,
            }
        }

        #[must_use]
        #[inline]
        pub const fn loop_do(label: Option<SN<Ident>>, body: StatBlock) -> Self {
            Self::LoopDo { label, body }
        }
    }

    impl ConditionalStatFragment {
        #[must_use]
        #[inline]
        pub const fn new(cond: SN<Expr>, body: StatBlock) -> Self {
            Self { cond, body }
        }
    }

    impl RValue {
        #[must_use]
        #[inline]
        pub const fn call(func_name: SN<Ident>, args: Box<[SN<Expr>]>) -> Self {
            Self::Call { func_name, args }
        }
    }

    impl ArrayElem {
        #[must_use]
        #[inline]
        pub const fn new(array_name: SN<Ident>, indices: NonemptyArray<SN<Expr>>) -> Self {
            Self {
                array_name,
                indices,
            }
        }
    }

    impl Ident {
        #[allow(clippy::should_implement_trait)]
        #[must_use]
        #[inline]
        pub fn from_str(s: &str) -> Self {
            Self(InternStr::from(s))
        }

        #[must_use]
        #[inline]
        pub fn from_boxed_str(s: Box<str>) -> Self {
            Self(InternStr::from(s))
        }

        #[must_use]
        #[inline]
        pub fn from_string(s: String) -> Self {
            Self::from_boxed_str(s.into_boxed_str())
        }

        #[must_use]
        #[inline]
        pub const fn inner(&self) -> &InternStr {
            &self.0
        }

        #[must_use]
        #[inline]
        pub fn into_inner(self) -> InternStr {
            self.0
        }
    }

    impl fmt::Display for Ident {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            <str as fmt::Display>::fmt(&self.0, f)
        }
    }

    impl Deref for Ident {
        type Target = str;

        #[allow(clippy::explicit_deref_methods)]
        #[inline]
        fn deref(&self) -> &Self::Target {
            self.0.deref()
        }
    }

    impl From<Ident> for String {
        #[inline]
        fn from(ident: Ident) -> Self {
            ident.0.to_string()
        }
    }

    impl From<InternStr> for Ident {
        #[inline]
        fn from(str: InternStr) -> Self {
            Self(str)
        }
    }
}
