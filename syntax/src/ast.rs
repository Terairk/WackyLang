#![allow(clippy::arbitrary_source_item_ordering)]

use crate::nonempty::NonemptyArray;
use crate::source::{SourcedNode, SourcedSpan};
use delegate::delegate;
use internment::ArcIntern;
use std::{fmt, fmt::Debug, ops::Deref};
use thiserror::Error;

/// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;

#[derive(Clone, Debug)]
pub struct Program {
    pub funcs: Box<[Func]>,
    pub body: SN<StatBlock>,
}

impl Program {
    #[must_use]
    #[inline]
    pub const fn new(funcs: Box<[Func]>, body: SN<StatBlock>) -> Self {
        Self { funcs, body }
    }
}

#[derive(Clone, Debug)]
pub struct Func {
    pub return_type: SN<Type>,
    pub name: SN<Ident>,
    pub params: Box<[FuncParam]>,
    pub body: SN<StatBlock>,
}

impl Func {
    #[must_use]
    #[inline]
    pub const fn new(
        return_type: SN<Type>,
        name: SN<Ident>,
        params: Box<[FuncParam]>,
        body: SN<StatBlock>,
    ) -> Self {
        Self {
            return_type,
            name,
            params,
            body,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FuncParam {
    pub r#type: SN<Type>,
    pub name: SN<Ident>,
}

impl FuncParam {
    #[must_use]
    #[inline]
    pub const fn new(r#type: SN<Type>, name: SN<Ident>) -> Self {
        Self { r#type, name }
    }
}

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct StatBlock(NonemptyArray<SN<Stat>>);

#[derive(Error, Debug)]
#[error("Cannot create to `StatBlock` because the supplied `Vec<Stat>` is empty")]
pub struct EmptyStatVecError;

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

    /// A block of statements is called ‘returning’ if the last statement in the block is either:
    ///
    ///     1. a ‘return’ statement
    ///     2. an ‘exit’ statement
    ///     3. an ‘if’ statement with two returning blocks.
    ///
    /// All function bodies **MUST** be returning blocks.
    pub fn is_return_block(&self) -> bool {
        match &**self.last() {
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

#[derive(Clone, Debug)]
pub enum Stat {
    Skip,
    VarDefinition {
        r#type: SN<Type>,
        name: SN<Ident>,
        rvalue: RValue,
    },
    Assignment {
        lvalue: LValue,
        rvalue: RValue,
    },
    Read(LValue),
    Free(SN<Expr>),
    Return(SN<Expr>),
    Exit(SN<Expr>),
    Print(SN<Expr>),
    Println(SN<Expr>),
    IfThenElse {
        if_cond: SN<Expr>,
        then_body: SN<StatBlock>,
        else_body: SN<StatBlock>,
    },
    WhileDo {
        while_cond: SN<Expr>,
        body: SN<StatBlock>,
    },
    Scoped(SN<StatBlock>),
}

impl Stat {
    #[must_use]
    #[inline]
    pub const fn var_definition(r#type: SN<Type>, name: SN<Ident>, rvalue: RValue) -> Self {
        Self::VarDefinition {
            r#type,
            name,
            rvalue,
        }
    }

    #[must_use]
    #[inline]
    pub const fn assignment(lvalue: LValue, rvalue: RValue) -> Self {
        Self::Assignment { lvalue, rvalue }
    }

    #[must_use]
    #[inline]
    pub const fn if_then_else(
        if_cond: SN<Expr>,
        then_body: SN<StatBlock>,
        else_body: SN<StatBlock>,
    ) -> Self {
        Self::IfThenElse {
            if_cond,
            then_body,
            else_body,
        }
    }

    #[must_use]
    #[inline]
    pub const fn while_do(while_cond: SN<Expr>, body: SN<StatBlock>) -> Self {
        Self::WhileDo { while_cond, body }
    }
}

#[derive(Clone, Debug)]
pub enum LValue {
    Ident(SN<Ident>),
    ArrayElem(ArrayElem),
    PairElem(SN<PairElem>),
}

#[derive(Clone, Debug)]
pub enum RValue {
    Expr(SN<Expr>),
    ArrayLiter(Box<[SN<Expr>]>),
    NewPair(SN<Expr>, SN<Expr>),
    PairElem(PairElem),
    Call {
        func_name: SN<Ident>,
        args: Box<[SN<Expr>]>,
    },
}

impl RValue {
    #[must_use]
    #[inline]
    pub const fn call(func_name: SN<Ident>, args: Box<[SN<Expr>]>) -> Self {
        Self::Call { func_name, args }
    }
}

#[derive(Clone, Debug)]
pub enum PairElem {
    Fst(SN<LValue>),
    Snd(SN<LValue>),
}

#[derive(Clone, Debug)]
pub enum Type {
    BaseType(SN<BaseType>),
    ArrayType(SN<ArrayType>),
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

impl ArrayType {
    #[must_use]
    #[inline]
    pub const fn new(elem_type: Type) -> Self {
        Self { elem_type }
    }
}

#[derive(Clone, Debug)]
pub enum PairElemType {
    BaseType(SN<BaseType>),
    ArrayType(SN<ArrayType>),
    Pair(SourcedSpan),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Liter(Liter),
    Ident(Ident),
    ArrayElem(ArrayElem),
    Unary(SN<UnaryOper>, SN<Self>),
    Binary(SN<Self>, SN<BinaryOper>, SN<Self>),
    Paren(SN<Self>),

    // Generated only by parser errors.
    Error(SourcedSpan),
}

#[derive(Clone, Debug)]
pub enum Liter {
    IntLiter(i32),
    BoolLiter(bool),
    CharLiter(char),
    StrLiter(ArcIntern<str>),
    PairLiter,
}

#[derive(Clone, Debug)]
pub enum UnaryOper {
    Not,
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
    And,
    Or,
}

impl BinaryOper {
    /// The precedence of binary operators in WACC, where lower
    /// is higher. Source: WACC-language spec, Table 4.
    #[must_use]
    #[inline]
    pub const fn precedence(&self) -> u8 {
        match *self {
            Self::Mul | Self::Div | Self::Mod => 1,
            Self::Add | Self::Sub => 2,
            Self::Lte | Self::Lt | Self::Gte | Self::Gt => 3,
            Self::Eq | Self::Neq => 4,
            Self::And => 5,
            Self::Or => 6,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident(ArcIntern<str>);

impl Ident {
    #[allow(clippy::should_implement_trait)]
    #[must_use]
    #[inline]
    pub fn from_str(s: &str) -> Self {
        Self(ArcIntern::from(s))
    }

    #[must_use]
    #[inline]
    pub fn from_boxed_str(s: Box<str>) -> Self {
        Self(ArcIntern::from(s))
    }

    #[must_use]
    #[inline]
    pub fn from_string(s: String) -> Self {
        Self::from_boxed_str(s.into_boxed_str())
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

#[derive(Clone, Debug)]
pub struct ArrayElem {
    pub array_name: SN<Ident>,
    pub indices: NonemptyArray<SN<Expr>>,
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
