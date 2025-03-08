#![allow(clippy::arbitrary_source_item_ordering)]

use crate::alias::InternStr;
use crate::source::{SourcedNode, SourcedSpan};
use delegate::delegate;
use std::fmt::Debug;
use thiserror::Error;
use util::nonempty::NonemptyArray;

// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;

#[derive(Clone, Debug)]
pub struct Program {
    pub funcs: Box<[Func]>,
    pub body: StatBlock,
}

#[derive(Clone, Debug)]
pub struct Func {
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
    IfThenElse {
        if_cond: SN<Expr>,
        then_body: StatBlock,
        else_body: StatBlock,
    },
    WhileDo {
        while_cond: SN<Expr>,
        body: StatBlock,
    },
    Scoped(StatBlock),
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
pub struct PairElem(pub PairElemSelector, pub SN<LValue>);

#[derive(Clone, Debug)]
pub struct ArrayElem {
    pub array_name: SN<Ident>,
    pub indices: NonemptyArray<SN<Expr>>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Liter(SN<Liter>),
    Ident(SN<Ident>),
    ArrayElem(SN<ArrayElem>),
    Unary(SN<UnaryOper>, SN<Self>),
    Binary(SN<Self>, SN<BinaryOper>, SN<Self>),
    Paren(SN<Self>),
    IfThenElse {
        if_cond: SN<Expr>,
        then_val: SN<Expr>,
        else_val: SN<Expr>,
    },

    // Generated only by parser errors.
    Error(SourcedSpan),
}

impl Expr {
    #[must_use]
    #[inline]
    pub const fn if_then_else(if_cond: SN<Expr>, then_val: SN<Expr>, else_val: SN<Expr>) -> Self {
        Self::IfThenElse {
            if_cond,
            then_val,
            else_val,
        }
    }
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
    BNot,
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
    BAnd,
    BXor,
    BOr,
    LAnd,
    LOr,
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

#[derive(Clone, Debug)]
pub enum PairElemType {
    ArrayType(SN<ArrayType>),
    BaseType(SN<BaseType>),
    Pair(SourcedSpan),
}

impl Type {
    #[inline]
    #[must_use]
    pub fn size_of(&self) -> usize {
        match *self {
            Self::BaseType(ref b) => b.size_of(),
            Self::ArrayType(ref a) => a.size_of(),
            Self::PairType(_, _) => BaseType::PAIR_PTR_BYTES,
            Self::Error(_) => unreachable!("This branch should always be caught during parsing."),
        }
    }
}

impl BaseType {
    // TODO: the frontend is supposed to be backend-agnostic. The precise sizes of these types
    //       should be moved elsewhere/to the backend, make the frontend more agnostic to e.g.
    //       if the target is 64-bit or 32-bit.

    /// A pointer on a 64-bit machine is 8 bytes.
    pub const PTR_BYTES: usize = 8;

    /// A signed 32-bit integer takes up 4 bytes.
    pub const INT_BYTES: usize = 4;

    /// A boolean can be represented with 1 byte.
    pub const BOOL_BYTES: usize = 1;

    /// A 7-bit ASCII character can be represented with one byte.
    pub const CHAR_BYTES: usize = 1;

    /// The string-type is merely a pointer to the data of the string-body:
    /// a length-prefixed contiguous array of characters.
    pub const STRING_PTR_BYTES: usize = Self::PTR_BYTES;

    /// The length of the string is a pointer-sized value.
    pub const STRING_LEN_BYTES: usize = Self::INT_BYTES;

    /// The string's "elements" are characters.
    pub const STRING_ELEM_BYTES: usize = Self::CHAR_BYTES;

    /// The array-type is merely a pointer to the data of the array-body:
    /// a length-prefixed contiguous array of homogeneously-sized elements.
    pub const ARRAY_PTR_BYTES: usize = Self::PTR_BYTES;

    /// The length of the array is a pointer-sized value.
    pub const ARRAY_LEN_BYTES: usize = Self::INT_BYTES;

    /// A pair is a pointer to two contiguous values on the heap.
    /// A pair null-literal is simply an all-zero null pointer.
    pub const PAIR_PTR_BYTES: usize = Self::PTR_BYTES;

    #[inline]
    #[must_use]
    pub const fn size_of(&self) -> usize {
        match *self {
            Self::Int => Self::INT_BYTES,
            Self::Bool => Self::BOOL_BYTES,
            Self::Char => Self::CHAR_BYTES,
            Self::String => Self::STRING_PTR_BYTES,
        }
    }

    /// The string-body is a value in the heap which begins with a pointer-sized length value,
    /// followed by a contiguous array of characters which corresponds to that length value.
    #[allow(clippy::arithmetic_side_effects)]
    #[inline]
    #[must_use]
    pub const fn string_body_bytes(len: usize) -> usize {
        Self::STRING_LEN_BYTES + len * Self::STRING_ELEM_BYTES
    }
}

impl ArrayType {
    #[must_use]
    #[inline]
    pub const fn new(elem_type: Type) -> Self {
        Self { elem_type }
    }

    #[inline]
    #[must_use]
    pub const fn size_of(&self) -> usize {
        BaseType::ARRAY_PTR_BYTES
    }

    /// The array-body is a value in the heap which begins with a pointer-sized length value,
    /// followed by a contiguous array of elements which corresponds to that length value.
    #[allow(clippy::arithmetic_side_effects)]
    #[inline]
    #[must_use]
    pub fn array_body_bytes(&self, len: usize) -> usize {
        BaseType::ARRAY_LEN_BYTES + len * self.elem_size_of()
    }

    /// The size (in bytes) of the elements of this array
    #[inline]
    #[must_use]
    pub fn elem_size_of(&self) -> usize {
        self.elem_type.size_of()
    }
}

impl PairElemType {
    #[inline]
    #[must_use]
    pub fn size_of(&self) -> usize {
        match *self {
            Self::ArrayType(ref a) => a.size_of(),
            Self::BaseType(ref b) => b.size_of(),
            Self::Pair(_) => BaseType::PAIR_PTR_BYTES,
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
        return_type: SN<Type>,
        name: SN<Ident>,
        params: Box<[FuncParam]>,
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
    //     3. an ‘if’ statement with two returning blocks.
    //
    // All function bodies **MUST** be returning blocks.
    #[inline]
    pub fn is_return_block(&self) -> bool {
        match &**self.last() {
            Stat::Return(_) | Stat::Exit(_) => true,
            Stat::IfThenElse {
                then_body,
                else_body,
                ..
            } => then_body.is_return_block() && else_body.is_return_block(),
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
    pub const fn if_then_else(
        if_cond: SN<Expr>,
        then_body: StatBlock,
        else_body: StatBlock,
    ) -> Self {
        Self::IfThenElse {
            if_cond,
            then_body,
            else_body,
        }
    }

    #[must_use]
    #[inline]
    pub const fn while_do(while_cond: SN<Expr>, body: StatBlock) -> Self {
        Self::WhileDo { while_cond, body }
    }
}

impl RValue {
    #[must_use]
    #[inline]
    pub const fn call(func_name: SN<Ident>, args: Box<[SN<Expr>]>) -> Self {
        Self::Call { func_name, args }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Ident(InternStr);

// impls related to `Ident`
mod ident_impls {
    use crate::alias::InternStr;
    use crate::parsing::ast::Ident;
    use std::fmt;
    use std::ops::Deref;

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
