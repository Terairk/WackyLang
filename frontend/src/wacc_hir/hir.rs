use crate::alias::InternStr;
use crate::parsing::ast;
use crate::source::{SourcedBoxedNode, SourcedNode, SourcedSpan};
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
    /// Functions retain their names (no renaming)
    pub name: SN<ast::Ident>,
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
    LoopDo {
        label: LoopLabel,
        body: StatBlock,
    },
    NextLoop(LoopLabel),
    Break(LoopLabel),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LValue {
    Ident(SN<Ident>),
    ArrayElem(SN<ArrayElem>),
    PairElem(SN<PairElem>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum RValue {
    Expr(SN<Expr>),
    ArrayLiter(Box<[SN<Expr>]>),
    NewPair(SN<Expr>, SN<Expr>),
    PairElem(SN<PairElem>),
    Call {
        func_name: SN<ast::Ident>,
        args: Box<[SN<Expr>]>,
    },
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
#[repr(u8)]
pub enum PairElemSelector {
    Fst,
    Snd,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PairElem(pub PairElemSelector, pub SBN<LValue>);

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ArrayElem {
    pub array_name: SN<Ident>,
    pub indices: NonemptyArray<SN<Expr>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
    Liter(SN<Liter>),
    Ident(SN<Ident>),
    ArrayElem(SBN<ArrayElem>),
    Unary(SN<UnaryOper>, SBN<Self>),
    Binary(SBN<Self>, SN<BinaryOper>, SBN<Self>),
    IfThenElse {
        if_cond: SBN<Self>,
        then_val: SBN<Self>,
        else_val: SBN<Self>,
    },
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Liter {
    IntLiter(i32),
    BoolLiter(bool),
    // 7-bit ASCII-literal fits in 8 bytes
    CharLiter(u8),
    StrLiter(InternStr),
    PairLiter,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum UnaryOper {
    LNot,
    Minus,
    Len,
    Ord,
    Chr,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub ident: ast::Ident,
    pub uuid: usize,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LoopLabel {
    pub ident: ast::Ident,
    pub uuid: usize,
}

// all implementation blocks live here
mod impls {
    use crate::parsing::ast;
    use crate::wacc_hir::hir::{
        ArrayElem, ArrayType, EmptyStatVecError, Expr, Func, FuncParam, Ident, LValue, LoopLabel,
        Program, RValue, Stat, StatBlock, Type, SBN, SN,
    };
    use delegate::delegate;
    use std::fmt;
    use util::func::f2::F2OnceExt as _;
    use util::nonempty::NonemptyArray;

    impl Expr {
        #[must_use]
        #[inline]
        pub const fn if_then_else(
            if_cond: SBN<Self>,
            then_val: SBN<Self>,
            else_val: SBN<Self>,
        ) -> Self {
            Self::IfThenElse {
                if_cond,
                then_val,
                else_val,
            }
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
            name: SN<ast::Ident>,
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
    }

    impl RValue {
        #[must_use]
        #[inline]
        pub const fn call(func_name: SN<ast::Ident>, args: Box<[SN<Expr>]>) -> Self {
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

    // Customised debug/display for prettier debugging
    impl fmt::Debug for Ident {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}@{}", self.ident, self.uuid)
        }
    }
    impl fmt::Display for Ident {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}@{}", self.ident, self.uuid)
        }
    }

    impl Ident {
        pub const ZERO_UUID: usize = 0;

        /// We use a counter to generate unique IDs, counter is automatically incremented
        /// Note we use the same counter for all identifiers.
        /// Takes in [`ast::Ident`] as this is what we always get from the AST
        #[inline]
        #[must_use]
        pub const fn new(counter: &mut usize, ident: ast::Ident) -> Self {
            // would rather crash in debug builds than define a saturating
            // so we can change this to u128
            // though I suspect we'd have bigger problems before then
            #[allow(clippy::arithmetic_side_effects)]
            *counter += 1;
            Self {
                ident,
                uuid: *counter,
            }
        }

        #[must_use]
        #[inline]
        pub fn new_sn(counter: &mut usize, ident_sn: SN<ast::Ident>) -> SN<Self> {
            ident_sn.map_inner(Self::new.curry()(counter))
        }

        // Function used to create a rogue renamed so we can still build tree even if we have errors
        #[must_use]
        #[inline]
        pub const fn new_rogue_zero(ident: ast::Ident) -> Self {
            Self {
                ident,
                uuid: Self::ZERO_UUID,
            }
        }

        #[must_use]
        #[inline]
        pub fn new_rouge_zero_sn(ident_sn: SN<ast::Ident>) -> SN<Self> {
            ident_sn.map_inner(Self::new_rogue_zero)
        }
    }

    // Customised debug/display for prettier debugging
    impl fmt::Debug for LoopLabel {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}@{}", self.ident, self.uuid)
        }
    }
    impl fmt::Display for LoopLabel {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}@{}", self.ident, self.uuid)
        }
    }

    impl LoopLabel {
        pub const ZERO_UUID: usize = 0;

        /// We use a counter to generate unique IDs, counter is automatically incremented
        /// Note we use the same counter for all identifiers.
        /// Takes in [`ast::Ident`] as this is what we always get from the AST
        #[inline]
        #[must_use]
        pub(crate) const fn new(counter: &mut usize, ident: ast::Ident) -> Self {
            // would rather crash in debug builds than define a saturating
            // so we can change this to u128
            // though I suspect we'd have bigger problems before then
            #[allow(clippy::arithmetic_side_effects)]
            *counter += 1;
            Self {
                ident,
                uuid: *counter,
            }
        }

        // #[must_use]
        // #[inline]
        // pub fn new_sn(counter: &mut usize, ident_sn: SN<ast::Ident>) -> SN<Self> {
        //     ident_sn.map_inner(Self::new.curry()(counter))
        // }

        // Function used to create a rogue loop-label so we can still build tree even if we have errors
        #[must_use]
        #[inline]
        pub const fn new_rogue_zero(ident: ast::Ident) -> Self {
            Self {
                ident,
                uuid: Self::ZERO_UUID,
            }
        }

        // #[must_use]
        // #[inline]
        // pub fn new_rouge_zero_sn(ident_sn: SN<ast::Ident>) -> SN<Self> {
        //     ident_sn.map_inner(Self::new_rogue_zero)
        // }
    }
}
