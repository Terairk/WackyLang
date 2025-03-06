#![allow(clippy::arbitrary_source_item_ordering)]

use crate::source::{SourcedNode, SourcedSpan};
use crate::types::Type;
use delegate::delegate;
use internment::ArcIntern;
use std::{fmt::Debug, ops::Deref};
use thiserror::Error;
use util::nonempty::NonemptyArray;
/* File contains the definition for the AST
 * This AST is generic over a Name (N) and a Type (T)
 * Example AST is UntypedAST = Program<Ident, ()>
 * This file contains the definitions first and then the impl blocks
 */

// A file-local type alias for better readability of type definitions
type SN<T> = SourcedNode<T>;

// Definitions for Names used to parametrise AST
// Ident on its own will be considered an OriginalName

// id_table: Just for RenamedAST
// id_func_table: HashMap<RenamedName, (ReturnedType, List<ParamType>)
// symbol_table: HashMap<RenamedName, SemanticType>

// rename: ParserAST -> RenamedAST - identifier table
// symbol table is created
// type-check: RenamedAST -> maybe id_func_table -> TypedAST
//

// For the rest of the file
// N refers to Ident | RenamedName
// T refers to Type | Semantic Type
// where Type is really a Syntactic Type

#[derive(Clone, Debug)]
pub struct Program<N, T> {
    pub funcs: Box<[Func<N, T>]>,
    pub body: StatBlock<N, T>,
}

#[derive(Clone, Debug)]
pub struct Func<N, T> {
    // Leave this as Type to not mess up parametricity
    // This shouldn't change though you can use the helpers in types.rs
    // to convert to a SemanticType
    // name is an Ident since functions are not renamed
    pub return_type: SN<Type>,
    pub name: SN<Ident>,
    pub params: Box<[FuncParam<N>]>,
    pub body: StatBlock<N, T>,
}

#[derive(Clone, Debug)]
pub struct FuncParam<N> {
    // Leave this as Type to not mess up parametricity
    // This shouldn't change though you can use the helpers in types.rs
    // to convert to a SemanticType
    pub r#type: SN<Type>, // Leave this as Type
    pub name: SN<N>,
}

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct StatBlock<N, T>(pub NonemptyArray<SN<Stat<N, T>>>);

#[derive(Error, Debug)]
#[error("Cannot create to `StatBlock` because the supplied `Vec<Stat>` is empty")]
pub struct EmptyStatVecError;

#[derive(Clone, Debug)]
pub enum Stat<N, T> {
    Skip,
    VarDefinition {
        // Leave this to not mess up parametricity
        // This shouldn't change though you can use the helpers in types.rs
        // to convert to a SemanticType
        r#type: SN<Type>,
        name: SN<N>,
        rvalue: SN<RValue<N, T>>,
    },
    Assignment {
        lvalue: SN<LValue<N, T>>,
        rvalue: SN<RValue<N, T>>,
    },
    Read(SN<LValue<N, T>>),
    Free(SN<Expr<N, T>>),
    Return(SN<Expr<N, T>>),
    Exit(SN<Expr<N, T>>),
    Print(SN<Expr<N, T>>),
    Println(SN<Expr<N, T>>),
    IfThenElse {
        if_cond: SN<Expr<N, T>>,
        then_body: StatBlock<N, T>,
        else_body: StatBlock<N, T>,
    },
    WhileDo {
        while_cond: SN<Expr<N, T>>,
        body: StatBlock<N, T>,
    },
    Scoped(StatBlock<N, T>),
}

#[derive(Clone, Debug)]
pub enum LValue<N, T> {
    Ident(SN<N>, T),
    ArrayElem(SN<ArrayElem<N, T>>, T),
    PairElem(SN<PairElem<N, T>>, T),
}

impl<N, T: Clone> LValue<N, T> {
    #[inline]
    pub fn get_type(&self) -> T {
        match *self {
            Self::Ident(_, ref t) | Self::ArrayElem(_, ref t) | Self::PairElem(_, ref t) => {
                t.clone()
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum RValue<N, T> {
    Expr(SN<Expr<N, T>>, T),                    // Type info already in Expr
    ArrayLiter(Box<[SN<Expr<N, T>>]>, T),       // Array needs a type
    NewPair(SN<Expr<N, T>>, SN<Expr<N, T>>, T), // Pair needs a type I think
    PairElem(SN<PairElem<N, T>>, T),            // Needs a type similar to LValue
    Call {
        func_name: SN<Ident>,
        args: Box<[SN<Expr<N, T>>]>,
        return_type: T, // Add return type here
    },
}

#[derive(Clone, Debug)]
pub enum PairElem<N, T> {
    Fst(SN<LValue<N, T>>),
    Snd(SN<LValue<N, T>>),
}

#[derive(Clone, Debug)]
pub struct ArrayElem<N, T> {
    pub array_name: SN<N>,
    pub indices: NonemptyArray<SN<Expr<N, T>>>,
}

#[derive(Clone, Debug)]
pub enum Expr<N, T> {
    Liter(Liter, T),
    Ident(SN<N>, T),
    ArrayElem(SN<ArrayElem<N, T>>, T),
    Unary(SN<UnaryOper>, SN<Self>, T),
    Binary(SN<Self>, SN<BinaryOper>, SN<Self>, T),
    Paren(SN<Self>, T),

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

// Currently, these functions are for the parser to use
// TODO: maybe this will change so edit this if this does

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

impl<N, T> Program<N, T> {
    #[must_use]
    #[inline]
    pub const fn new(funcs: Box<[Func<N, T>]>, body: StatBlock<N, T>) -> Self {
        Self { funcs, body }
    }
}

impl<N, T> Func<N, T> {
    #[must_use]
    #[inline]
    pub const fn new(
        return_type: SN<Type>,
        name: SN<Ident>,
        params: Box<[FuncParam<N>]>,
        body: StatBlock<N, T>,
    ) -> Self {
        Self {
            return_type,
            name,
            params,
            body,
        }
    }
}

impl<N> FuncParam<N> {
    #[must_use]
    #[inline]
    pub const fn new(r#type: SN<Type>, name: SN<N>) -> Self {
        Self { r#type, name }
    }
}

impl<N, T> StatBlock<N, T> {
    #[must_use]
    #[inline]
    pub fn singleton(spanned_stat: SN<Stat<N, T>>) -> Self {
        Self(NonemptyArray::singleton(spanned_stat))
    }

    #[allow(clippy::missing_errors_doc)]
    #[inline]
    pub fn try_new(spanned_stats: Vec<SN<Stat<N, T>>>) -> Result<Self, EmptyStatVecError> {
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
    #[must_use]
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
            pub fn first(&self) -> &SN<Stat<N, T>>;
            #[inline]
            pub fn last(&self) -> &SN<Stat<N, T>>;
        }
    }
}

impl<N, T> From<SN<Stat<N, T>>> for StatBlock<N, T> {
    #[inline]
    fn from(spanned_stat: SN<Stat<N, T>>) -> Self {
        Self::singleton(spanned_stat)
    }
}
impl<N, T> TryFrom<Vec<SN<Stat<N, T>>>> for StatBlock<N, T> {
    type Error = EmptyStatVecError;

    #[inline]
    fn try_from(spanned_stats: Vec<SN<Stat<N, T>>>) -> Result<Self, Self::Error> {
        Self::try_new(spanned_stats)
    }
}
impl<N, T> Stat<N, T> {
    #[must_use]
    #[inline]
    pub const fn var_definition(r#type: SN<Type>, name: SN<N>, rvalue: SN<RValue<N, T>>) -> Self {
        Self::VarDefinition {
            r#type,
            name,
            rvalue,
        }
    }

    #[must_use]
    #[inline]
    pub const fn assignment(lvalue: SN<LValue<N, T>>, rvalue: SN<RValue<N, T>>) -> Self {
        Self::Assignment { lvalue, rvalue }
    }

    #[must_use]
    #[inline]
    pub const fn if_then_else(
        if_cond: SN<Expr<N, T>>,
        then_body: StatBlock<N, T>,
        else_body: StatBlock<N, T>,
    ) -> Self {
        Self::IfThenElse {
            if_cond,
            then_body,
            else_body,
        }
    }

    #[must_use]
    #[inline]
    pub const fn while_do(while_cond: SN<Expr<N, T>>, body: StatBlock<N, T>) -> Self {
        Self::WhileDo { while_cond, body }
    }
}

impl RValue<Ident, ()> {
    #[must_use]
    #[inline]
    pub const fn call(func_name: SN<Ident>, args: Box<[SN<Expr<Ident, ()>>]>) -> Self {
        Self::Call {
            func_name,
            args,
            return_type: (),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Ident(ArcIntern<str>);

// impls related to `Ident`
mod ident_impls {
    use crate::ast::Ident;
    use internment::ArcIntern;
    use std::fmt;
    use std::ops::Deref;

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

        #[must_use]
        #[inline]
        pub const fn inner(&self) -> &ArcIntern<str> {
            &self.0
        }

        #[must_use]
        #[inline]
        pub fn into_inner(self) -> ArcIntern<str> {
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
}

impl<N, T> ArrayElem<N, T> {
    #[must_use]
    #[inline]
    pub const fn new(array_name: SN<N>, indices: NonemptyArray<SN<Expr<N, T>>>) -> Self {
        Self {
            array_name,
            indices,
        }
    }
}
