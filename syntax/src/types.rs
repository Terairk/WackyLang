use crate::source::SourcedNode;
use crate::source::SourcedSpan;
use std::cmp::PartialEq;
type SN<T> = SourcedNode<T>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemanticType {
    Int,
    Bool,
    Char,
    String,
    Array(Box<SemanticType>),
    Pair(Box<SemanticType>, Box<SemanticType>),
    ErasedPair,
    Unknown,
    AnyType,
    Error(SourcedSpan), // For invalid types
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
    ArrayType(SN<ArrayType>),
    BaseType(SN<BaseType>),
    Pair(SourcedSpan),
}

// Helper Functions to convert from syntactic types to semantic types
// might be useful during type checking
impl Type {
    #[inline]
    pub fn to_semantic_type(&self) -> SemanticType {
        match self {
            Self::BaseType(sn_base) => match sn_base.inner() {
                BaseType::Int => SemanticType::Int,
                BaseType::Bool => SemanticType::Bool,
                BaseType::Char => SemanticType::Char,
                BaseType::String => SemanticType::String,
            },
            Self::ArrayType(sn_array) => {
                let elem_type = sn_array.elem_type.to_semantic_type();
                SemanticType::Array(Box::new(elem_type))
            }
            Self::PairType(left, right) => {
                let left_type = left.to_semantic_type();
                let right_type = right.to_semantic_type();
                SemanticType::Pair(Box::new(left_type), Box::new(right_type))
            }
            Self::Error(span) => SemanticType::Error(span.clone()),
        }
    }
}

impl PairElemType {
    #[inline]
    pub fn to_semantic_type(&self) -> SemanticType {
        match self {
            PairElemType::BaseType(sn_base) => match sn_base.inner() {
                BaseType::Int => SemanticType::Int,
                BaseType::Bool => SemanticType::Bool,
                BaseType::Char => SemanticType::Char,
                BaseType::String => SemanticType::String,
            },
            PairElemType::ArrayType(sn_array) => {
                let elem_type = sn_array.elem_type.to_semantic_type();
                SemanticType::Array(Box::new(elem_type))
            }
            PairElemType::Pair(_span) => SemanticType::ErasedPair,
        }
    }
}

impl SemanticType {
    fn pair_inner_erasable(from: &SemanticType, to: &SemanticType) -> bool {
        use SemanticType::{Pair, ErasedPair};
        if let Pair(_, _) = from {
            return *to == ErasedPair;
        }

        return from == to;
    }

    pub fn can_coerce_into(&self, to: &SemanticType) -> bool {
        use SemanticType::*;

        if to == &SemanticType::String {
            if let SemanticType::Array(from_inner) = self {
                return **from_inner == SemanticType::Char;
            }
        }

        match (self, to) {
            (AnyType, _) => true,
            (Error(_), _) => true,
            (_, AnyType) => true,
            (_, Error(_)) => true,
            (Int, Int) | (Bool, Bool) | (Char, Char) | (String, String) => true,
            (Array(a), Array(b)) => {
                a == b || // arrays are invariant
                **a == AnyType
            },
            (Pair(a1, b1), Pair(a2, b2)) => {
                (a1 == a2 && b1 == b2) || // pairs are invariant
                (SemanticType::pair_inner_erasable(a1, a2) && SemanticType::pair_inner_erasable(b1, b2)) ||
                (**a1 == AnyType && **b1 == AnyType) // unless it's null
            },
            (ErasedPair, Pair(_, _)) => true,
            (Pair(_, _), ErasedPair) => true,
            (ErasedPair, ErasedPair) => true,
            (Unknown, Unknown) => true,
            _ => false,
        }
    }
}