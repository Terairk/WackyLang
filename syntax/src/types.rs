use crate::source::SourcedNode;
use crate::source::SourcedSpan;
use std::cmp::PartialEq;
type SN<T> = SourcedNode<T>;

#[derive(Clone, Debug)]
pub enum SemanticType {
    Int,
    Bool,
    Char,
    String,
    Array(Box<SemanticType>),
    Pair(Box<SemanticType>, Box<SemanticType>),
    ErasedPair,
    Unknown,
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
            PairElemType::Pair(span) => SemanticType::Error(span.clone()),
        }
    }
}

impl PartialEq for SemanticType {
    fn eq(&self, other: &Self) -> bool {
        use SemanticType::*;
        match (self, other) {
            (Int, Int) | (Bool, Bool) | (Char, Char) | (String, String) => true,
            (Array(a), Array(b)) => a == b,
            (Pair(a1, b1), Pair(a2, b2)) => a1 == a2 && b1 == b2,
            (ErasedPair, ErasedPair) => true,
            (Unknown, Unknown) => true,
            (Error(_), Error(_)) => true, // Treat all errors as equal, ignoring SourcedSpan details
            _ => false,
        }
    }
}

impl Eq for SemanticType {}

