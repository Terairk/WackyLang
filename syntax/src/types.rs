use crate::source::SourcedNode;
use crate::source::SourcedSpan;
type SN<T> = SourcedNode<T>;

#[derive(Clone, Debug)]
pub enum SemanticType {
    Int,
    Bool,
    Char,
    String,
    Array(Box<SemanticType>),
    Pair(Box<SemanticType>, Box<SemanticType>),
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
    BaseType(SN<BaseType>),
    ArrayType(SN<ArrayType>),
    Pair(SourcedSpan),
}

// Helper Functions to convert from syntactic types to semantic types
// might be useful during type checking
impl Type {
    pub fn to_semantic_type(&self) -> SemanticType {
        match self {
            Type::BaseType(sn_base) => match sn_base.inner() {
                BaseType::Int => SemanticType::Int,
                BaseType::Bool => SemanticType::Bool,
                BaseType::Char => SemanticType::Char,
                BaseType::String => SemanticType::String,
            },
            Type::ArrayType(sn_array) => {
                let elem_type = sn_array.elem_type.to_semantic_type();
                SemanticType::Array(Box::new(elem_type))
            }
            Type::PairType(left, right) => {
                let left_type = left.to_semantic_type();
                let right_type = right.to_semantic_type();
                SemanticType::Pair(Box::new(left_type), Box::new(right_type))
            }
            Type::Error(span) => SemanticType::Error(span.clone()),
        }
    }
}

impl PairElemType {
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
