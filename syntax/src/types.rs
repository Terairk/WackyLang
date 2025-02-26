use crate::source::SourcedNode;
use crate::source::SourcedSpan;
use std::cmp::PartialEq;
use std::fmt;
use std::fmt::{Display, Formatter};

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
    pub const STRING_LEN_BYTES: usize = Self::PTR_BYTES;

    /// The string's "elements" are characters.
    pub const STRING_ELEM_BYTES: usize = Self::CHAR_BYTES;

    /// The array-type is merely a pointer to the data of the array-body:
    /// a length-prefixed contiguous array of homogeneously-sized elements.
    pub const ARRAY_PTR_BYTES: usize = Self::PTR_BYTES;

    /// The length of the array is a pointer-sized value.
    pub const ARRAY_LEN_BYTES: usize = Self::PTR_BYTES;

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

impl SemanticType {
    fn pair_inner_erasable(from: &SemanticType, to: &SemanticType) -> bool {
        use SemanticType::{ErasedPair, Pair};
        if let Pair(_, _) = from {
            if *to == ErasedPair {
                return true;
            }
        } else if let Pair(_, _) = to {
            if *from == ErasedPair {
                return true;
            }
        }

        from == to
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
                SemanticType::pair_inner_erasable(a, b) ||
                **a == AnyType
            }
            (Pair(a1, b1), Pair(a2, b2)) => {
                (a1 == a2 && b1 == b2) || // pairs are invariant
                (SemanticType::pair_inner_erasable(a1, a2) && SemanticType::pair_inner_erasable(b1, b2))
            }
            (ErasedPair, Pair(_, _)) => true,
            (Pair(_, _), ErasedPair) => true,
            (ErasedPair, ErasedPair) => true,
            _ => false,
        }
    }

    #[inline]
    #[must_use]
    pub fn size_of(&self) -> usize {
        match *self {
            Self::Int => BaseType::INT_BYTES,
            Self::Bool => BaseType::BOOL_BYTES,
            Self::Char => BaseType::CHAR_BYTES,
            Self::String => BaseType::STRING_PTR_BYTES,
            Self::Array(_) => BaseType::ARRAY_PTR_BYTES,
            Self::Pair(_, _) | Self::ErasedPair => BaseType::PAIR_PTR_BYTES,
            Self::AnyType => {
                unreachable!("We should not be checking the size of the any-type.")
            }
            Self::Error(_) => {
                unreachable!("The error type should not propogate beyond semantic analysis.")
            }
        }
    }
}

impl Display for SemanticType {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SemanticType::Int => write!(f, "int"),
            SemanticType::Bool => write!(f, "bool"),
            SemanticType::Char => write!(f, "char"),
            SemanticType::String => write!(f, "string"),
            SemanticType::Array(elem) => write!(f, "{}[]", elem),
            SemanticType::Pair(left, right) => write!(f, "pair({}, {})", left, right),
            SemanticType::ErasedPair => write!(f, "pair"),
            SemanticType::Error(_span) => write!(f, "unknown gibberish"),
            SemanticType::AnyType => write!(f, "any"),
        }
    }
}
