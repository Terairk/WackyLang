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

#[derive(Clone, Copy, Debug)]
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

/* ================ DEBUG IMPLS ==================== */

/* ================ END DEBUG IMPLS ================ */

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

impl ArrayType {
    #[must_use]
    #[inline]
    pub const fn new(elem_type: Type) -> Self {
        Self { elem_type }
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

    #[must_use]
    #[inline]
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

    /// # Safety
    /// If you are _sure_ the semantic type is that of an array, you can unsafely extract
    /// the inner element type; if it isn't an array-type, a runtime panic will occur.
    #[inline]
    #[must_use]
    pub unsafe fn into_array_elem_type(self) -> Self {
        // extract the semantic types
        match self {
            Self::Array(elems_ty) => *elems_ty,
            _ => unreachable!("The type is assumed to be array, but wasn't."),
        }
    }

    /// # Safety
    /// If you are _sure_ the semantic type is that of a pair, you can unsafely extract
    /// the inner element types; if it isn't a pair-type, a runtime panic will occur.
    #[inline]
    #[must_use]
    pub unsafe fn into_pair_elem_types(self) -> (Self, Self) {
        // extract the semantic types
        match self {
            Self::Pair(fst, snd) => (*fst, *snd),
            // TODO: Check if it correct
            // According to the Spec, we should allow one side of the assignments to be untyped, i.e.:
            // pair(int, pair) q = ... ;
            // int x = fst snd q is valid even if we don't know the type during compile time
            // It may fail runtime due to incorrect types, but we should not care here
            // So assume it's AnyType, we should fail somewhere later if it's incorrect(I think)
            Self::ErasedPair => (SemanticType::AnyType, SemanticType::AnyType),
            _ => unreachable!("The type is assumed to be pair, but wasn't."),
        }
    }

    #[inline]
    #[must_use]
    pub fn array(elem: Self) -> Self {
        Self::Array(Box::new(elem))
    }

    #[inline]
    #[must_use]
    pub fn pair(fst: Self, snd: Self) -> Self {
        Self::Pair(Box::new(fst), Box::new(snd))
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
