use syntax::types::SemanticType;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WackType {
    Int,
    Bool,
    Char,
    String,
    Array(Box<Self>),
    Pair(Box<Self>, Box<Self>),
    Pointer(Box<Self>), // not sure if we need this
    ErasedPair,
}

impl From<SemanticType> for WackType {
    fn from(t: SemanticType) -> Self {
        match t {
            SemanticType::Int => WackType::Int,
            SemanticType::Bool => WackType::Bool,
            SemanticType::Char => WackType::Char,
            SemanticType::String => WackType::String,
            SemanticType::Array(t) => WackType::Array(Box::new((*t).into())),
            SemanticType::Pair(t1, t2) => {
                WackType::Pair(Box::new((*t1).into()), Box::new((*t2).into()))
            }
            SemanticType::ErasedPair => WackType::ErasedPair,
            _ => panic!("Error in frontend if these types persist past frontend"),
        }
    }
}

impl WackType {
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

    #[inline]
    #[must_use]
    pub fn size_of(&self) -> usize {
        match self {
            Self::Int => 4,
            Self::Bool => 1,
            Self::Char => 1,
            Self::String => 8,
            Self::Array(_) => 8,
            Self::Pair(_, _) => 8,
            Self::Pointer(_) => 8,
            Self::ErasedPair => 8,
        }
    }
}
