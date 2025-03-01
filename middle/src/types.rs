use syntax::types::SemanticType;

/// These are separate types for now, but will be merged later
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct WackFuncType {
    args: Box<[WackType]>,
    ret: Box<WackType>,
}

impl WackFuncType {
    pub fn from_semantic_type(args: Vec<SemanticType>, ret: SemanticType) -> Self {
        let args: Box<[WackType]> = args.into_iter().map(WackType::from_semantic_type).collect();
        let ret = Box::new(WackType::from_semantic_type(ret));
        Self { args, ret }
    }
}

/// Very basic types that __will do for now__, but will be replaced later.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum WackType {
    Pointer(WackPointerType),

    /// Like pointer, points to a location, but the location is code
    Label,
    Int {
        width: BitWidth,
    },
    Array(Box<WackType>),
    Pair(Box<WackType>, Box<WackType>),
}

impl WackType {
    /// A pointer on a 64-bit machine is 8 bytes.
    pub const PTR_BYTES: usize = 8;

    pub const ANY_POINTER: WackType = WackType::Pointer(WackPointerType::Any);

    pub fn pointer_of(of: WackType) -> Self {
        Self::Pointer(WackPointerType::of(of))
    }

    pub fn array(elem_ty: WackType) -> Self {
        Self::Array(Box::new(elem_ty))
    }

    pub fn pair(fst: WackType, snd: WackType) -> Self {
        Self::Pair(Box::new(fst), Box::new(snd))
    }

    pub fn from_semantic_type(semantic_type: SemanticType) -> Self {
        match semantic_type {
            SemanticType::Int => Self::Int {
                width: BitWidth::W32,
            },
            SemanticType::Bool => Self::Int {
                width: BitWidth::W8,
            },
            SemanticType::Char => Self::Int {
                width: BitWidth::W8,
            },
            // a string has the same type-representation as `char[]`
            SemanticType::String => {
                Self::from_semantic_type(SemanticType::array(SemanticType::Char))
            }
            // an array type in WACC translates to a __pointer__ to an array in memory
            SemanticType::Array(elem_ty) => {
                Self::pointer_of(Self::array(Self::from_semantic_type(*elem_ty)))
            }
            // a pair type in WACC translates to a __pointer__ to a pair in memory
            SemanticType::Pair(fst, snd) => Self::pointer_of(Self::pair(
                Self::from_semantic_type(*fst),
                Self::from_semantic_type(*snd),
            )),
            // an erased-pair type can point to just-about anything...
            SemanticType::ErasedPair => Self::ANY_POINTER,

            // something went wrong in the frontend if these branches were reached
            SemanticType::AnyType | SemanticType::Error(_) => {
                unreachable!("There should not be `AnyType` or `Error` types by this stage")
            }
        }
    }

    #[inline]
    pub fn try_size_of(&self) -> Result<usize, Box<str>> {
        match self {
            WackType::Pointer(_) => Ok(Self::PTR_BYTES), // pointers are the same size regardless
            WackType::Int { width } => Ok(width.into_bit_width() as usize),
            WackType::Pair(fst, snd) => Ok(fst.try_size_of()? + snd.try_size_of()?),
            WackType::Array(_) => {
                Err("Cannot know the size of arrays without knowing its length".into())
            }
            WackType::Label => Err("It doesn't make sense to look up labels".into()),
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

            //
            // TODO: this was copied from the semantic-version of the same function: update this function somehow to reflect the same changes
            //
            // // TODO: Check if it correct
            // // According to the Spec, we should allow one side of the assignments to be untyped, i.e.:
            // // pair(int, pair) q = ... ;
            // // int x = fst snd q is valid even if we don't know the type during compile time
            // // It may fail runtime due to incorrect types, but we should not care here
            // // So assume it's AnyType, we should fail somewhere later if it's incorrect(I think)
            // Self::ErasedPair => (SemanticType::AnyType, SemanticType::AnyType),
            //
            _ => unreachable!("The type is assumed to be pair, but wasn't."),
        }
    }
}

/// Using semi-typed pointers for now, but will eventually make them fully untyped.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum WackPointerType {
    Any,
    Of(Box<WackType>),
}

impl WackPointerType {
    pub fn of(ty: WackType) -> Self {
        Self::Of(Box::new(ty))
    }

    pub fn deref_type(&self) -> Option<WackType> {
        match self {
            Self::Any => None,
            Self::Of(ty) => Some(*ty.clone()),
        }
    }

    pub fn try_from_wack_type(wack_type: WackType) -> Option<Self> {
        match wack_type {
            WackType::Pointer(p) => Some(p),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum BitWidth {
    W8 = 8,
    W16 = 16,
    W32 = 32,
    W64 = 64,
}

// impls relating to `BitWidth`
pub mod x86_64_int_type {
    use crate::types::BitWidth;
    use thiserror::Error;

    #[derive(Error, Debug)]
    #[error("Expected TODO_FIX_ERROR_MESSAGE, found {0} bits")]
    pub struct InvalidBitWidthError(u32);

    impl BitWidth {
        /// # Errors
        /// TODO: add some docs here eventually
        #[inline]
        pub const fn try_from_u32(width: u32) -> Result<Self, InvalidBitWidthError> {
            match width {
                8 => Ok(Self::W8),
                16 => Ok(Self::W16),
                32 => Ok(Self::W32),
                64 => Ok(Self::W64),
                _ => Err(InvalidBitWidthError(width)),
            }
        }

        #[allow(clippy::as_conversions)]
        #[must_use]
        #[inline]
        pub const fn into_bit_width(self) -> u8 {
            self as u8
        }

        #[allow(clippy::as_conversions)]
        #[must_use]
        #[inline]
        pub const fn bit_width(&self) -> u8 {
            Self::into_bit_width(*self)
        }
    }
}
