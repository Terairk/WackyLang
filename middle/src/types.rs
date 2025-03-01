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
    Int { width: BitWidth },
    Array(Box<WackType>),
    Pair(Box<WackType>, Box<WackType>),
}

impl WackType {
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
