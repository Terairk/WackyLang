use frontend::wacc_thir::thir::Type;
use frontend::wacc_thir::types::BaseType;
use util::ext::BoxedSliceExt;

/// These are separate types for now, but will be merged later
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct WackFuncType {
    args: Box<[WackType]>,
    ret: Box<WackType>,
}

impl WackFuncType {
    pub fn from_thir_type(args: Box<[Type]>, ret: Type) -> Self {
        let args: Box<[WackType]> = args.map(WackType::from_thir_type);
        let ret = Box::new(WackType::from_thir_type(ret));
        Self { args, ret }
    }
}

/// Very basic types that __will do for now__, but will be replaced later.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum WackType {
    Pointer(WackPointerType),

    /// Like pointer, points to a location, but the location is code
    Int {
        width: BitWidth,
    },
    Array(Box<WackType>),
    Pair(Box<WackType>, Box<WackType>),

    // this type represents a placeholder type for things that will never be accessed
    Uninhabited,
}

impl WackType {
    /// A pointer on a 64-bit machine is 8 bytes.
    pub const PTR_BYTES: usize = 8;

    /// The width of the integer-length of a WACC array.
    pub const WACC_ARRAY_LEN_WIDTH: BitWidth = BitWidth::W32;

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

    #[inline]
    pub fn from_thir_type(thir_type: Type) -> Self {
        match thir_type {
            Type::BaseType(BaseType::Bool) => Self::Int {
                width: BitWidth::W8,
            },
            Type::BaseType(BaseType::Char) => Self::Int {
                width: BitWidth::W8,
            },
            Type::BaseType(BaseType::Int) => Self::Int {
                width: BitWidth::W32,
            },
            // a string has the same type-representation as `char[]`
            Type::BaseType(BaseType::String) => Self::from_thir_type(Type::char_array()),
            // an array type in WACC translates to a __pointer__ to an array in memory
            Type::ArrayType(boxed) => {
                Self::pointer_of(Self::array(
                    match Self::from_thir_type((*boxed).elem_type) {
                        // arrays of type Any: `any[]` are a direct result of them being
                        // empty array literals, meaning it is fine to default to untyped pointer-elements
                        // in order to avoid weird typing bugs
                        Self::Uninhabited => Self::Pointer(WackPointerType::Any),
                        other => other,
                    },
                ))
            }
            // a pair type in WACC translates to a __pointer__ to a pair in memory
            Type::PairType(boxed) => Self::pointer_of(Self::pair(
                Self::from_thir_type(boxed.fst_type),
                Self::from_thir_type(boxed.snd_type),
            )),

            // TODO: if compile bugs, not handling this this might be the culprit.....
            // // an erased-pair type can point to just-about anything...
            // SemanticType::ErasedPair => Self::ANY_POINTER,

            // Map AnyType to Uninhabited, as they are logically the same thing - types that will
            // never be inhabited/used directly
            Type::Any => Self::Uninhabited,
        }
    }

    #[inline]
    pub fn try_size_of(&self) -> Result<usize, Box<str>> {
        match self {
            Self::Pointer(_) => Ok(Self::PTR_BYTES), // pointers are the same size regardless
            Self::Int { width } => Ok(width.into_byte_width() as usize),
            Self::Pair(fst, snd) => Ok(fst.try_size_of()? + snd.try_size_of()?),
            Self::Array(_) => {
                Err("Cannot know the size of arrays without knowing its length".into())
            }
            Self::Uninhabited => Err("Uninhabited types should never be accessed".into()),
        }
    }

    /// # Safety
    /// If you are _sure_ the THIR type is that of an array, you can unsafely extract
    /// the inner element type; if it isn't an array-type, a runtime panic will occur.
    #[inline]
    #[must_use]
    pub unsafe fn into_array_elem_type(self) -> Self {
        // extract the THIR types
        match self {
            Self::Array(elems_ty) => *elems_ty,
            _ => unreachable!("The type is assumed to be array, but wasn't."),
        }
    }

    /// # Safety
    /// If you are _sure_ the THIR type is that of a pair, you can unsafely extract
    /// the inner element types; if it isn't a pair-type, a runtime panic will occur.
    #[inline]
    #[must_use]
    pub unsafe fn into_pair_elem_types(self) -> (Self, Self) {
        // extract the THIR types
        match self {
            Self::Pair(fst, snd) => (*fst, *snd),

            //
            // TODO: this was copied from the THIR-version of the same function: update this function somehow to reflect the same changes
            //
            // // TODO: Check if it correct
            // // According to the Spec, we should allow one side of the assignments to be untyped, i.e.:
            // // pair(int, pair) q = ... ;
            // // int x = fst snd q is valid even if we don't know the type during compile time
            // // It may fail runtime due to incorrect types, but we should not care here
            // // So assume it's AnyType, we should fail somewhere later if it's incorrect(I think)
            // Self::ErasedPair => (SemanticType::AnyType, SemanticType::AnyType),
            //
            _ => unreachable!(
                "The type is assumed to be pair, but was actually {:#?}",
                self
            ),
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

        pub const fn into_byte_width(self) -> u8 {
            self.into_bit_width() / 8
        }

        pub const fn byte_width(&self) -> u8 {
            Self::into_byte_width(*self)
        }
    }
}
