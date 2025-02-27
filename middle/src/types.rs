// wack-IR deals with:
// unsigned numbers: u8, u16, u32, u64
//

#[repr(u8)]
pub enum Width {
    W8 = 1,
    W16 = 2,
    W32 = 4,
    W64 = 8,
}

pub enum WackType {
    Void,
    Function,
    FirstClass(FirstClassType),
}

pub enum FirstClassType {
    SingleValue(SingleValueType),
}

pub enum SingleValueType {
    /// An unsigned integer
    Integer { width: u32 },
}

/// An integer type, with no distinction between signed/unsigned.
///
/// It can be any arbitrary bit-width, from 1 to 2^23 bits. However, the most common bit-widths
/// correspond to [`WackIntType::INT_8`], [`WackIntType::INT_16`], [`WackIntType::INT_32`], and
/// [`WackIntType::INT_64`].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(transparent)]
pub struct WackIntType {
    width: u32,
}

// impl blocks relating to `WackInt`
pub mod wack_int {
    use crate::types::WackIntType;
    use thiserror::Error;

    #[derive(Error, Debug)]
    #[error("Expected an integer-width between 1 and 2^23 bits, found {0} bits")]
    pub struct InvalidBitWidthError(u32);

    impl WackIntType {
        /// The maximum bit-width is 2^23 bits, mirroring LLVM's type-system
        pub const MAX_WIDTH: u32 = 2 << Self::MAX_WIDTH_BITS;
        const MAX_WIDTH_BITS: u8 = 23;

        /// The minimum bit-width is 1 bit, mirroring LLVM's type-system
        pub const MIN_WIDTH: u32 = 1;

        /// An 8-bit (or 1-byte) integer type (no distinction between signed/unsigned).
        pub const INT_8: Self = Self { width: 8 };

        /// An 16-bit (or 2-byte) unsigned integer type (no distinction between signed/unsigned).
        pub const INT_16: Self = Self { width: 16 };

        /// An 32-bit (or 32-byte) unsigned integer type (no distinction between signed/unsigned).
        pub const INT_32: Self = Self { width: 32 };

        /// An 64-bit (or 8-byte) unsigned integer type (no distinction between signed/unsigned).
        pub const INT_64: Self = Self { width: 64 };

        /// # Safety
        /// Only safe if you are _absolutely certain_ that [`width`] is between [`Self::MIN_WIDTH`]
        /// and [`Self::MAX_WIDTH`].
        #[inline]
        #[must_use]
        pub const unsafe fn new_with_unchecked(width: u32) -> Self {
            Self { width }
        }

        /// # Errors
        /// Will only construct if [`width`] is between [`Self::MIN_WIDTH`] and [`Self::MAX_WIDTH`].
        #[inline]
        pub const fn try_new_with(width: u32) -> Result<Self, InvalidBitWidthError> {
            match width {
                Self::MIN_WIDTH..=Self::MAX_WIDTH => Ok(Self { width }),
                _ => Err(InvalidBitWidthError(width)),
            }
        }

        #[inline]
        #[must_use]
        pub const fn width(&self) -> u32 {
            self.width
        }

        #[inline]
        #[must_use]
        pub const fn into_width(self) -> u32 {
            self.width
        }
    }

    impl TryFrom<u32> for WackIntType {
        type Error = InvalidBitWidthError;

        #[inline]
        fn try_from(value: u32) -> Result<Self, Self::Error> {
            Self::try_new_with(value)
        }
    }
}
