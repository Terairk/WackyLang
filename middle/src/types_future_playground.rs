// Playground code is not usedanywhere, we can safely comment it out to prevent 16 warnings
// // NOTE: labels can be cast to pointers via an instruction
// //       you MAY be able to get function pointers?????????? out of functions...
//
// #[derive(Clone, PartialEq, Eq, Debug)]
// pub enum WackType_2 {
//     Void,
//     Fn(WackFnType_2),
//     FirstClass(FirstClassType_2),
// }
//
// #[derive(Clone, PartialEq, Eq, Debug)]
// pub struct WackFnType_2 {
//     arg_types: Box<[WackType_2]>, // change later to supportable fn types
//     ret: Box<[WackType_2]>,       // change later to supportable fn return types
// }
//
// #[derive(Clone, PartialEq, Eq, Debug)]
// pub enum FirstClassType_2 {
//     SingleValue(SingleValueType_2),
//     Label,
//     // Token, // Uncomment if we ever need unintrospectable/unobscurable-values types lol 😂
//     // Metadata, // Uncomment if we ever need to attach metadata lol 😂
//     Aggregate(WackAggregateType),
// }
//
// #[derive(Clone, Copy, PartialEq, Eq, Debug)]
// pub enum SingleValueType_2 {
//     Int(WackIntType_2),
//     // Float(WackFloatType), // Uncomment if we ever need float types
//     // X86Amx, // Uncomment if we ever need to use Advanced Matrix Extensions lol 😂
//     Pointer,
//     // TargetExtensionType, // Uncomment if we ever need to use target-dependent types lol 😂
//     // Vector, // Uncomment if we ever need to use SIMD-vector types lol 😂
// }
//
// /// An integer type, with no distinction between signed/unsigned.
// ///
// /// It can be any arbitrary bit-width, from 1 to 2^23 bits. However, the most common bit-widths
// /// correspond to [`WackIntType_2::INT_8`], [`WackIntType_2::INT_16`], [`WackIntType_2::INT_32`], and
// /// [`WackIntType_2::INT_64`].
// #[derive(Clone, Copy, PartialEq, Eq, Debug)]
// #[repr(transparent)]
// pub struct WackIntType_2 {
//     bit_width: u32,
// }
//
// // impl blocks relating to `WackInt`
// pub mod wack_int {
//     use crate::types_future_playground::WackIntType_2;
//     use thiserror::Error;
//
//     #[derive(Error, Debug)]
//     #[error("Expected an integer bit-width between 1 and 2^23 bits, found {0} bits")]
//     pub struct InvalidBitWidthError(u32);
//
//     impl WackIntType_2 {
//         /// The maximum bit-width is 2^23 bits, mirroring LLVM's type-system
//         pub const MAX_BIT_WIDTH: u32 = 2 << Self::MAX_BIT_WIDTH_LOG2;
//         const MAX_BIT_WIDTH_LOG2: u8 = 23;
//
//         /// The minimum bit-width is 1 bit, mirroring LLVM's type-system
//         pub const MIN_BIT_WIDTH: u32 = 1;
//
//         /// An 8-bit (or 1-byte) integer type (no distinction between signed/unsigned).
//         pub const INT_8: Self = Self { bit_width: 8 };
//
//         /// An 16-bit (or 2-byte) unsigned integer type (no distinction between signed/unsigned).
//         pub const INT_16: Self = Self { bit_width: 16 };
//
//         /// An 32-bit (or 32-byte) unsigned integer type (no distinction between signed/unsigned).
//         pub const INT_32: Self = Self { bit_width: 32 };
//
//         /// An 64-bit (or 8-byte) unsigned integer type (no distinction between signed/unsigned).
//         pub const INT_64: Self = Self { bit_width: 64 };
//
//         /// # Safety
//         /// Only safe if you are _absolutely certain_ that [`width`] is between [`Self::MIN_WIDTH`]
//         /// and [`Self::MAX_WIDTH`].
//         #[inline]
//         #[must_use]
//         pub const unsafe fn new_with_unchecked(bit_width: u32) -> Self {
//             Self { bit_width }
//         }
//
//         /// # Errors
//         /// Will only construct if [`width`] is between [`Self::MIN_WIDTH`] and [`Self::MAX_WIDTH`].
//         #[inline]
//         pub const fn try_new_with(bit_width: u32) -> Result<Self, InvalidBitWidthError> {
//             match bit_width {
//                 Self::MIN_BIT_WIDTH..=Self::MAX_BIT_WIDTH => Ok(Self { bit_width }),
//                 _ => Err(InvalidBitWidthError(bit_width)),
//             }
//         }
//
//         #[inline]
//         #[must_use]
//         pub const fn bit_width(&self) -> u32 {
//             self.bit_width
//         }
//
//         #[inline]
//         #[must_use]
//         pub const fn into_bit_width(self) -> u32 {
//             self.bit_width
//         }
//     }
//
//     impl TryFrom<u32> for WackIntType_2 {
//         type Error = InvalidBitWidthError;
//
//         #[inline]
//         fn try_from(value: u32) -> Result<Self, Self::Error> {
//             Self::try_new_with(value)
//         }
//     }
// }
//
// /// TODO: make docs from https://llvm.org/docs/LangRef.html#floating-point-types
// ///       if we ever end up using floating point type...
// #[derive(Clone, Copy, PartialEq, Eq, Debug)]
// #[repr(u8)]
// pub enum WackFloatType_2 {
//     Ieee(Ieee754),
//     BFloat,
//     X86Fp80,
//     PpcFp128,
// }
//
// /// TODO: make docs from https://llvm.org/docs/LangRef.html#floating-point-types
// ///       if we ever end up using floating point type...
// #[derive(Clone, Copy, PartialEq, Eq, Debug)]
// #[repr(u8)]
// pub enum Ieee754 {
//     Bin16,
//     Bin32,
//     Bin64,
//     Bin128,
// }
//
// #[derive(Clone, PartialEq, Eq, Debug)]
// pub enum WackAggregateType {
//     Array(WackArrayType_2),
//     Struct(WackStructType_2),
// }
//
// #[derive(Clone, PartialEq, Eq, Debug)]
// pub struct WackArrayType_2 {
//     size: usize,
//     elem_type: Box<WackType_2>, // change to "sized" types
// }
//
// #[derive(Clone, PartialEq, Eq, Debug)]
// pub struct WackStructType_2 {
//     field_types: Vec<WackType_2>, // change to "sized" type
//     packed: bool,
// }
