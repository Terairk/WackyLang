// /// TODO: check this is correct repr
// ///
// /// I am assuming x86_64 can only handle a fixed number of integer types?
// /// namely, 8-bit, 16-bit, 32-bit, 64-bit?? unless there is a greater range....
// #[derive(Clone, Copy, PartialEq, Eq, Debug)]
// #[repr(u8)]
// pub enum X86_64IntType {
//     Int8 = 8,
//     Int16 = 16,
//     Int32 = 32,
//     Int64 = 64,
// }
//
// // impls relating to `X86_64IntType`
// pub mod x86_64_int_type {
//     use crate::future_types_playground::X86_64IntType;
//     use middle::types_future_playground::WackIntType_2;
//     use thiserror::Error;
//
//     #[derive(Error, Debug)]
//     #[error("Expected TODO_FIX_ERROR_MESSAGE, found {0} bits")]
//     pub struct InvalidBitWidthError(u32);
//
//     impl X86_64IntType {
//         /// # Errors
//         /// TODO: add some docs here eventually
//         #[inline]
//         pub const fn try_from_u32(width: u32) -> Result<Self, InvalidBitWidthError> {
//             match width {
//                 8 => Ok(Self::Int8),
//                 16 => Ok(Self::Int16),
//                 32 => Ok(Self::Int32),
//                 64 => Ok(Self::Int64),
//                 _ => Err(InvalidBitWidthError(width)),
//             }
//         }
//
//         /// # Errors
//         /// TODO: add some docs here eventually
//         #[inline]
//         pub const fn try_from_wack_int(
//             wack_int: WackIntType_2,
//         ) -> Result<Self, InvalidBitWidthError> {
//             Self::try_from_u32(wack_int.bit_width())
//         }
//
//         #[allow(clippy::as_conversions)]
//         #[must_use]
//         #[inline]
//         pub const fn into_bit_width(self) -> u8 {
//             self as u8
//         }
//
//         #[allow(clippy::as_conversions)]
//         #[must_use]
//         #[inline]
//         pub const fn bit_width(&self) -> u8 {
//             Self::into_bit_width(*self)
//         }
//     }
//
//     impl TryFrom<WackIntType_2> for X86_64IntType {
//         type Error = InvalidBitWidthError;
//
//         #[inline]
//         fn try_from(value: WackIntType_2) -> Result<Self, Self::Error> {
//             Self::try_from_wack_int(value)
//         }
//     }
// }
