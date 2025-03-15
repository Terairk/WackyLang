// use crate::rust_gadt_playground::{TyEqInstance, TyEqWitness};
//
// // pub trait MyTrait {}
// // pub type MyTraitDyn = dyn MyTrait;
//
// #[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
// pub struct I32Ptr {
//     ptr: i32,
// }
//
// // `data Op r where`
// #[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
// pub enum Op<R> {
//     // `OpRead :: IntPtr -> Op Int`
//     Read(I32Ptr, TyEqWitness<R, i32>),
//
//     // `OpWrite :: IntPtr -> Int -> Op ()`
//     Write(I32Ptr, i32, TyEqWitness<R, ()>),
//
//     // `OpCAS :: IntPtr -> Int -> Int -> Op Bool`
//     CAS(I32Ptr, i32, i32, TyEqWitness<R, bool>), // compare and swap
// }
//
// #[inline]
// pub fn test_pattern_match<R>(op: &Op<R>, other: R) -> bool {
//     match op {
//         Op::Read(ptr, eq_i32_wit) => {
//             // I can "equality-cast" `R` to `i32`
//             let other = <R as TyEqInstance<i32>>::into_ty_eq_with(other, eq_i32_wit);
//
//             // and use operations specific to the type
//             ptr.ptr <= other
//         }
//         Op::Write(ptr, i32_val, _eq_unit_wit) => {
//             // the type is `R=()`, uninteresting...
//             ptr.ptr == *i32_val
//         }
//         Op::CAS(ptr, i32_val_a, i32_val_b, eq_bool_wit) => {
//             // I can "equality-cast" `R` to `bool` here
//             let other = <R as TyEqInstance<bool>>::into_ty_eq_with(other, eq_bool_wit);
//
//             // and use operations specific to the type
//             ptr.ptr < *i32_val_a && ptr.ptr > *i32_val_b && other
//         }
//     }
// }
//
// // `data Program a where`
// pub enum Program<'a, A> {
//     // Ideally, we would exclude `R` entirely, like in Haskell
//     // `Return :: a -> Program a`
//     Return(A),
//
//     // `Step :: Op r -> (r -> Program a) -> Program a`
//     Step(Box<dyn ProgramStep<A> + 'a>),
// }
//
// // Abstracts over existential type `r` in `Step` constructor (since we can't have existential types in Rust)
// pub trait ProgramStep<A> {}
//
// // Helper structs holds operation and continuation
// pub struct Step<'a, 'b, R, A>(Op<R>, Box<dyn FnOnce(R) -> Program<'b, A> + 'a>);
//
// impl<R, A> ProgramStep<A> for Step<'_, '_, R, A> {}
//
// #[inline]
// pub fn lift<'a, A: 'a>(i: Op<A>) -> Program<'a, A> {
//     Program::Step(Box::new(Step(i, Box::new(Program::Return))))
// }
