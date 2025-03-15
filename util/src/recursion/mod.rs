// use crate::recursion::compose::Compose;
// use recursion::{Collapsible, Expandable, MappableFrame, PartiallyApplied};
//
// pub mod compose;
// pub mod fix;
// pub mod list;
//
// // attempting to do mutual recursion on Even/Odd definitions
// // example here: https://github.com/recursion-schemes/recursion-schemes/issues/9
// // https://stackoverflow.com/questions/51073110/recursion-schemes-with-several-types
// // https://www.reddit.com/r/haskell/comments/3sm1j1/how_to_mix_the_base_functorrecursion_scheme_stuff/
// //
//
// #[derive(Clone, Debug)]
// enum Even {
//     Zero,
//     Odd(usize, Box<Odd>),
// }
//
// #[derive(Clone, Debug)]
// enum Odd {
//     One(usize),
//     Even(usize, Box<Even>),
// }
//
// // split these two into reusable parts
// enum RawEvenFrame<A> {
//     ZeroFrame,
//     OddFrame(usize, A),
// }
//
// enum RawOddFrame<A> {
//     OneFrame(usize),
//     EvenFrame(usize, A),
// }
//
// impl MappableFrame for RawEvenFrame<PartiallyApplied> {
//     type Frame<X> = RawEvenFrame<X>;
//
//     fn map_frame<A, B>(input: Self::Frame<A>, mut f: impl FnMut(A) -> B) -> Self::Frame<B> {
//         match input {
//             RawEvenFrame::ZeroFrame => RawEvenFrame::ZeroFrame,
//             RawEvenFrame::OddFrame(n, a) => RawEvenFrame::OddFrame(n, f(a)),
//         }
//     }
// }
//
// impl MappableFrame for RawOddFrame<PartiallyApplied> {
//     type Frame<X> = RawOddFrame<X>;
//
//     fn map_frame<A, B>(input: Self::Frame<A>, mut f: impl FnMut(A) -> B) -> Self::Frame<B> {
//         match input {
//             RawOddFrame::OneFrame(n) => RawOddFrame::OneFrame(n),
//             RawOddFrame::EvenFrame(n, a) => RawOddFrame::EvenFrame(n, f(a)),
//         }
//     }
// }
//
// type EvenFrame<A> = Compose<RawEvenFrame<PartiallyApplied>, RawOddFrame<PartiallyApplied>, A>;
// type OddFrame<A> = Compose<RawOddFrame<PartiallyApplied>, RawEvenFrame<PartiallyApplied>, A>;
//
// impl Collapsible for &'_ Even {
//     type FrameToken = EvenFrame<PartiallyApplied>;
//
//     #[inline]
//     fn into_frame(self) -> <Self::FrameToken as MappableFrame>::Frame<Self> {
//         match *self {
//             Even::Zero => RawEvenFrame::ZeroFrame,
//             Even::Odd(ref _n, ref _odd) => todo!(), // RawEvenFrame::OddFrame(*n, s)
//         }
//     }
// }
//
// impl Expandable for Even {
//     type FrameToken = EvenFrame<PartiallyApplied>;
//
//     fn from_frame(val: <Self::FrameToken as MappableFrame>::Frame<Self>) -> Self {
//         match val {
//             RawEvenFrame::ZeroFrame => Even::Zero,
//             RawEvenFrame::OddFrame(_n, _odd) => todo!(),
//         }
//     }
// }
//
// impl Collapsible for &'_ Odd {
//     type FrameToken = OddFrame<PartiallyApplied>;
//
//     #[inline]
//     fn into_frame(self) -> <Self::FrameToken as MappableFrame>::Frame<Self> {
//         match *self {
//             Odd::One(ref n) => RawOddFrame::OneFrame(*n),
//             Odd::Even(ref _n, ref _even) => todo!(), // need RawEvenFrame<&Odd> found RawEvenFrame<RawOddFrame<&Even>>
//         }
//     }
// }
//
// impl Expandable for Odd {
//     type FrameToken = OddFrame<PartiallyApplied>;
//
//     fn from_frame(val: <Self::FrameToken as MappableFrame>::Frame<Self>) -> Self {
//         match val {
//             RawOddFrame::OneFrame(n) => Odd::One(n),
//             RawOddFrame::EvenFrame(_n, _even) => todo!(),
//         }
//     }
// }
