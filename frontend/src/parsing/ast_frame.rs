// TODO: either actually implement mutually recursive recursion-schemes, or delete this

// #![allow(clippy::arbitrary_source_item_ordering)]
//
// use crate::parsing::ast::{BinaryOper, Ident, Liter, UnaryOper};
// use crate::source::{SourcedBoxedNode, SourcedNode, SourcedSpan};
// use std::fmt::Debug;
// use util::nonempty::NonemptyArray;
//
// // A file-local type alias for better readability of type definitions
// type SN<T> = SourcedNode<T>;
// type SBN<T> = SourcedBoxedNode<T>;
//
// // #[derive(Clone, Debug)]
// // pub struct Program {
// //     pub funcs: Box<[Func]>,
// //     pub body: StatBlock,
// // }
// //
// // #[derive(Clone, Debug)]
// // pub struct Func {
// //     pub return_type: SN<Type>,
// //     pub name: SN<Ident>,
// //     pub params: Box<[FuncParam]>,
// //     pub body: StatBlock,
// // }
// //
// // #[derive(Clone, Debug)]
// // pub struct FuncParam {
// //     pub r#type: SN<Type>,
// //     pub name: SN<Ident>,
// // }
// //
// // #[derive(Clone, Debug)]
// // #[repr(transparent)]
// // pub struct StatBlock(pub NonemptyArray<SN<Stat>>);
// //
// // #[derive(Error, Debug)]
// // #[error("Cannot create to `StatBlock` because the supplied `Vec<Stat>` is empty")]
// // pub struct EmptyStatVecError;
// //
// // #[derive(Clone, Debug)]
// // pub enum Stat {
// //     Skip,
// //     VarDefinition {
// //         r#type: SN<Type>,
// //         name: SN<Ident>,
// //         rvalue: SN<RValue>,
// //     },
// //     Assignment {
// //         lvalue: SN<LValue>,
// //         rvalue: SN<RValue>,
// //     },
// //     Read(SN<LValue>),
// //     Free(SN<Expr>),
// //     Return(SN<Expr>),
// //     Exit(SN<Expr>),
// //     Print(SN<Expr>),
// //     Println(SN<Expr>),
// //     IfThenElse {
// //         if_cond: SN<Expr>,
// //         then_body: StatBlock,
// //         else_body: StatBlock,
// //     },
// //     WhileDo {
// //         while_cond: SN<Expr>,
// //         body: StatBlock,
// //     },
// //     Scoped(StatBlock),
// // }
// //
// // #[derive(Clone, Debug)]
// // pub enum LValue {
// //     Ident(SN<Ident>),
// //     ArrayElem(SN<ArrayElem>),
// //     PairElem(SN<PairElem>),
// // }
// //
// // #[derive(Clone, Debug)]
// // pub enum RValue {
// //     Expr(SN<Expr>),
// //     ArrayLiter(Box<[SN<Expr>]>),
// //     NewPair(SN<Expr>, SN<Expr>),
// //     PairElem(SN<PairElem>),
// //     Call {
// //         func_name: SN<Ident>,
// //         args: Box<[SN<Expr>]>,
// //     },
// // }
// //
// // #[derive(Clone, Debug)]
// // #[repr(u8)]
// // pub enum PairElemSelector {
// //     Fst,
// //     Snd,
// // }
// //
// // #[derive(Clone, Debug)]
// // pub struct PairElem(pub PairElemSelector, pub SBN<LValue>);
//
// #[derive(Clone, Debug)]
// pub struct ArrayElemFrame<A> {
//     pub array_name: SN<Ident>,
//     pub indices: NonemptyArray<SN<A>>,
// }
//
// // SBN<T> -> BoxedNode<T, SourcedSpan> -> Node<Box<T>, SourcedSpan>
// // Node<A, SourcedSpan> -> SN<A>
// #[derive(Clone, Debug)]
// pub enum ExprFrame<A> {
//     Liter(SN<Liter>),
//     Ident(SN<Ident>),
//     ArrayElem(SN<ArrayElemFrame<A>>),
//     Unary(SN<UnaryOper>, SN<A>),
//     Binary(SN<A>, SN<BinaryOper>, SN<A>),
//     Paren(SN<A>),
//     IfThenElse {
//         if_cond: SN<A>,
//         then_val: SN<A>,
//         else_val: SN<A>,
//     },
//
//     // Generated only by parser errors.
//     Error(SourcedSpan),
// }
//
// // #[derive(Clone, Debug)]
// // pub enum Type {
// //     BaseType(SN<BaseType>),
// //     ArrayType(SBN<ArrayType>),
// //     PairType(PairElemType, PairElemType),
// //
// //     // Generated only by parser errors.
// //     Error(SourcedSpan),
// // }
// //
// // #[derive(Clone, Debug)]
// // pub enum BaseType {
// //     Int,
// //     Bool,
// //     Char,
// //     String,
// // }
// //
// // #[derive(Clone, Debug)]
// // pub struct ArrayType {
// //     pub elem_type: Type,
// // }
// //
// // #[derive(Clone, Debug)]
// // pub enum PairElemType {
// //     ArrayType(SBN<ArrayType>),
// //     BaseType(SN<BaseType>),
// //     Pair(SourcedSpan),
// // }
//
// mod impls {
//     use crate::parsing::ast::{ArrayElem, Expr};
//     use crate::parsing::ast_frame::{ArrayElemFrame, ExprFrame};
//     use recursion::{Collapsible, Expandable, MappableFrame, PartiallyApplied};
//
//     impl MappableFrame for ArrayElemFrame<PartiallyApplied> {
//         type Frame<X> = ArrayElemFrame<X>;
//
//         #[inline]
//         fn map_frame<A, B>(input: Self::Frame<A>, mut f: impl FnMut(A) -> B) -> Self::Frame<B> {
//             ArrayElemFrame {
//                 array_name: input.array_name,
//                 indices: input.indices.map(|e| e.map_inner(&mut f)),
//             }
//         }
//     }
//
//     impl MappableFrame for ExprFrame<PartiallyApplied> {
//         type Frame<X> = ExprFrame<X>;
//
//         #[inline]
//         fn map_frame<A, B>(input: Self::Frame<A>, mut f: impl FnMut(A) -> B) -> Self::Frame<B> {
//             match input {
//                 ExprFrame::Liter(l) => ExprFrame::Liter(l),
//                 ExprFrame::Ident(i) => ExprFrame::Ident(i),
//                 ExprFrame::ArrayElem(a) => ExprFrame::ArrayElem(
//                     a.map_inner(|a| ArrayElemFrame::<PartiallyApplied>::map_frame(a, f)),
//                 ),
//                 ExprFrame::Unary(op, e) => ExprFrame::Unary(op, e.map_inner(&mut f)),
//                 ExprFrame::Binary(le, op, re) => {
//                     ExprFrame::Binary(le.map_inner(&mut f), op, re.map_inner(&mut f))
//                 }
//                 ExprFrame::Paren(e) => ExprFrame::Paren(e.map_inner(&mut f)),
//                 ExprFrame::IfThenElse {
//                     if_cond,
//                     then_val,
//                     else_val,
//                 } => ExprFrame::IfThenElse {
//                     if_cond: if_cond.map_inner(&mut f),
//                     then_val: then_val.map_inner(&mut f),
//                     else_val: else_val.map_inner(&mut f),
//                 },
//                 ExprFrame::Error(e) => ExprFrame::Error(e),
//             }
//         }
//     }
//
//     impl Collapsible for &'_ ArrayElem {
//         type FrameToken = ArrayElemFrame<PartiallyApplied>;
//
//         #[inline]
//         fn into_frame(self) -> <Self::FrameToken as MappableFrame>::Frame<Self> {
//             let ArrayElem {
//                 array_name,
//                 indices,
//             } = self;
//
//             todo!()
//         }
//     }
//
//     impl Collapsible for &'_ Expr {
//         type FrameToken = ExprFrame<PartiallyApplied>;
//
//         #[inline]
//         fn into_frame(self) -> <Self::FrameToken as MappableFrame>::Frame<Self> {
//             match *self {
//                 Expr::Liter(ref l) => ExprFrame::Liter(l.clone()),
//                 Expr::Ident(ref i) => ExprFrame::Ident(i.clone()),
//                 Expr::ArrayElem(ref a) => unimplemented!(),
//                 Expr::Unary(ref op, ref e) => {
//                     ExprFrame::Unary(op.clone(), e.transpose_ref_unboxed())
//                 }
//                 Expr::Binary(ref le, ref op, ref re) => ExprFrame::Binary(
//                     le.transpose_ref_unboxed(),
//                     op.clone(),
//                     re.transpose_ref_unboxed(),
//                 ),
//                 Expr::Paren(ref e) => ExprFrame::Paren(e.transpose_ref_unboxed()),
//                 Expr::IfThenElse {
//                     ref if_cond,
//                     ref then_val,
//                     ref else_val,
//                 } => ExprFrame::IfThenElse {
//                     if_cond: if_cond.transpose_ref_unboxed(),
//                     then_val: then_val.transpose_ref_unboxed(),
//                     else_val: else_val.transpose_ref_unboxed(),
//                 },
//                 Expr::Error(ref e) => ExprFrame::Error(e.clone()),
//             }
//         }
//     }
//
//     impl Expandable for Expr {
//         type FrameToken = ExprFrame<PartiallyApplied>;
//
//         #[inline]
//         fn from_frame(val: <Self::FrameToken as MappableFrame>::Frame<Self>) -> Self {
//             match val {
//                 ExprFrame::Liter(l) => Self::Liter(l),
//                 ExprFrame::ArrayElem(a) => todo!(),
//                 ExprFrame::Ident(i) => Self::Ident(i),
//                 ExprFrame::Unary(op, e) => Self::Unary(op, e.box_inner()),
//                 ExprFrame::Binary(le, op, re) => Self::Binary(le.box_inner(), op, re.box_inner()),
//                 ExprFrame::Paren(e) => Self::Paren(e.box_inner()),
//                 ExprFrame::IfThenElse {
//                     if_cond,
//                     then_val,
//                     else_val,
//                 } => Self::IfThenElse {
//                     if_cond: if_cond.box_inner(),
//                     then_val: then_val.box_inner(),
//                     else_val: else_val.box_inner(),
//                 },
//                 ExprFrame::Error(e) => Self::Error(e),
//             }
//         }
//     }
// }
