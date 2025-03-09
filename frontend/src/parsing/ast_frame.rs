// use crate::parsing::ast;
// use crate::source::{SourcedNode, SourcedSpan};
//
// // A file-local type alias for better readability of type definitions
// type SN<T> = SourcedNode<T>;
// type ST<T> = (T, SourcedSpan);
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
// // pub struct PairElem(pub PairElemSelector, pub SN<LValue>);
// //
// // #[derive(Clone, Debug)]
// // pub struct ArrayElem {
// //     pub array_name: SN<ast::Ident>,
// //     pub indices: NonemptyArray<SN<Expr>>,
// // }
//
// #[derive(Clone, Debug)]
// pub enum ExprFrame<A> {
//     Liter(SN<ast::Liter>),
//     Ident(SN<ast::Ident>),
//     // ArrayElem(SN<ArrayElem>), // TODO: add later
//     Unary(SN<ast::UnaryOper>, A),
//     Binary(A, SN<ast::BinaryOper>, A),
//     Paren(A),
//     IfThenElse {
//         if_cond: A,
//         then_val: A,
//         else_val: A,
//     },
//
//     // Generated only by parser errors.
//     Error(SourcedSpan),
// }
//
// // #[derive(Clone, Debug)]
// // pub enum Type {
// //     BaseType(SN<BaseType>),
// //     ArrayType(SN<ArrayType>),
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
// //     ArrayType(SN<ArrayType>),
// //     BaseType(SN<BaseType>),
// //     Pair(SourcedSpan),
// // }
//
// mod impls {
//     use crate::parsing::ast::Expr;
//     use crate::parsing::ast_frame::ExprFrame;
//     use recursion::{Collapsible, Expandable, MappableFrame, PartiallyApplied};
//
//     impl MappableFrame for ExprFrame<PartiallyApplied> {
//         type Frame<X> = ExprFrame<X>;
//
//         fn map_frame<A, B>(input: Self::Frame<A>, f: impl FnMut(A) -> B) -> Self::Frame<B> {
//             todo!()
//         }
//     }
//
//     impl<'a> Collapsible for &'a Expr {
//         type FrameToken = ExprFrame<PartiallyApplied>;
//
//         fn into_frame(self) -> <Self::FrameToken as MappableFrame>::Frame<Self> {
//             match self {
//                 Expr::Liter(l) => ExprFrame::Liter(l.clone()),
//                 Expr::Ident(i) => ExprFrame::Ident(i.clone()),
//                 Expr::ArrayElem(_) => unimplemented!(),
//                 Expr::Unary(op, e) => ExprFrame::Unary(op.clone(), &*e),
//                 Expr::Binary(_, _, _) => {}
//                 Expr::Paren(_) => {}
//                 Expr::IfThenElse { .. } => {}
//                 Expr::Error(_) => {}
//             }
//         }
//     }
// }
