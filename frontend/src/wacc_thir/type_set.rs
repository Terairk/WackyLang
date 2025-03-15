// use crate::wacc_thir::types::{BaseType, Type};
// use std::collections::HashSet;
// use std::hash::Hash;
// use std::rc::Rc;
//
// /// A finite set of types is represented by a boxed-slice of types.
// type FiniteTypeSet = HashSet<Type>;
//
// /// A set of [`Type`]s
// #[derive(Clone, PartialEq, Eq, Debug)]
// pub enum TypeSet {
//     /// A finite set, defined by its collection of elements.
//     Finite(FiniteTypeSet),
//
//     /// A (potentially infinite) set, defined by a predicate function.
//     Predicate(TypePredicate),
// }
//
// /// A language for constructing predicate-functions from [`Type`]s to [`bool`]s,
// /// which can used to define a (potentially infinite) subset of [`Type`]s.
// #[derive(Clone, PartialEq, Eq, Debug)]
// pub enum TypePredicate {
//     /// Constant truth-value, either `true` or `false`, regardless of the input [`Type`]s.
//     Bool(bool),
//
//     /// A predicate which succeeds only if the input [`Type`] matches __both__ sub-predicates.
//     And(Box<TypePredicate>, Box<TypePredicate>),
//
//     /// A predicate which succeeds if the input [`Type`] matches __at least one__ of the sub-predicates.
//     Or(Box<TypePredicate>, Box<TypePredicate>),
//
//     /// A predicate which succeeds if the input [`Type`] is exactly-equal to the specified [`Type`].
//     Equals(Type),
//
//     /// A predicate which succeeds if the input [`Type`] is an element of the specified finite set.
//     ElementOf(Rc<FiniteTypeSet>),
//
//     /// Predicate to check if the input [`Type`] is _precisely_ one of the [`BaseType`]s.
//     BaseType(BaseType),
//
//     /// Predicate that matches any [`ArrayType`] such that its element-type matches its own inner predicate.
//     ArrayType { elem_predicate: Box<TypePredicate> },
//
//     /// Predicate that matches any [`PairType`] such that __both__ of its element-types
//     /// matches their respective inner predicates.
//     PairType {
//         fst_predicate: Box<TypePredicate>,
//         snd_predicate: Box<TypePredicate>,
//     },
// }
//
// #[derive(Clone, PartialEq, Eq, Debug)]
// pub enum TypePredicateFrame<A> {
//     Bool(bool),
//     And(A, A),
//     Or(A, A),
//     Equals(Type),
//     ElementOf(Rc<FiniteTypeSet>),
//     BaseType(BaseType),
//     ArrayType { elem_predicate: A },
//     PairType { fst_predicate: A, snd_predicate: A },
// }
//
// mod impls {
//     use crate::wacc_thir::type_set::{
//         intersections_slice, FiniteTypeSet, TypePredicate, TypePredicateFrame, TypeSet,
//     };
//     use crate::wacc_thir::types::{Type};
//     use recursion::{Collapsible, Expandable, MappableFrame, PartiallyApplied};
//
//     impl TypeSet {
//         #[inline]
//         #[must_use]
//         pub fn intersection(self, other: Self) -> Self {
//             match (self, other) {
//                 // handle finite-only intersection by manually intersecting the hash-sets of elements
//                 (Self::Finite(a), Self::Finite(b)) => Self::Finite(intersections_slice(&[&a, &b])),
//
//                 // handle predicate-only intersection by creating a new predicate which contains BOTH of the sub-predicates
//                 (Self::Predicate(a), Self::Predicate(b)) => Self::Predicate(a.and(b)),
//
//                 // in case of predicate-finite intersection, filter finite hash-set by the predicate
//                 (Self::Finite(fin), Self::Predicate(p))
//                 | (Self::Predicate(p), Self::Finite(fin)) => Self::Finite(p.select_from(fin)),
//             }
//         }
//
//         #[inline]
//         #[must_use]
//         pub fn union(self, other: Self) -> Self {
//             match (self, other) {
//                 // handle finite-only union by manually intersecting the hash-sets of elements
//                 (Self::Finite(mut accum), Self::Finite(other)) => {
//                     // extend mutably, and return
//                     accum.extend(other);
//                     Self::Finite(accum)
//                 }
//
//                 // handle predicate-only union by creating a new predicate which contains BOTH of the sub-predicates
//                 (Self::Predicate(a), Self::Predicate(b)) => Self::Predicate(a.or(b)),
//
//                 // in case of predicate-finite union, filter finite hash-set by the predicate
//                 (Self::Finite(fin), Self::Predicate(p))
//                 | (Self::Predicate(p), Self::Finite(fin)) => Self::Finite(p.select_from(fin)),
//             }
//         }
//     }
//
//     impl TypePredicate {
//         /// Predicate to check if the input [`Type`] is _precisely_ the [`Type::Any`] type.
//         pub const IS_ANY: Self = Self::Equals(Type::Any);
//
//         /// Predicate to check if the input [`Type`] is an [`PairType::ERASED_PAIR`] type.
//         pub fn is_erased_pair() -> Self {
//             Self::Equals(Type::erased_pair())
//         }
//
//         #[inline]
//         #[must_use]
//         pub fn and(self, other: Self) -> Self {
//             Self::And(Box::new(self), Box::new(other))
//         }
//
//         #[inline]
//         #[must_use]
//         pub fn or(self, other: Self) -> Self {
//             Self::Or(Box::new(self), Box::new(other))
//         }
//
//         #[inline]
//         #[must_use]
//         pub fn array_from_elem_predicate(self) -> Self {
//             Self::ArrayType {
//                 elem_predicate: Box::new(self),
//             }
//         }
//
//         #[inline]
//         #[must_use]
//         pub fn pair_from_elem_predicates(self, other: Self) -> Self {
//             Self::PairType {
//                 fst_predicate: Box::new(self),
//                 snd_predicate: Box::new(other),
//             }
//         }
//
//         #[inline]
//         #[must_use]
//         pub fn eval(&self, r#type: &Type) -> bool {
//             match self {
//                 Self::Bool(b) => *b,
//                 Self::And(a, b) => a.eval(r#type) && b.eval(r#type),
//                 Self::Or(a, b) => a.eval(r#type) || b.eval(r#type),
//                 Self::Equals(t) => t == r#type,
//                 Self::ElementOf(s) => s.contains(r#type),
//                 Self::BaseType(a) => {
//                     // check that the provided type is a base-type
//                     // and matches _exactly_ the expected base-type
//                     if let Type::BaseType(b) = r#type {
//                         a == b
//                     } else {
//                         false
//                     }
//                 }
//                 Self::ArrayType { elem_predicate } => {
//                     // check that the current type is an array-type, and if so
//                     // return the result of evaluating the inner predicate
//                     if let Type::ArrayType(a) = r#type {
//                         elem_predicate.eval(&a.elem_type)
//                     } else {
//                         false
//                     }
//                 }
//                 Self::PairType {
//                     fst_predicate,
//                     snd_predicate,
//                 } => {
//                     // check that the current type is a pair-type, and if so
//                     // return the CONJUNCTION of results of evaluating the inner predicates
//                     if let Type::PairType(p) = r#type {
//                         fst_predicate.eval(&p.fst_type) && snd_predicate.eval(&p.snd_type)
//                     } else {
//                         false
//                     }
//                 }
//             }
//         }
//
//         #[inline]
//         #[must_use]
//         pub fn select_from(&self, fin: FiniteTypeSet) -> FiniteTypeSet {
//             fin.into_iter().filter(|t| self.eval(t)).collect()
//         }
//     }
//
//     impl MappableFrame for TypePredicateFrame<PartiallyApplied> {
//         type Frame<X> = TypePredicateFrame<X>;
//
//         #[inline]
//         fn map_frame<A, B>(input: Self::Frame<A>, mut f: impl FnMut(A) -> B) -> Self::Frame<B> {
//             match input {
//                 TypePredicateFrame::Bool(b) => TypePredicateFrame::Bool(b),
//                 TypePredicateFrame::And(a, b) => TypePredicateFrame::And(f(a), f(b)),
//                 TypePredicateFrame::Or(a, b) => TypePredicateFrame::Or(f(a), f(b)),
//                 TypePredicateFrame::Equals(t) => TypePredicateFrame::Equals(t),
//                 TypePredicateFrame::ElementOf(s) => TypePredicateFrame::ElementOf(s),
//                 TypePredicateFrame::BaseType(b) => TypePredicateFrame::BaseType(b),
//                 TypePredicateFrame::ArrayType { elem_predicate } => TypePredicateFrame::ArrayType {
//                     elem_predicate: f(elem_predicate),
//                 },
//                 TypePredicateFrame::PairType {
//                     fst_predicate,
//                     snd_predicate,
//                 } => TypePredicateFrame::PairType {
//                     fst_predicate: f(fst_predicate),
//                     snd_predicate: f(snd_predicate),
//                 },
//             }
//         }
//     }
//
//     impl Collapsible for &'_ TypePredicate {
//         type FrameToken = TypePredicateFrame<PartiallyApplied>;
//
//         #[inline]
//         fn into_frame(self) -> <Self::FrameToken as MappableFrame>::Frame<Self> {
//             match self {
//                 TypePredicate::Bool(b) => TypePredicateFrame::Bool(*b),
//                 TypePredicate::And(a, b) => TypePredicateFrame::And(a, b),
//                 TypePredicate::Or(a, b) => TypePredicateFrame::Or(a, b),
//                 TypePredicate::Equals(t) => TypePredicateFrame::Equals(t.clone()),
//                 TypePredicate::ElementOf(s) => TypePredicateFrame::ElementOf(s.clone()),
//                 TypePredicate::BaseType(b) => TypePredicateFrame::BaseType(b.clone()),
//                 TypePredicate::ArrayType { elem_predicate } => {
//                     TypePredicateFrame::ArrayType { elem_predicate }
//                 }
//                 TypePredicate::PairType {
//                     fst_predicate,
//                     snd_predicate,
//                 } => TypePredicateFrame::PairType {
//                     fst_predicate,
//                     snd_predicate,
//                 },
//             }
//         }
//     }
//
//     impl Expandable for TypePredicate {
//         type FrameToken = TypePredicateFrame<PartiallyApplied>;
//
//         #[inline]
//         fn from_frame(val: <Self::FrameToken as MappableFrame>::Frame<Self>) -> Self {
//             match val {
//                 TypePredicateFrame::Bool(b) => Self::Bool(b),
//                 TypePredicateFrame::And(a, b) => a.and(b),
//                 TypePredicateFrame::Or(a, b) => a.or(b),
//                 TypePredicateFrame::Equals(t) => TypePredicate::Equals(t),
//                 TypePredicateFrame::ElementOf(s) => TypePredicate::ElementOf(s),
//                 TypePredicateFrame::BaseType(b) => Self::BaseType(b),
//                 TypePredicateFrame::ArrayType { elem_predicate } => {
//                     elem_predicate.array_from_elem_predicate()
//                 }
//                 TypePredicateFrame::PairType {
//                     fst_predicate,
//                     snd_predicate,
//                 } => fst_predicate.pair_from_elem_predicates(snd_predicate),
//             }
//         }
//     }
// }
//
// /// Helper function for implementing an intersection of a slice of [`HashSet`]s.
// fn intersections_slice<T>(sets: &[&HashSet<T>]) -> HashSet<T>
// where
//     T: Clone + Eq + Hash,
// {
//     match sets.len() {
//         0 => HashSet::new(),
//         _ => sets[1..].iter().fold(sets[0].clone(), |mut acc, set| {
//             acc.retain(|item| set.contains(item));
//             acc
//         }),
//     }
// }
//
// /// Helper function for implementing an intersection of an iterator of [`HashSet`]s.
// fn intersections_iter<'a, T>(mut sets: impl Iterator<Item = &'a HashSet<T>>) -> HashSet<T>
// where
//     T: Clone + Eq + Hash + 'a,
// {
//     sets.next().map_or_else(HashSet::new, |first| {
//         sets.fold(first.clone(), |mut acc, set| {
//             acc.retain(|item| set.contains(item));
//             acc
//         })
//     })
// }
