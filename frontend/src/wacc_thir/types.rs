//! This module defines type-level as well as value-level type-variants, in order to be able to
//! perform convenient type-level compile-time computations

#![allow(clippy::inline_always)]

use crate::private;
use std::cmp;

#[allow(clippy::enum_variant_names)]
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    BaseType(BaseType),
    ArrayType(Box<ArrayType>),
    PairType(Box<PairType>),

    /// Type representing unknown/potentially erroneous types
    Any,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum BaseType {
    Int,
    Bool,
    Char,
    String,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ArrayType {
    pub elem_type: Type,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct PairType {
    pub fst_type: Type,
    pub snd_type: Type,
}

/// Define trait for all "sub"-types which can be "equal-to" [`Type`]s. This allows for removing
/// the artificial variance of constructors and nesting, when logically comparing type equality.
pub trait Equality: Into<Type> + private::Sealed {
    #[inline(always)]
    fn ops(self) -> TypeOps<Self> {
        TypeOps(self)
    }

    #[inline(always)]
    fn equal_to<Target>(&self, target: &Target) -> bool
    where
        Self: Clone,
        Target: Equality + Clone,
    {
        self.to_type() == target.to_type()
    }

    #[inline(always)]
    fn to_type(&self) -> Type
    where
        Self: Clone,
    {
        self.clone().into()
    }
}

pub trait Specificity: Equality {
    /// See [`Type::partial_specificity_cmp`].
    #[inline(always)]
    fn partial_specificity_cmp<Target>(&self, target: &Target) -> Option<cmp::Ordering>
    where
        Self: Clone,
        Target: Equality + Clone,
    {
        Type::partial_specificity_cmp(&self.to_type(), &target.to_type())
    }

    /// See [`Type::greatest_lower_bound`].
    #[inline(always)]
    fn greatest_lower_bound<Target>(&self, target: &Target) -> Option<Type>
    where
        Self: Clone,
        Target: Equality + Clone,
    {
        Type::greatest_lower_bound(&self.to_type(), &target.to_type())
    }
}

pub trait Weakening: Equality {
    /// See [`Type::weakens_to`].
    #[inline(always)]
    fn weakens_to<Target>(&self, target: &Target) -> bool
    where
        Self: Clone,
        Target: Equality + Clone,
    {
        Type::weakens_to(&self.to_type(), &target.to_type())
    }
}

/// A type is compatible with another type if-and-only if:
/// 1) it is equal to the target type, or
/// 2) it weakens to the target type.
pub trait Compatibility: Equality {
    /// See [`Type::compatible_with`].
    #[inline(always)]
    fn compatible_with<Target>(&self, target: &Target) -> bool
    where
        Self: Clone,
        Target: Equality + Clone,
    {
        Type::compatible_with(&self.to_type(), &target.to_type())
    }
}

/// A newtype for any type-equal objects which implements various overloadable operations like
/// [`PartialEq`], [`PartialOrd`] and so on based on the [`Equality`], [`Specificity`], etc., definitions.
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct TypeOps<T>(pub T);

mod impls {
    use crate::wacc_thir::types::{
        ArrayType, BaseType, Compatibility, Equality, PairType, Specificity, Type, TypeOps,
        Weakening,
    };
    use std::fmt::{Display, Formatter};
    use std::ops::Add;
    use std::{cmp, fmt};

    impl Type {
        pub const INT: Self = Self::BaseType(BaseType::Int);
        pub const BOOL: Self = Self::BaseType(BaseType::Bool);
        pub const CHAR: Self = Self::BaseType(BaseType::Char);
        pub const STRING: Self = Self::BaseType(BaseType::String);

        /// See [`ArrayType::UNKNOWN_ARRAY`].
        #[must_use]
        #[inline]
        pub fn unknown_array() -> Type {
            Self::ArrayType(Box::new(ArrayType::UNKNOWN_ARRAY))
        }

        /// See [`ArrayType::CHAR_ARRAY`].
        #[must_use]
        #[inline]
        pub fn char_array() -> Type {
            Self::ArrayType(Box::new(ArrayType::CHAR_ARRAY))
        }

        /// See [`PairType::ERASED_PAIR`].
        #[must_use]
        #[inline]
        pub fn erased_pair() -> Self {
            Self::PairType(Box::new(PairType::ERASED_PAIR))
        }

        #[must_use]
        #[inline]
        pub fn from_array_type(array_type: ArrayType) -> Self {
            Self::ArrayType(Box::new(array_type))
        }

        #[must_use]
        #[inline]
        pub fn from_pair_type(pair_type: PairType) -> Self {
            Self::PairType(Box::new(pair_type))
        }

        /// See [`ArrayType::new`].
        #[must_use]
        #[inline]
        pub fn array_type(elem_type: Self) -> Self {
            Self::from_array_type(ArrayType::new(elem_type))
        }

        /// See [`PairType::new`].
        #[must_use]
        #[inline]
        pub fn pair_type(fst_type: Self, snd_type: Self) -> Self {
            Self::from_pair_type(PairType::new(fst_type, snd_type))
        }

        /// A partial ordering of the specificity of [`Type`]s, where more __specific__ [`Type`]s
        /// are [`cmp::Ordering::Less`] than more __general__ [`Type`]s.
        #[inline]
        #[must_use]
        pub fn partial_specificity_cmp(&self, target: &Self) -> Option<cmp::Ordering> {
            // short circuit if types are exactly equal to each other
            if self == target {
                return Some(cmp::Ordering::Equal);
            }

            // Otherwise, try to determine if they are less-than or greater-than
            match (self, target) {
                // `Any`-type is always on top i.e. it is the least specific type of all types
                (_, Self::Any) => Some(cmp::Ordering::Less),
                (Self::Any, _) => Some(cmp::Ordering::Greater),

                // Specificity of array types is directly dependent on the specificity of their
                // inner element types, i.e. `t1 <= t2  <=>  array(t1) <= array(t2)`
                (Self::ArrayType(self_boxed), Self::ArrayType(target_boxed)) => {
                    let self_elem = &self_boxed.elem_type;
                    let target_elem = &target_boxed.elem_type;
                    self_elem.partial_specificity_cmp(target_elem)
                }

                // Specificity of pair types is directly dependent on the specificities of BOTH
                // of their respective element types,
                // i.e. `a1 <= a2 && b1 <= b2  <=>  pair(a1, b1) <= pair(a2, b2)`
                //
                // Notice this definition does not allow parameters to vary independently in order
                // to preserve the antisymmetric property of partial orders
                (Self::PairType(self_boxed), Self::PairType(target_boxed)) => {
                    // dereference boxed types
                    let self_ref = self_boxed.as_ref();
                    let target_ref = target_boxed.as_ref();

                    // ensure both inner elements are comparable
                    let fst_cmp = self_ref
                        .fst_type
                        .partial_specificity_cmp(&target_ref.fst_type)?;
                    let snd_cmp = self_ref
                        .snd_type
                        .partial_specificity_cmp(&target_ref.snd_type)?;

                    //
                    match (fst_cmp, snd_cmp) {
                        // `a1 < a2 && b1 < b2  <=>  pair(a1, b1) < pair(a2, b2)` and
                        // `a1 < a2 && b1 = b2  <=>  pair(a1, b1) < pair(a2, b2)`
                        (cmp::Ordering::Less, cmp::Ordering::Less | cmp::Ordering::Equal) => {
                            Some(cmp::Ordering::Less)
                        }

                        // `a1 > a2 && b1 > b2  <=>  pair(a1, b1) > pair(a2, b2)` and
                        // `a1 > a2 && b1 = b2  <=>  pair(a1, b1) > pair(a2, b2)`
                        (cmp::Ordering::Greater, cmp::Ordering::Greater | cmp::Ordering::Equal) => {
                            Some(cmp::Ordering::Greater)
                        }

                        // `a1 = a2 && b1 < b2  <=>  pair(a1, b1) < pair(a2, b2)` and
                        // `a1 = a2 && b1 = b2  <=>  pair(a1, b1) = pair(a2, b2)` and
                        // `a1 = a2 && b1 > b2  <=>  pair(a1, b1) > pair(a2, b2)`
                        (cmp::Ordering::Equal, _) => Some(snd_cmp),

                        // if `a1 < a2 && b1 > b2`, then `pair(a1, b1)` and `pair(a2, b2)` are
                        // incomparable, e.g. `pair(int, any)` not comparable to `pair(any, int)`
                        //
                        // similarly if `a1 > a2 && b1 < b2`, then `pair(a1, b1)` and `pair(a2, b2)`
                        // are incomparable, e.g. `pair(any, int)` not comparable to `pair(int, any)`
                        (cmp::Ordering::Less, cmp::Ordering::Greater)
                        | (cmp::Ordering::Greater, cmp::Ordering::Less) => None,
                    }
                }

                // any other types are incomparable
                _ => None,
            }
        }

        /// Finds a Greatest-Lower-Bound (GLB) on two [`Type`]s if it exists, according to the type-[`Specificity`]
        /// partial order; where more __specific__ [`Type`]s are [`cmp::Ordering::Less`] than more __general__ [`Type`]s.
        ///
        /// This means `GLB(t1, t2)` (if it exists) will be the __most general type__ that is
        /// __at least__ as specific as __both__ `t1` and `t2` (hence Greatest-Lower-Bound).
        ///
        /// For example, for `t1 = pair(any, int)` and `t2 = pair(int, any)` we have `GLB(t1, t2) = pair(int, int)`.
        #[must_use]
        #[inline]
        pub fn greatest_lower_bound(&self, other: &Self) -> Option<Self> {
            match self.partial_specificity_cmp(other) {
                // handle the easy case that they are directly comparable
                Some(cmp) => Some(match cmp {
                    // if `t1 <= t2` then `t1` is the greatest-lower-bound s.t. `t1 <= t1, t2`
                    cmp::Ordering::Less | cmp::Ordering::Equal => self.clone(),
                    // if `t1 > t2` then `t2` is the greatest-lower-bound s.t. `t2 <= t1, t2`
                    cmp::Ordering::Greater => other.clone(),
                }),

                // handle the case of incomparable types
                None => match (self, other) {
                    // an array-type can only have a Greatest-Lower-Bound (GLB) with another
                    // array type, and only if their respective inner element types have a GLB
                    (Self::ArrayType(self_boxed), Self::ArrayType(target_boxed)) => {
                        // ensure inner elements have a GLB
                        let inner_glb = Self::greatest_lower_bound(
                            &self_boxed.elem_type,
                            &target_boxed.elem_type,
                        )?;

                        // construct array-GLB
                        Some(Self::array_type(inner_glb))
                    }

                    // a pair-type can only have a Greatest-Lower-Bound (GLB) with another
                    // pair type, and only if BOTH of their respective inner element types have GLBs
                    (Self::PairType(self_boxed), Self::PairType(target_boxed)) => {
                        // dereference boxed types
                        let self_ref = self_boxed.as_ref();
                        let target_ref = target_boxed.as_ref();

                        // ensure both inner elements have a GLB
                        let fst_glb =
                            Self::greatest_lower_bound(&self_ref.fst_type, &target_ref.fst_type)?;
                        let snd_glb =
                            Self::greatest_lower_bound(&self_ref.snd_type, &target_ref.snd_type)?;

                        // construct pair-GLB
                        Some(Self::pair_type(fst_glb, snd_glb))
                    }

                    // All other types do not have a GLB, due to type-shape mismatches
                    _ => None,
                },
            }
        }

        /// Checks if [`Type`] can weaken to another [`target`] [`Type`], which usually occurs
        /// due to `string`-weakening and `pair`-erasure.
        #[inline]
        #[must_use]
        pub fn weakens_to(&self, target: &Self) -> bool {
            match (self, target) {
                // Array-weakening rules
                (Self::ArrayType(self_boxed), target) => {
                    match (&self_boxed.elem_type, target) {
                        // `char[]` array-type weakens to `string` type
                        (Self::BaseType(BaseType::Char), Self::BaseType(BaseType::String)) => true,

                        // no other array-types weaken to any other type
                        _ => false,
                    }
                }

                // Pair-weakening rules
                (Self::PairType(self_boxed), target) => {
                    let self_ref = self_boxed.as_ref();
                    match (self_ref, target) {
                        (_, Self::PairType(target_boxed)) => {
                            let target_ref = target_boxed.as_ref();
                            match (
                                (&self_ref.fst_type, &self_ref.snd_type),
                                (&target_ref.fst_type, &target_ref.snd_type),
                            ) {
                                // Erased-pair-type `pair(any, any)` weakens to any pair type `pair(t1, t2)`,
                                // and vice-versa any pair type `pair(t1, t2)` weakens to erased-pair-type `pair(any, any)`.
                                ((Self::Any, Self::Any), _) | (_, (Self::Any, Self::Any)) => true,

                                // no other pair-types weaken to any other pair-type
                                _ => false,
                            }
                        }

                        // no other pair-types weaken to any other type
                        _ => false,
                    }
                }

                // no other types weaken to any other type
                _ => false,
            }
        }

        /// A type is compatible with another type if-and-only if:
        /// 1) it is equal to the target type, or
        /// 2) it weakens to the target type.
        #[must_use]
        #[inline]
        pub fn compatible_with(&self, target: &Self) -> bool {
            self == target || self.weakens_to(target)
        }

        // Tries to compute the Lowest-Common-Ancestor of two types, where
        // the resulting type
        //
        //
        // First tries to compute the Greatest-Lower-Bound (GLB)
        // pub fn lowest_common_ancestor(&self, other: &Self) -> Option<Self> {}
    }

    impl From<BaseType> for Type {
        #[inline(always)]
        fn from(value: BaseType) -> Self {
            Self::BaseType(value)
        }
    }

    impl From<ArrayType> for Type {
        #[inline(always)]
        fn from(value: ArrayType) -> Self {
            Self::from_array_type(value)
        }
    }

    impl From<PairType> for Type {
        #[inline(always)]
        fn from(value: PairType) -> Self {
            Self::from_pair_type(value)
        }
    }

    impl<T: Equality> From<TypeOps<T>> for Type {
        #[inline(always)]
        fn from(value: TypeOps<T>) -> Self {
            value.0.into()
        }
    }

    impl BaseType {}

    impl ArrayType {
        /// Alias for type `array[any]`.
        pub const UNKNOWN_ARRAY: Self = Self::new(Type::Any);

        /// Alias for type `array[char]`.
        pub const CHAR_ARRAY: Self = Self::new(Type::BaseType(BaseType::Char));

        #[inline]
        #[must_use]
        pub const fn new(elem_type: Type) -> Self {
            Self { elem_type }
        }
    }

    impl PairType {
        /// The erased-pair-type is simply an alias for type `pair(any, any)`.
        pub const ERASED_PAIR: Self = Self::new(Type::Any, Type::Any);

        #[inline]
        #[must_use]
        pub const fn new(fst_type: Type, snd_type: Type) -> Self {
            Self { fst_type, snd_type }
        }
    }

    // only implement type-equality for these types
    impl Equality for Type {}
    impl Equality for BaseType {}
    impl Equality for ArrayType {}
    impl Equality for PairType {}
    impl<T: Equality> Equality for TypeOps<T> {}

    // blanket specificity + weakening implementation for any type-equal object
    impl<T: Equality> Specificity for T {}
    impl<T: Equality> Weakening for T {}
    impl<T: Equality> Compatibility for T {}

    // newtype blanket implementation of ops
    impl<T: Equality + Clone, Rhs: Equality + Clone> PartialEq<Rhs> for TypeOps<T> {
        #[inline(always)]
        fn eq(&self, other: &Rhs) -> bool {
            self.0.equal_to(other)
        }
    }
    impl<T: Equality + Clone> Eq for TypeOps<T> {}
    impl<T: Specificity + Clone, Rhs: Equality + Clone> PartialOrd<Rhs> for TypeOps<T> {
        #[inline(always)]
        fn partial_cmp(&self, other: &Rhs) -> Option<cmp::Ordering> {
            self.0.partial_specificity_cmp(other)
        }
    }
    impl<T: Specificity + Clone, Rhs: Equality + Clone> Add<Rhs> for TypeOps<T> {
        type Output = TypeOps<Option<Type>>;

        #[inline(always)]
        fn add(self, rhs: Rhs) -> Self::Output {
            TypeOps(self.0.greatest_lower_bound(&rhs))
        }
    }
    impl<T: Specificity + Clone, Rhs: Equality + Clone> Add<Rhs> for TypeOps<Option<T>> {
        type Output = TypeOps<Option<Type>>;

        #[inline(always)]
        fn add(self, rhs: Rhs) -> Self::Output {
            TypeOps(self.0.and_then(|lhs| lhs.greatest_lower_bound(&rhs)))
        }
    }
    impl<T: Specificity + Clone, Rhs: Equality + Clone> Add<TypeOps<Option<Rhs>>>
        for TypeOps<Option<T>>
    {
        type Output = TypeOps<Option<Type>>;
        #[inline(always)]
        fn add(self, rhs: TypeOps<Option<Rhs>>) -> Self::Output {
            TypeOps(match (self.0, rhs.0) {
                (Some(lhs), Some(rhs)) => lhs.greatest_lower_bound(&rhs),
                _ => None,
            })
        }
    }

    impl Display for Type {
        #[inline]
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            match *self {
                Type::BaseType(ref b) => b.fmt(f),
                Type::ArrayType(ref a) => a.fmt(f),
                Type::PairType(ref p) => p.fmt(f),
                Type::Any => write!(f, "any"),
            }
        }
    }

    impl Display for BaseType {
        #[inline]
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            match *self {
                BaseType::Int => write!(f, "int"),
                BaseType::Bool => write!(f, "bool"),
                BaseType::Char => write!(f, "char"),
                BaseType::String => write!(f, "string"),
            }
        }
    }

    impl Display for ArrayType {
        #[inline]
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "{}[]", self.elem_type)
        }
    }

    impl Display for PairType {
        #[inline]
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            if self == &PairType::ERASED_PAIR {
                write!(f, "pair")
            } else {
                write!(f, "pair({}, {})", self.fst_type, self.snd_type)
            }
        }
    }
}
