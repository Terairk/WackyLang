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
    #[inline(always)]
    fn partial_specificity_cmp<Target>(&self, target: &Target) -> Option<cmp::Ordering>
    where
        Self: Clone,
        Target: Equality + Clone,
    {
        Type::partial_specificity_cmp(&self.to_type(), &target.to_type())
    }
}

pub trait Weakening: Equality {
    fn weakens_to(&self, target: &Type) -> bool;
}

/// A type is compatible with another type if-and-only if:
/// 1) it is equal to the target type, or
/// 2) it weakens to the target type.
pub trait CompatibleWith: Weakening {
    fn compatible_with(&self, target: &Type) -> bool;
}

/// A newtype for any type-equal objects which implements various overloadable operations like
/// [`PartialEq`], [`PartialOrd`] and so on based on the [`Equality`], [`Specificity`], etc., definitions.
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct TypeOps<T: Equality>(pub T);

mod impls {
    use crate::wacc_thir::types::{
        ArrayType, BaseType, Equality, PairType, Specificity, Type, TypeOps,
    };
    use std::cmp;

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

        // pub fn weakens_to(&self, target: &Type) -> bool {
        //
        // }
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

    impl BaseType {}

    impl ArrayType {
        /// Alias for type `array(any)`.
        pub const UNKNOWN_ARRAY: Self = Self::new(Type::Any);

        #[inline]
        #[must_use]
        pub const fn new(elem_type: Type) -> Self {
            Self { elem_type }
        }

        #[inline]
        #[must_use]
        pub const fn weakens_to(&self, target: &Type) -> bool {
            match (self, target) {
                // `char[]` type weakens to `string` type
                (
                    Self {
                        elem_type: Type::BaseType(BaseType::Char),
                    },
                    Type::BaseType(BaseType::String),
                ) => true,

                // no other array types weaken to any other type
                _ => false,
            }
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

    // blanket specificity implementation for any type-equal object
    impl<T: Equality> Specificity for T {}

    // newtype blanket implementation of ops
    impl<T: Equality + Clone, Rhs: Equality + Clone> PartialEq<Rhs> for TypeOps<T> {
        #[inline(always)]
        fn eq(&self, other: &Rhs) -> bool {
            self.0.equal_to(other)
        }
    }
    impl<T: Equality + Clone> PartialEq<Self> for TypeOps<T> {
        #[inline(always)]
        fn eq(&self, other: &Self) -> bool {
            self == &other.0
        }
    }
    impl<T: Equality + Clone> Eq for TypeOps<T> {}
    impl<T: Specificity + Clone, Rhs: Equality + Clone> PartialOrd<Rhs> for TypeOps<T> {
        #[inline(always)]
        fn partial_cmp(&self, other: &Rhs) -> Option<cmp::Ordering> {
            self.0.partial_specificity_cmp(other)
        }
    }

    // A type is compatible with another type if-and-only if:
    // 1) it is equal to the target type, or
    // 2) it weakens to the target type.
    // impl<T: Weakening> CompatibleWith for T {
    //     #[inline(always)]
    //     fn compatible_with(&self, target: &Type) -> bool {
    //         self.equal_to(target) || self.weakens_to(target)
    //     }
    // }
}
