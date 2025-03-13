use crate::private;
use std::marker::PhantomData;

/// Emulation of `T == U` bounds with `T: Is<Ty = U>`.
#[const_trait]
pub trait Is: private::Sealed {
    type Ty: Is<Ty = Self> + ?Sized;

    fn refl(self) -> Self::Ty
    where
        Self: Sized;

    fn inv_refl(this: Self::Ty) -> Self
    where
        Self::Ty: Sized;

    fn refl_ref(&self) -> &Self::Ty;

    fn inv_refl_ref(this: &Self::Ty) -> &Self;

    fn refl_mut(&mut self) -> &mut Self::Ty;

    fn inv_refl_mut(this: &mut Self::Ty) -> &mut Self;
}

/// This is a type-witness for [`Is`], it can only be constructed for types that implement [`Is`]
/// for a specific [`Is::Ty`] type.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[repr(transparent)]
pub struct IsWit<L: ?Sized, R: ?Sized>(PhantomData<L>, PhantomData<R>);

#[inline]
#[must_use]
pub const fn is<L: Is<Ty = R> + ?Sized, R: ?Sized>() -> IsWit<L, R> {
    IsWit::new()
}

/// This is an instance provider trait for [`Is`], it uses specialization to provide the correct
/// implementation of [`Is`] with static-dispatch, based on a supplied [`IsWit`] witness value
#[const_trait]
pub trait IsInstance<Ty: ?Sized> {
    fn refl_with(self, is_wit: &IsWit<Self, Ty>) -> Ty
    where
        Self: Sized,
        Ty: Sized;

    fn inv_refl_with(this: Ty, is_wit: &IsWit<Self, Ty>) -> Self
    where
        Self: Sized,
        Ty: Sized;

    fn refl_ref_with(&self, is_wit: &IsWit<Self, Ty>) -> &Ty;

    fn inv_refl_ref_with<'a>(this: &'a Ty, is_wit: &IsWit<Self, Ty>) -> &'a Self;

    fn refl_mut_with(&mut self, is_wit: &IsWit<Self, Ty>) -> &mut Ty;

    fn inv_refl_mut_with<'a>(this: &'a mut Ty, is_wit: &IsWit<Self, Ty>) -> &'a mut Self;
}

mod impls {
    #![allow(clippy::inline_always)]

    use crate::typelevel::is::{Is, IsInstance, IsWit};
    use std::marker::PhantomData;

    impl<T: ?Sized> const Is for T {
        type Ty = T;

        #[inline(always)]
        fn refl(self) -> T
        where
            T: Sized,
        {
            self
        }

        #[inline(always)]
        fn inv_refl(this: T) -> T
        where
            T: Sized,
        {
            this
        }

        #[inline(always)]
        fn refl_ref(&self) -> &T {
            self
        }

        #[inline(always)]
        fn inv_refl_ref(this: &T) -> &T {
            this
        }

        #[inline(always)]
        fn refl_mut(&mut self) -> &mut T {
            self
        }

        #[inline(always)]
        fn inv_refl_mut(this: &mut T) -> &mut T {
            this
        }
    }

    impl<L: ?Sized, R: ?Sized> IsWit<L, R> {
        #[inline(always)]
        #[must_use]
        pub const fn new() -> Self
        where
            L: Is<Ty = R>,
        {
            Self(PhantomData, PhantomData)
        }
    }

    impl<L: Is<Ty = R> + ?Sized, R: ?Sized> Default for IsWit<L, R> {
        #[inline(always)]
        fn default() -> Self {
            Self::new()
        }
    }

    impl<L: ?Sized, R: ?Sized> const IsInstance<R> for L {
        #[inline(always)]
        default fn refl_with(self, _is_wit: &IsWit<Self, R>) -> R
        where
            Self: Sized,
            R: Sized,
        {
            unreachable!()
        }

        #[inline(always)]
        default fn inv_refl_with(_this: R, _is_wit: &IsWit<Self, R>) -> Self
        where
            Self: Sized,
            R: Sized,
        {
            unreachable!()
        }

        #[inline(always)]
        default fn refl_ref_with(&self, _is_wit: &IsWit<Self, R>) -> &R {
            unreachable!()
        }

        #[inline(always)]
        default fn inv_refl_ref_with<'a>(_this: &'a R, _is_wit: &IsWit<Self, R>) -> &'a Self {
            unreachable!()
        }

        #[inline(always)]
        default fn refl_mut_with(&mut self, _is_wit: &IsWit<Self, R>) -> &mut R {
            unreachable!()
        }

        #[inline(always)]
        default fn inv_refl_mut_with<'a>(
            _this: &'a mut R,
            _is_wit: &IsWit<Self, R>,
        ) -> &'a mut Self {
            unreachable!()
        }
    }
    impl<L: ~const Is<Ty = R> + ?Sized, R: ?Sized> const IsInstance<R> for L {
        #[inline(always)]
        fn refl_with(self, _is_wit: &IsWit<Self, R>) -> R
        where
            Self: Sized,
            R: Sized,
        {
            self.refl()
        }

        #[inline(always)]
        fn inv_refl_with(this: R, _is_wit: &IsWit<Self, R>) -> Self
        where
            Self: Sized,
            R: Sized,
        {
            Self::inv_refl(this)
        }

        #[inline(always)]
        fn refl_ref_with(&self, _is_wit: &IsWit<Self, R>) -> &R {
            self.refl_ref()
        }

        #[inline(always)]
        fn inv_refl_ref_with<'a>(this: &'a R, _is_wit: &IsWit<Self, R>) -> &'a Self {
            Self::inv_refl_ref(this)
        }

        #[inline(always)]
        fn refl_mut_with(&mut self, _is_wit: &IsWit<Self, R>) -> &mut R {
            self.refl_mut()
        }

        #[inline(always)]
        fn inv_refl_mut_with<'a>(this: &'a mut R, _is_wit: &IsWit<Self, R>) -> &'a mut Self {
            Self::inv_refl_mut(this)
        }
    }
}
