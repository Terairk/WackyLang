use crate::private;
use std::marker::PhantomData;

/// Type-level emulation of the maybe-type.
pub trait Maybe: private::Sealed {
    type T: ?Sized;

    fn match_on<O, NF, JF>(self, nothing: NF, just: JF) -> O
    where
        Self::T: Sized,
        NF: FnOnce(Nothing<Self::T>) -> O,
        JF: FnOnce(Just<Self::T>) -> O;
}

#[derive(Copy, Eq, PartialEq, Debug, Hash)]
pub struct Nothing<T: ?Sized>(pub(super) PhantomData<T>);

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
pub struct Just<T: ?Sized>(pub T);

mod impls {
    #![allow(clippy::inline_always)]

    use crate::typelevel::maybe::{Just, Maybe, Nothing};
    use std::marker::PhantomData;

    impl<T> Nothing<T> {
        #[inline(always)]
        #[must_use]
        pub const fn new() -> Self {
            Self(PhantomData)
        }
    }

    impl<T: ?Sized> Maybe for Nothing<T> {
        type T = T;

        #[inline(always)]
        fn match_on<O, NF, JF>(self, nothing: NF, _just: JF) -> O
        where
            Self::T: Sized,
            NF: FnOnce(Nothing<Self::T>) -> O,
            JF: FnOnce(Just<Self::T>) -> O,
        {
            nothing(self)
        }
    }

    impl<T> Default for Nothing<T> {
        #[inline(always)]
        fn default() -> Self {
            Self::new()
        }
    }

    impl<T> Clone for Nothing<T> {
        #[inline(always)]
        fn clone(&self) -> Self {
            Self::new()
        }
    }

    impl<T: ?Sized> Maybe for Just<T> {
        type T = T;

        #[inline(always)]
        fn match_on<O, NF, JF>(self, _nothing: NF, just: JF) -> O
        where
            Self::T: Sized,
            NF: FnOnce(Nothing<Self::T>) -> O,
            JF: FnOnce(Just<Self::T>) -> O,
        {
            just(self)
        }
    }
}
