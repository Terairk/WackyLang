pub mod curry2;

#[inline]
pub const fn tuple2<T1, T2>(t1: T1, t2: T2) -> (T1, T2) {
    (t1, t2)
}

#[inline]
pub const fn tuple3<T1, T2, T3>(t1: T1, t2: T2, t3: T3) -> (T1, T2, T3) {
    (t1, t2, t3)
}

pub(super) mod arg {
    use crate::apply::ty_maybe::{Just, Nothing};

    pub trait Arg {
        /// The type of the argument
        type T;
    }

    impl<T> Arg for Nothing<T> {
        type T = T;
    }

    impl<T> Arg for Just<T> {
        type T = T;
    }
}

/// Type-level maybe-type
pub(super) mod ty_maybe {
    use std::marker::PhantomData;

    pub trait Maybe {
        type Variant: Maybe;
    }

    #[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
    pub struct Just<T: ?Sized>(pub T);

    impl<T> Nothing<T> {
        #[inline]
        pub const fn new() -> Self {
            Self(PhantomData)
        }
    }

    impl<T> Default for Nothing<T> {
        #[inline]
        fn default() -> Self {
            Self::new()
        }
    }

    impl<T> Clone for Nothing<T> {
        #[inline]
        fn clone(&self) -> Self {
            Self::new()
        }
    }

    #[derive(Copy, Eq, PartialEq, Debug, Hash)]
    pub struct Nothing<T: ?Sized>(pub(super) PhantomData<T>);
}

pub trait F1Once<A> {
    type Output;
}
