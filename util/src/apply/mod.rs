#![allow(clippy::inline_always)]

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
    use crate::typelevel::maybe::{Just, Nothing};

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

pub mod f1 {
    pub trait F1<A>: F1Mut<A> {
        fn call1(&self, a: A) -> Self::Output;
    }

    pub trait F1Mut<A>: F1Once<A> {
        fn call1_mut(&mut self, a: A) -> Self::Output;
    }

    pub trait F1Once<A> {
        type Output;

        fn call1_once(self, a: A) -> Self::Output;
    }

    mod impls {
        use crate::apply::f1::{F1Mut, F1Once, F1};

        impl<F, A, B> F1<A> for F
        where
            F: Fn(A) -> B,
        {
            #[inline(always)]
            fn call1(&self, a: A) -> Self::Output {
                self(a)
            }
        }

        impl<F, A, B> F1Mut<A> for F
        where
            F: FnMut(A) -> B,
        {
            #[inline(always)]
            fn call1_mut(&mut self, a: A) -> Self::Output {
                self(a)
            }
        }

        impl<F, A, B> F1Once<A> for F
        where
            F: FnOnce(A) -> B,
        {
            type Output = F::Output;

            #[inline(always)]
            fn call1_once(self, a: A) -> Self::Output {
                self(a)
            }
        }
    }
}

pub mod f2 {
    use crate::apply::f1::{F1Mut, F1Once, F1};

    pub trait F2<A, B>: F2Mut<A, B> {
        fn call2(&self, a: A, b: B) -> Self::Output;
    }

    pub trait F2Mut<A, B>: F2Once<A, B> {
        fn call2_mut(&mut self, a: A, b: B) -> Self::Output;
    }

    pub trait F2Once<A, B> {
        type Output;
        type Applied: F1Once<B, Output = Self::Output>;
        type Curried: F1Once<A, Output = Self::Applied>;

        fn call2_once(self, a: A, b: B) -> Self::Output;

        fn curry(self) -> Self::Curried;
    }

    mod impls {
        use crate::apply::curry2::{Applied0, Applied1};
        use crate::apply::f2::{F2Mut, F2Once, F2};

        impl<F, A, B, C> F2<A, B> for F
        where
            F: Fn(A, B) -> C,
            A: Clone,
        {
            #[inline(always)]
            fn call2(&self, a: A, b: B) -> Self::Output {
                self(a, b)
            }
        }

        impl<F, A, B, C> F2Mut<A, B> for F
        where
            F: FnMut(A, B) -> C,
            A: Clone,
        {
            #[inline(always)]
            fn call2_mut(&mut self, a: A, b: B) -> Self::Output {
                self(a, b)
            }
        }

        impl<F, A, B, C> F2Once<A, B> for F
        where
            F: FnOnce(A, B) -> C,
        {
            type Output = F::Output;
            type Applied = Applied1<Self, A, B, C>;
            type Curried = Applied0<Self, A, B, C>;

            #[inline(always)]
            fn call2_once(self, a: A, b: B) -> Self::Output {
                self(a, b)
            }

            #[inline(always)]
            fn curry(self) -> Self::Curried {
                Applied0::curry(self)
            }
        }
    }
}
