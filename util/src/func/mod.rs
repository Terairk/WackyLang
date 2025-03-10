//! Simulating functional patterns in Rust, like partial application and composition. Code heavily
//! inspired by the [naan](https://github.com/cakekindel/naan/tree/main) crate.

#![allow(clippy::inline_always)]

use crate::func::f1::F1Once;
use std::ops::Deref;

pub mod compose;
pub mod curry2;

#[inline]
pub const fn tuple2<T1, T2>(t1: T1, t2: T2) -> (T1, T2) {
    (t1, t2)
}

#[inline]
pub const fn tuple3<T1, T2, T3>(t1: T1, t2: T2, t3: T3) -> (T1, T2, T3) {
    (t1, t2, t3)
}

/// Type of [`call_deref`]
#[allow(non_camel_case_types)]
pub type call_deref_t<F, A, B> = fn(f: F, a: A) -> B;

/// Lift a function from `&ADeref -> B` to `A -> B`
/// where `A` can [`Deref::deref`] as `ADeref`
///
/// Used by [`F1Once::chain_ref`].
#[inline(always)]
pub fn call_deref<F, A, ADeref: ?Sized, B>(f: F, a: A) -> B
where
    A: Deref<Target = ADeref>,
    F: for<'a> F1Once<&'a ADeref, Output = B>,
{
    f.call1_once(&*a)
}

pub(super) mod arg {
    use crate::typelevel::maybe::{Just, Nothing};

    #[const_trait]
    pub trait Arg {
        /// The type of the argument
        type T;
    }

    impl<T> const Arg for Nothing<T> {
        type T = T;
    }

    impl<T> const Arg for Just<T> {
        type T = T;
    }
}

pub mod f1 {
    use crate::func::call_deref_t;
    use crate::func::compose::Compose;
    use crate::func::curry2::Applied1;
    use std::ops::Deref;

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

    pub trait F1OnceExt<A>: F1Once<A> {
        fn chain<G, C>(self, g: G) -> Compose<Self, G, Self::Output>
        where
            Self: Sized,
            G: F1Once<Self::Output, Output = C>;

        #[allow(clippy::type_complexity)]
        fn chain_ref<G, BDeref: ?Sized, C>(
            self,
            g: G,
        ) -> Compose<
            Self,
            Applied1<call_deref_t<G, Self::Output, C>, G, Self::Output, C>,
            Self::Output,
        >
        where
            Self: Sized,
            G: for<'any> F1Once<&'any BDeref, Output = C>,
            Self::Output: Deref<Target = BDeref>;
    }

    mod impls {
        use crate::func::compose::Compose;
        use crate::func::curry2::Applied1;
        use crate::func::f1::{F1Mut, F1Once, F1OnceExt, F1};
        use crate::func::f2::F2OnceExt as _;
        use crate::func::{call_deref, call_deref_t};
        use std::ops::Deref;

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

        impl<F, A> F1OnceExt<A> for F
        where
            F: F1Once<A>,
        {
            #[inline(always)]
            fn chain<G, C>(self, g: G) -> Compose<Self, G, Self::Output>
            where
                Self: Sized,
                G: F1Once<Self::Output, Output = C>,
            {
                Compose::compose(self, g)
            }

            #[allow(clippy::as_conversions)]
            #[inline(always)]
            fn chain_ref<G, BDeref: ?Sized, C>(
                self,
                g: G,
            ) -> Compose<
                Self,
                Applied1<call_deref_t<G, Self::Output, C>, G, Self::Output, C>,
                Self::Output,
            >
            where
                Self: Sized,
                G: for<'any> F1Once<&'any BDeref, Output = C>,
                Self::Output: Deref<Target = BDeref>,
            {
                Compose::compose(
                    self,
                    (call_deref as call_deref_t<G, Self::Output, C>).curry()(g),
                )
            }
        }
    }
}

pub mod f2 {
    use crate::func::f1::{F1Once, F1};

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
    }

    #[const_trait]
    pub trait F2OnceExt<A, B>: F2Once<A, B> {
        fn curry(self) -> Self::Curried;
    }

    mod impls {
        use crate::func::curry2::{Applied0, Applied1};
        use crate::func::f2::{F2Mut, F2Once, F2OnceExt, F2};

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
            type Output = C;
            type Applied = Applied1<Self, A, B, C>;
            type Curried = Applied0<Self, A, B, C>;

            #[inline(always)]
            fn call2_once(self, a: A, b: B) -> Self::Output {
                self(a, b)
            }
        }

        impl<F, A, B, C> const F2OnceExt<A, B> for F
        where
            F: F2Once<A, B, Output = C, Curried = Applied0<Self, A, B, C>>,
        {
            #[inline(always)]
            fn curry(self) -> Self::Curried {
                Applied0::curry(self)
            }
        }
    }
}
