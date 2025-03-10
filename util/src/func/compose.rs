#![allow(clippy::inline_always)]

use std::marker::PhantomData;

/// Struct capturing function composition.
#[derive(Eq, PartialEq, Debug, Hash)]
pub struct Compose<F, G, B> {
    f: F,
    g: G,
    _b: PhantomData<B>,
}

mod compose_impls {
    use crate::func::compose::Compose;
    use crate::func::f1::F1Once;
    use std::marker::PhantomData;

    impl<F, G, B> Compose<F, G, B> {
        /// See [`Compose`]
        #[allow(clippy::self_named_constructors)]
        #[inline(always)]
        pub const fn compose<A, C>(f: F, g: G) -> Self
        where
            F: F1Once<A, Output = B>,
            G: F1Once<B, Output = C>,
        {
            Self {
                f,
                g,
                _b: PhantomData,
            }
        }

        #[inline(always)]
        pub const fn chain<G2, A, B2, C>(self, g2: G2) -> Compose<Self, G2, B2>
        where
            F: F1Once<A, Output = B>,
            G: F1Once<B, Output = B2>,
            G2: F1Once<B2, Output = C>,
        {
            Compose {
                f: self,
                g: g2,
                _b: PhantomData,
            }
        }
    }

    impl<F, G, B> Clone for Compose<F, G, B>
    where
        F: Clone,
        G: Clone,
    {
        #[inline(always)]
        fn clone(&self) -> Self {
            Self {
                f: self.f.clone(),
                g: self.g.clone(),
                _b: PhantomData,
            }
        }
    }

    impl<F, G, B> Copy for Compose<F, G, B>
    where
        F: Copy,
        G: Copy,
    {
    }
}

mod fn_impls {
    use crate::func::compose::Compose;
    use crate::func::f1::{F1Mut, F1Once, F1};

    impl<F, G, A, B, C> FnOnce<(A,)> for Compose<F, G, B>
    where
        F: F1Once<A, Output = B>,
        G: F1Once<B, Output = C>,
    {
        type Output = C;

        #[inline(always)]
        extern "rust-call" fn call_once(self, args: (A,)) -> Self::Output {
            self.g.call1_once(self.f.call1_once(args.0))
        }
    }

    impl<F, G, A, B, C> FnMut<(A,)> for Compose<F, G, B>
    where
        Self: FnOnce<(A,), Output = C>,
        F: F1Mut<A, Output = B>,
        G: F1Mut<B, Output = C>,
    {
        #[inline(always)]
        extern "rust-call" fn call_mut(&mut self, args: (A,)) -> Self::Output {
            self.g.call1_mut(self.f.call1_mut(args.0))
        }
    }

    impl<F, G, A, B, C> Fn<(A,)> for Compose<F, G, B>
    where
        Self: FnMut<(A,), Output = C>,
        F: F1<A, Output = B>,
        G: F1<B, Output = C>,
    {
        #[inline(always)]
        extern "rust-call" fn call(&self, args: (A,)) -> Self::Output {
            self.g.call1(self.f.call1(args.0))
        }
    }
}
