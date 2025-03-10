#![allow(clippy::inline_always)]

use crate::typelevel::maybe::{Just, Nothing};
use std::marker::PhantomData;

pub type Applied0<F, A, B, R> = Curry2<F, Nothing<A>, Nothing<B>, R>;

pub type Applied1<F, A, B, R> = Curry2<F, Just<A>, Nothing<B>, R>;

#[derive(Eq, PartialEq, Debug, Hash)]
pub struct Curry2<F, A: ?Sized, B: ?Sized, R: ?Sized> {
    f: F,
    _b: PhantomData<B>,
    _r: PhantomData<R>,
    a: A, // last field can be unsized, pretty convenient
}

mod curry2_impls {
    use crate::func::curry2::{Applied0, Applied1};
    use crate::func::f2::{F2Mut, F2Once, F2};
    use crate::typelevel::maybe::{Just, Nothing};
    use std::marker::PhantomData;

    impl<F, A, B, R> Applied0<F, A, B, R>
    where
        F: F2Once<A, B, Output = R>,
    {
        #[inline(always)]
        pub const fn curry(f: F) -> Self {
            Self {
                f,
                a: Nothing::new(),
                _b: PhantomData,
                _r: PhantomData,
            }
        }

        #[inline(always)]
        pub fn uncurry(self) -> F {
            self.f
        }

        #[inline(always)]
        pub fn apply(self, a: A) -> Applied1<F, A, B, R> {
            Applied1 {
                f: self.f,
                a: Just(a),
                _b: PhantomData,
                _r: PhantomData,
            }
        }

        #[inline(always)]
        pub fn apply_mut(&mut self, a: A) -> Applied1<F, A, B, R>
        where
            F: Clone,
        {
            Applied1 {
                f: self.f.clone(),
                a: Just(a),
                _b: PhantomData,
                _r: PhantomData,
            }
        }

        #[inline(always)]
        pub fn apply_ref(&self, a: A) -> Applied1<F, A, B, R>
        where
            F: Clone,
        {
            Applied1 {
                f: self.f.clone(),
                a: Just(a),
                _b: PhantomData,
                _r: PhantomData,
            }
        }
    }

    impl<F, A, B, R> Clone for Applied0<F, A, B, R>
    where
        F: Clone,
    {
        #[inline(always)]
        fn clone(&self) -> Self {
            Self {
                f: self.f.clone(),
                _b: PhantomData,
                _r: PhantomData,
                a: self.a.clone(),
            }
        }
    }

    impl<F, A, B, R> Copy for Applied0<F, A, B, R>
    where
        F: Copy,
        A: Copy,
    {
    }

    impl<F, A, B, R> Applied1<F, A, B, R>
    where
        F: F2Once<A, B, Output = R>,
    {
        #[inline(always)]
        pub fn apply(self, b: B) -> R {
            self.f.call2_once(self.a.0, b)
        }

        #[inline(always)]
        pub fn apply_mut(&mut self, b: B) -> R
        where
            F: F2Mut<A, B, Output = R>,
            A: Clone,
        {
            self.f.call2_mut(self.a.0.clone(), b)
        }

        #[inline(always)]
        pub fn apply_ref(&self, b: B) -> R
        where
            F: F2<A, B, Output = R>,
            A: Clone,
        {
            self.f.call2(self.a.0.clone(), b)
        }
    }

    impl<F, A, B, R> Clone for Applied1<F, A, B, R>
    where
        F: Clone,
        A: Clone,
    {
        #[inline(always)]
        fn clone(&self) -> Self {
            Self {
                f: self.f.clone(),
                _b: PhantomData,
                _r: PhantomData,
                a: self.a.clone(),
            }
        }
    }
    impl<F, A, B, R> Copy for Applied1<F, A, B, R>
    where
        F: Copy,
        A: Copy,
    {
    }
}

mod fn_impls {
    use crate::func::curry2::{Applied0, Applied1};

    impl<F, A, B, R> FnOnce<(A,)> for Applied0<F, A, B, R>
    where
        F: FnOnce(A, B) -> R,
    {
        type Output = Applied1<F, A, B, R>;

        #[inline(always)]
        extern "rust-call" fn call_once(self, args: (A,)) -> Self::Output {
            self.apply(args.0)
        }
    }

    impl<F, A, B, R> FnMut<(A,)> for Applied0<F, A, B, R>
    where
        Self: FnOnce<(A,), Output = Applied1<F, A, B, R>>,
        F: FnMut(A, B) -> R + Clone,
    {
        #[inline(always)]
        extern "rust-call" fn call_mut(&mut self, args: (A,)) -> Self::Output {
            self.apply_mut(args.0)
        }
    }

    impl<F, A, B, R> Fn<(A,)> for Applied0<F, A, B, R>
    where
        Self: FnMut<(A,), Output = Applied1<F, A, B, R>>,
        F: Fn(A, B) -> R + Clone,
    {
        #[inline(always)]
        extern "rust-call" fn call(&self, args: (A,)) -> Self::Output {
            self.apply_ref(args.0)
        }
    }

    impl<F, A, B, R> FnOnce<(B,)> for Applied1<F, A, B, R>
    where
        F: FnOnce(A, B) -> R,
    {
        type Output = R;

        #[inline(always)]
        extern "rust-call" fn call_once(self, args: (B,)) -> Self::Output {
            self.apply(args.0)
        }
    }

    impl<F, A, B, R> FnMut<(B,)> for Applied1<F, A, B, R>
    where
        Self: FnOnce<(B,), Output = R>,
        F: FnMut(A, B) -> R,
        A: Clone,
    {
        #[inline(always)]
        extern "rust-call" fn call_mut(&mut self, args: (B,)) -> Self::Output {
            self.apply_mut(args.0)
        }
    }

    impl<F, A, B, R> Fn<(B,)> for Applied1<F, A, B, R>
    where
        Self: FnMut<(B,), Output = R>,
        F: Fn(A, B) -> R,
        A: Clone,
    {
        #[inline(always)]
        extern "rust-call" fn call(&self, args: (B,)) -> Self::Output {
            self.apply_ref(args.0)
        }
    }
}
