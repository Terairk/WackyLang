use crate::apply::ty_maybe::{Just, Nothing};
use std::marker::PhantomData;

pub type Applied0<F, A, B, C> = Curry2<F, Nothing<A>, Nothing<B>, C>;

pub type Applied1<F, A, B, C> = Curry2<F, Just<A>, Nothing<B>, C>;

pub struct Curry2<F, A: ?Sized, B: ?Sized, R: ?Sized> {
    f: F,
    _b: PhantomData<B>,
    _r: PhantomData<R>,
    a: A, // last field can be unsized, pretty convenient
}
