use recursion::{MappableFrame, PartiallyApplied};
use std::marker::PhantomData;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Compose<F, G, A>(PhantomData<F>, PhantomData<G>, PhantomData<A>);

impl<F: MappableFrame, G: MappableFrame> MappableFrame for Compose<F, G, PartiallyApplied> {
    type Frame<X> = F::Frame<G::Frame<X>>;

    #[inline]
    fn map_frame<A, B>(input: Self::Frame<A>, mut f: impl FnMut(A) -> B) -> Self::Frame<B> {
        F::map_frame(input, move |x| G::map_frame(x, &mut f))
    }
}
