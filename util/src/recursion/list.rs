#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ListFrame<X, XS> {
    Cons(X, XS),
    Nil,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CollapsibleSlice<'a, T>(pub &'a [T]);

mod impls {
    use crate::nonempty::NonemptyArray;
    use crate::recursion::list::{CollapsibleSlice, ListFrame};
    use recursion::{Collapsible, MappableFrame, PartiallyApplied};

    impl<X> MappableFrame for ListFrame<X, PartiallyApplied> {
        type Frame<XS> = ListFrame<X, XS>;

        #[inline]
        fn map_frame<A, B>(input: Self::Frame<A>, mut f: impl FnMut(A) -> B) -> Self::Frame<B> {
            match input {
                ListFrame::Cons(x, xs) => ListFrame::Cons(x, f(xs)),
                ListFrame::Nil => ListFrame::Nil,
            }
        }
    }

    impl<'a, T: 'a> Collapsible for CollapsibleSlice<'a, T> {
        type FrameToken = ListFrame<&'a T, PartiallyApplied>;

        #[inline]
        fn into_frame(self) -> <Self::FrameToken as MappableFrame>::Frame<Self> {
            match self.0.split_first() {
                Some((first, rest)) => ListFrame::Cons(first, CollapsibleSlice(rest)),
                None => ListFrame::Nil,
            }
        }
    }

    impl<'a, T> From<&'a [T]> for CollapsibleSlice<'a, T> {
        #[inline]
        fn from(value: &'a [T]) -> Self {
            CollapsibleSlice(value)
        }
    }

    impl<'a, T> From<&'a Vec<T>> for CollapsibleSlice<'a, T> {
        #[inline]
        fn from(value: &'a Vec<T>) -> Self {
            CollapsibleSlice(value.as_slice())
        }
    }

    impl<'a, T> From<&'a Box<[T]>> for CollapsibleSlice<'a, T> {
        #[inline]
        fn from(value: &'a Box<[T]>) -> Self {
            CollapsibleSlice(value)
        }
    }

    impl<'a, T> From<&'a NonemptyArray<T>> for CollapsibleSlice<'a, T> {
        #[inline]
        fn from(value: &'a NonemptyArray<T>) -> Self {
            CollapsibleSlice(value.as_slice())
        }
    }
}
