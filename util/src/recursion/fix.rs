use recursion::MappableFrame;

/// Heap allocated fix point of some frame [`F`]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Fix<F: MappableFrame>(pub Box<F::Frame<Fix<F>>>);

mod impls {
    use crate::recursion::fix::Fix;
    use recursion::{Collapsible, Expandable, MappableFrame};

    impl<F: MappableFrame> Collapsible for Fix<F> {
        type FrameToken = F;

        #[inline]
        fn into_frame(self) -> <Self::FrameToken as MappableFrame>::Frame<Self> {
            *self.0
        }
    }

    impl<F: MappableFrame> Expandable for Fix<F> {
        type FrameToken = F;

        #[inline]
        fn from_frame(val: <Self::FrameToken as MappableFrame>::Frame<Self>) -> Self {
            Self::new(val)
        }
    }

    impl<F: MappableFrame> Fix<F> {
        #[inline]
        pub fn new(x: F::Frame<Self>) -> Self {
            Self(Box::new(x))
        }
    }
}
