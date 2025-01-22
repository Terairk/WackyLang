use chumsky::Span;
use std::fmt;
use std::ops::{Deref, DerefMut, Range};

#[derive(Clone, Debug)]
pub struct Spanned<T: Clone, S: Span = Range<usize>> {
    pub inner: T,
    pub span: S,
}

impl<T: Clone, S: Span> Spanned<T, S> {
    pub fn new(inner: T, span: S) -> Self {
        Spanned { inner, span }
    }
}

impl<T: Clone, S: Span> Span for Spanned<T, S> {
    type Context = (T, S::Context);
    type Offset = S::Offset;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Spanned::new(context.0, S::new(context.1, range))
    }

    fn context(&self) -> Self::Context {
        (self.inner.clone(), self.span.context())
    }

    fn start(&self) -> Self::Offset {
        self.span.start()
    }

    fn end(&self) -> Self::Offset {
        self.span.end()
    }
}

impl<T: Clone, S: Span> Deref for Spanned<T, S> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Clone, S: Span> DerefMut for Spanned<T, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: Clone + fmt::Debug, S: Span> fmt::Display for Spanned<T, S>
where
    S::Offset: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}..{}: {:?}]",
            self.span.start(),
            self.span.end(),
            self.inner
        )
    }
}
