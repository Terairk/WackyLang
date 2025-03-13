#![allow(clippy::arbitrary_source_item_ordering)]

use crate::node::{BoxedNode, Node};
use ariadne::Span as AriadneSpan;
use chumsky::{prelude::SimpleSpan, span::Span as ChumskySpan};
use internment::Intern;
use std::{
    fmt,
    ops::{Deref, Range},
    path::Path,
};

/// Trait for identifiers which uniquely refer to a source. In the simplest case,
/// it is just the fully qualified file path.
pub trait SourceId: Clone + PartialEq + ToOwned {}

/// A source which is identified by a string, most commonly a file path.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StrSourceId(Intern<str>);

#[allow(clippy::arbitrary_source_item_ordering)]
impl StrSourceId {
    #[must_use]
    #[inline]
    pub fn empty() -> Self {
        Self(Intern::from(""))
    }

    #[must_use]
    #[inline]
    pub fn repl() -> Self {
        Self(Intern::from("repl"))
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    #[inline]
    pub fn from_str(s: &str) -> Self {
        Self(Intern::from(s))
    }

    #[must_use]
    #[inline]
    pub fn from_boxed_str(s: Box<str>) -> Self {
        Self(Intern::from(s))
    }

    #[must_use]
    #[inline]
    pub fn from_string(s: String) -> Self {
        Self::from_boxed_str(s.into_boxed_str())
    }

    #[inline]
    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        Self::from_string(path.as_ref().to_string_lossy().into_owned())
    }

    #[must_use]
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    #[allow(clippy::missing_errors_doc)]
    #[inline]
    fn fmt_impl(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl SourceId for StrSourceId {}

impl fmt::Debug for StrSourceId {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Self::fmt_impl(self, f)
    }
}

impl fmt::Display for StrSourceId {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Self::fmt_impl(self, f)
    }
}

impl From<&str> for StrSourceId {
    #[inline]
    fn from(value: &str) -> Self {
        Self::from_str(value)
    }
}

impl From<String> for StrSourceId {
    #[inline]
    fn from(value: String) -> Self {
        Self::from_string(value)
    }
}

impl Deref for StrSourceId {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl AsRef<StrSourceId> for &StrSourceId {
    #[inline]
    fn as_ref(&self) -> &StrSourceId {
        self
    }
}

/// Spans which have a [`SourceId`] attached to them
pub trait SourceIdSpan: ChumskySpan<Offset = usize> {
    type SourceId: SourceId;

    fn source_id(&self) -> &Self::SourceId;
}

/// A span implementation with reference to the [`SourceId`] of the source being spanned.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct WithSourceId<SourceIdT = StrSourceId, SpanT = SimpleSpan>
where
    SourceIdT: SourceId,
    SpanT: ChumskySpan<Offset = usize>,
{
    source_id: SourceIdT,
    span: SpanT,
}

impl<SourceIdT, SpanT> WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId,
    SpanT: ChumskySpan<Offset = usize>,
{
    #[allow(clippy::same_name_method)]
    #[inline]
    pub const fn new(source_id: SourceIdT, span: SpanT) -> Self {
        Self { source_id, span }
    }

    #[must_use]
    #[inline]
    pub fn map_span<F: Fn(SpanT) -> SpanT>(self, f: F) -> Self {
        Self {
            span: f(self.span),
            ..self
        }
    }

    #[inline]
    fn start_impl(&self) -> usize {
        self.span.start()
    }

    #[inline]
    fn end_impl(&self) -> usize {
        self.span.end()
    }

    #[allow(clippy::same_name_method)]
    #[inline]
    pub const fn source_id(&self) -> &SourceIdT {
        &self.source_id
    }

    #[inline]
    pub fn as_range(&self) -> Range<usize> {
        self.span.start()..self.span.end()
    }

    #[inline]
    pub fn into_range(self) -> Range<usize> {
        self.span.start()..self.span.end()
    }
}

impl<SourceIdT, SpanT> fmt::Debug for WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId + fmt::Debug,
    SpanT: ChumskySpan<Offset = usize> + fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.source_id, self.span)
    }
}

impl<SourceIdT, SpanT> fmt::Display for WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId + fmt::Display,
    SpanT: ChumskySpan<Offset = usize> + fmt::Display,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.source_id, self.span)
    }
}

impl<SourceIdT, SpanT> ChumskySpan for WithSourceId<SourceIdT, SpanT>
where
    // SourceIdT: SourceId + fmt::Debug,
    // SpanT: ChumskySpan<Offset = usize> + fmt::Debug,
    // SpanT::Context: fmt::Debug,
    SourceIdT: SourceId,
    SpanT: ChumskySpan<Offset = usize>,
{
    type Context = (SourceIdT, SpanT::Context);
    type Offset = usize;

    #[inline]
    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        // println!("creating new span: {:#?}, {:#?}", context, range);
        let (mut start, mut end) = (range.start, range.end);
        if start > end {
            // Hacky heuristic, if ranges are flipped then it is probably meant to be
            // the other way around
            std::mem::swap(&mut start, &mut end);
        }
        Self::new(context.0, SpanT::new(context.1, start..end))
    }

    #[inline]
    fn context(&self) -> Self::Context {
        // println!("creating context: {:#?}", self);
        (self.source_id.clone(), self.span.context())
    }

    #[inline]
    fn start(&self) -> Self::Offset {
        // println!("creating start: {:#?}", self);
        Self::start_impl(self)
    }

    #[inline]
    fn end(&self) -> Self::Offset {
        // println!("creating end: {:#?}", self);
        Self::end_impl(self)
    }
}

impl<SourceIdT, SpanT> SourceIdSpan for WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId,
    SpanT: ChumskySpan<Offset = usize>,
{
    type SourceId = SourceIdT;

    #[inline]
    fn source_id(&self) -> &Self::SourceId {
        Self::source_id(self)
    }
}

impl<SourceIdT, SpanT> AriadneSpan for WithSourceId<SourceIdT, SpanT>
where
    SourceIdT: SourceId,
    SpanT: ChumskySpan<Offset = usize>,
{
    type SourceId = SourceIdT;

    #[inline]
    fn source(&self) -> &Self::SourceId {
        Self::source_id(self)
    }

    #[inline]
    fn start(&self) -> usize {
        Self::start_impl(self)
    }

    #[inline]
    fn end(&self) -> usize {
        Self::end_impl(self)
    }
}

pub type SourcedSpan = WithSourceId<StrSourceId, SimpleSpan>;
pub type SourcedNode<T> = Node<T, SourcedSpan>;

impl<T> SourcedNode<T> {
    #[must_use]
    #[inline]
    pub fn source_id(&self) -> StrSourceId {
        self.context().source_id.clone()
    }

    #[must_use]
    #[inline]
    pub fn span(&self) -> SourcedSpan {
        self.context().clone()
    }
}

pub type SourcedBoxedNode<T> = BoxedNode<T, SourcedSpan>;
