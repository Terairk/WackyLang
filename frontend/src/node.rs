//! TODO: crate documentation
//!
//! this is here as a placeholder documentation
//!
//!

#![allow(clippy::arbitrary_source_item_ordering)]

use std::{
    cmp::Ordering,
    fmt,
    ops::{Deref, DerefMut},
};

/// A node allocates [T] on the heap and also contains additional context [C] about it.
#[derive(Clone)]
pub struct Node<T, C = ()> {
    inner: T,
    context: C,
}

impl<T, C> Node<T, C> {
    /// Create a new node with the given inner value and context.
    #[inline]
    pub const fn new(inner: T, context: C) -> Self {
        Self { inner, context }
    }

    #[inline]
    pub const fn inner(&self) -> &T {
        &self.inner
    }

    #[inline]
    pub const fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    #[inline]
    pub fn into_inner(self) -> T {
        self.inner
    }

    #[inline]
    pub fn box_inner(self) -> BoxedNode<T, C> {
        self.map_inner(Box::new)
    }

    #[inline]
    pub fn map_inner<U, F: FnOnce(T) -> U>(self, f: F) -> Node<U, C> {
        Node {
            inner: f(self.inner),
            context: self.context,
        }
    }

    #[inline]
    pub fn transpose_ref(&self) -> Node<&T, C>
    where
        C: Clone,
    {
        Node::new(&self.inner, self.context.clone())
    }

    #[inline]
    pub fn transpose_mut(&mut self) -> Node<&mut T, C>
    where
        C: Clone,
    {
        Node::new(&mut self.inner, self.context.clone())
    }

    #[inline]
    pub const fn context(&self) -> &C {
        &self.context
    }

    #[inline]
    pub const fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }

    #[inline]
    pub fn into_tuple(self) -> (T, C) {
        (self.inner, self.context)
    }

    #[inline]
    pub const fn as_tuple(&self) -> (&T, &C) {
        (&self.inner, &self.context)
    }

    #[inline]
    pub fn as_tuple_mut(&mut self) -> (&mut T, &mut C) {
        (&mut self.inner, &mut self.context)
    }
}

impl<T, C> Deref for Node<T, C> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl<T, C> DerefMut for Node<T, C> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner_mut()
    }
}

impl<T: PartialEq, C> PartialEq for Node<T, C> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<T: Eq, C> Eq for Node<T, C> {}

impl<T: PartialOrd, C> PartialOrd<Self> for Node<T, C> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl<T: Ord, C> Ord for Node<T, C> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<T: fmt::Debug, C: fmt::Debug> fmt::Debug for Node<T, C> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} ~ {:#?}", self.context, self.inner)
    }
}

impl<T: fmt::Display, C: fmt::Display> fmt::Display for Node<T, C> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:} ~ {:#}", self.context, self.inner)
    }
}

pub type BoxedNode<T, C> = Node<Box<T>, C>;

impl<T, C> BoxedNode<T, C> {
    #[inline]
    pub fn from_unboxed(inner: T, context: C) -> Self {
        Self {
            inner: Box::new(inner),
            context,
        }
    }

    #[inline]
    pub fn into_inner_unboxed(self) -> T {
        *self.inner
    }

    #[inline]
    pub fn map_inner_unboxed<U, F: FnOnce(T) -> U>(self, f: F) -> BoxedNode<U, C> {
        Node {
            inner: Box::new(f(*self.inner)),
            context: self.context,
        }
    }

    #[inline]
    pub fn transpose_ref_unboxed(&self) -> Node<&T, C>
    where
        C: Clone,
    {
        Node::new(&self.inner, self.context.clone())
    }

    #[inline]
    pub fn transpose_mut_unboxed(&mut self) -> Node<&mut T, C>
    where
        C: Clone,
    {
        Node::new(&mut self.inner, self.context.clone())
    }
}
