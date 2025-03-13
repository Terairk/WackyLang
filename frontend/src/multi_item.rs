use chumsky::container::Container;
use delegate::delegate;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum MultiItem<T> {
    Multi(Box<[T]>),
    Item(T),
}

impl<T> MultiItem<T> {
    #[inline]
    pub fn multi<IntoTS: Into<Box<[T]>>>(items: IntoTS) -> Self {
        Self::Multi(items.into())
    }

    #[inline]
    pub fn item<IntoT: Into<T>>(item: IntoT) -> Self {
        Self::Item(item.into())
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
#[repr(transparent)]
pub struct MultiItemVec<T>(Vec<T>);

impl<T> MultiItemVec<T> {
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    #[must_use]
    pub fn into_inner(self) -> Vec<T> {
        self.0
    }

    #[inline]
    pub fn push_multi_item(&mut self, item: MultiItem<T>) {
        match item {
            MultiItem::Multi(items) => self.0.extend(items),
            MultiItem::Item(item) => self.0.push(item),
        }
    }

    delegate! {
        to self.0 {
            #[inline]
            pub fn into_boxed_slice(self) -> Box<[T]>;
        }
    }
}

impl<T> Default for MultiItemVec<T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<T> From<MultiItemVec<T>> for Vec<T> {
    #[inline]
    fn from(value: MultiItemVec<T>) -> Self {
        value.into_inner()
    }
}

impl<T> From<MultiItemVec<T>> for Box<[T]> {
    #[inline]
    fn from(value: MultiItemVec<T>) -> Self {
        value.into_boxed_slice()
    }
}

impl<T> Container<MultiItem<T>> for MultiItemVec<T> {
    #[inline]
    fn push(&mut self, item: MultiItem<T>) {
        self.push_multi_item(item);
    }
}
