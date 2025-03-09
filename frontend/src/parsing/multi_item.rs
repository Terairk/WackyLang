use chumsky::container::Container;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum MultiItem<T> {
    Multi(Vec<T>),
    Item(T),
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
#[repr(transparent)]
pub struct ItemVec<T>(Vec<T>);

impl<T> ItemVec<T> {
    pub fn into_inner(self) -> Vec<T> {
        self.0
    }
}

impl<T> Default for ItemVec<T> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<T> Container<MultiItem<T>> for ItemVec<T> {
    fn push(&mut self, item: MultiItem<T>) {
        match item {
            MultiItem::Multi(mut items) => self.0.append(&mut items),
            MultiItem::Item(item) => self.0.push(item),
        }
    }
}
