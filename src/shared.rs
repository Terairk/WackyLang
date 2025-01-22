use std::rc::Rc;

pub type Shared<T> = Rc<T>;
pub type SharedString = Shared<str>;
pub type SharedArray<T> = Shared<[T]>;
