//! The goal is to translate the following GADT into Rust:
//!
//! ```haskell
//! class Foo a where
//!   foo :: a -> Bool
//!
//! data MyGADT a where
//!   SomeFoo :: Foo a => a -> MyGADT a
//!   Other :: a -> MyGADT a
//!
//! -- example usage
//! instance Foo Int where
//!   foo _ = True
//!
//! doThing :: GADT a -> Bool
//! doThing (SomeFoo a) = foo a
//! doThing (Other _) = False
//!
//! good = doThing (SomeFoo (10 :: Int)) // True
//! bad = doThing (SomeFoo (10 :: Double)) // type error, no instance for Foo Double
//! ```
//!
//! For this I will use specialization and zero-sized type-witnesses

use std::marker::PhantomData;

pub trait Foo {
    fn foo(&self) -> bool;
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum MyGADT<T> {
    SomeFoo(FooWitness<T>, T),
    Other(T),
}

/// This is a type-witness for [`Foo`], it can only be constructed for types that implement [`Foo`].
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[repr(transparent)]
pub struct FooWitness<T: ?Sized>(PhantomData<T>);

impl<T: ?Sized> FooWitness<T> {
    #[inline]
    #[must_use]
    pub const fn new() -> Self
    where
        T: Foo,
    {
        Self(PhantomData)
    }
}

impl<T: Foo + ?Sized> Default for FooWitness<T> {
    #[inline]
    #[must_use]
    fn default() -> Self {
        Self::new()
    }
}

#[inline]
#[must_use]
pub const fn is_foo<T: Foo + ?Sized>() -> FooWitness<T> {
    FooWitness::new()
}

// type-level booleans, may be useful later...
pub mod ty_bool {
    use crate::private;
    use std::fmt::Debug;
    use std::hash::Hash;

    #[derive(Copy, Clone, Eq, PartialEq, Debug, Default, Hash)]
    pub struct True;
    #[derive(Copy, Clone, Eq, PartialEq, Debug, Default, Hash)]
    pub struct False;
    pub trait Bool:
        Copy + Clone + Eq + PartialEq + Debug + Default + Hash + private::Sealed
    {
    }
    impl Bool for True {}
    impl Bool for False {}
}

// Type-level equality, useful for later
pub trait TyEq {
    type Rhs: TyEq<Rhs = Self> + ?Sized;
}

impl<T: ?Sized> TyEq for T {
    type Rhs = T;
}

/// This is an instance provider trait for [`Foo`], it uses specialization to provide the correct
/// implementation of [`Foo`] with static-dispatch, based on a supplied [`FooWitness`] witness value.
pub trait FooInstance {
    type Implemented: ty_bool::Bool;
    fn foo_with(&self, foo_witness: &FooWitness<Self>) -> bool;
}

impl<T: ?Sized> FooInstance for T {
    default type Implemented = ty_bool::False;

    #[inline]
    default fn foo_with(&self, _: &FooWitness<T>) -> bool {
        unreachable!()
    }
}

impl<T: Foo + ?Sized> FooInstance for T {
    type Implemented = ty_bool::True;

    #[inline]
    fn foo_with(&self, _: &FooWitness<T>) -> bool {
        T::foo(self)
    }
}

// this is the [`Foo`] instance definition for [`usize`]
impl Foo for usize {
    #[inline]
    fn foo(&self) -> bool {
        true
    }
}

// function which makes use of `MyGADT` in pattern matching:
// by construction, a value of type `FooWitness<T>` can only be inhabited if `T: Foo`
// so `FooInstance::foo_with` will __always__ specialize to the one that doesn't panic
#[inline]
pub fn do_thing<T>(a: &MyGADT<T>) -> bool {
    match *a {
        MyGADT::SomeFoo(ref wit, ref a) => a.foo_with(wit),
        MyGADT::Other(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::rust_gadt_playground::{
        do_thing, is_foo, ty_bool, FooInstance, MyGADT,
    };

    #[test]
    fn test_foo() {
        let arg: usize = 10;
        let usize_witness = is_foo::<usize>();
        // let double_witness = is_foo::<f64>(); // compile error

        let usize_implemented: <usize as FooInstance>::Implemented = ty_bool::True;
        // let double_not_implemented: <u32 as FooInstance>::Implemented = ty_bool::False; // doesn't compile

        // GADT construction
        let gadt_value = MyGADT::SomeFoo(usize_witness, arg);

        // this never panics
        assert!(do_thing(&gadt_value)); // OK
    }
}
