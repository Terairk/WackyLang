//! The goal is to translate the following GADT into Rust:
//!
//! ```haskell
//! class Foo a where
//!   foo :: a -> Bool
//!
//! data MyGADT a where
//!   SomeFoo :: Foo a => a -> MyGADT a
//!   SomeInt :: Int -> MyGADT Bool
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
//! Overall, the strategy seems to be to relate witnesses and trait-bounds
//! And if you need to provide more than one trait bound, you can just add more and more witnesses
//! Which will simply get compiled away
//!
//! We might even create a wrapper type like Witness<T, W>, where it will hold the witness type and
//! the underlying type, which can allow simple trait bounds to be recovered:
//!
//! ```rust
//! impl<T> Foo for Witness<T, FooWitness<T>> {
//!   fn foo(&self) -> bool {
//!     (self.0).foo_with(self.1) // this implementation gets replaced with original
//!   }  
//! }
//! ```
//!
//! So this way, if you have e.g.
//! ```rust
//! fn something<T>(a: MyGADT<T>) -> impl Foo {
//!   match a {
//!     MyGADT::SomeFoo(wit, a) => Witness { wit, a }, // Witness { wit, a } implements `Foo`
//!     _ => unimplemented!(),
//!   }
//! }
//! ```
//!

use std::marker::PhantomData;

pub trait Foo {
    fn foo(&self) -> bool;
}

/// This is the GADT being simulated
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum MyGADT<T> {
    SomeFoo(FooWitness<T>, T),
    SomeInt(TyEqWitness<T, bool>, usize),
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

    fn into_ty_eq(self) -> Self::Rhs
    where
        Self: Sized;

    fn from_ty_eq(rhs: Self::Rhs) -> Self
    where
        Self::Rhs: Sized;

    fn ty_eq_ref(&self) -> &Self::Rhs;

    fn from_ty_eq_ref(rhs: &Self::Rhs) -> &Self;

    fn ty_eq_mut(&mut self) -> &mut Self::Rhs;

    fn from_ty_eq_mut(rhs: &mut Self::Rhs) -> &mut Self;
}

#[allow(clippy::inline_always)]
impl<T: ?Sized> TyEq for T {
    type Rhs = T;

    #[inline(always)]
    fn into_ty_eq(self) -> T
    where
        T: Sized,
    {
        self
    }

    #[inline(always)]
    fn from_ty_eq(rhs: T) -> T
    where
        T: Sized,
    {
        rhs
    }

    #[inline(always)]
    fn ty_eq_ref(&self) -> &T {
        self
    }

    #[inline(always)]
    fn from_ty_eq_ref(rhs: &T) -> &T {
        rhs
    }

    #[inline(always)]
    fn ty_eq_mut(&mut self) -> &mut T {
        self
    }

    #[inline(always)]
    fn from_ty_eq_mut(rhs: &mut T) -> &mut T {
        rhs
    }
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

/// This is a type-witness for [`TyEq`], it can only be constructed for types that implement [`TyEq`]
/// for a specific [`Rhs`] type.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[repr(transparent)]
pub struct TyEqWitness<L: ?Sized, R: ?Sized>(PhantomData<L>, PhantomData<R>);

// In general, we can reuse witness structs as follows:
//
// Witness1<T>
// multiple impl blocks containing smart constructors for these witnesses ??

impl<L: ?Sized, R: ?Sized> TyEqWitness<L, R> {
    #[inline]
    #[must_use]
    pub const fn new() -> Self
    where
        L: TyEq<Rhs = R>,
    {
        Self(PhantomData, PhantomData)
    }
}

impl<L: TyEq<Rhs = R> + ?Sized, R: ?Sized> Default for TyEqWitness<L, R> {
    #[inline]
    #[must_use]
    fn default() -> Self {
        Self::new()
    }
}

#[inline]
#[must_use]
pub const fn is_ty_eq<L: TyEq<Rhs = R> + ?Sized, R: ?Sized>() -> TyEqWitness<L, R> {
    TyEqWitness::new()
}

/// This is an instance provider trait for [`TyEq`], it uses specialization to provide the correct
/// implementation of [`TyEq`] with static-dispatch, based on a supplied [`TyEqWitness`] witness value.
pub trait TyEqInstance<Rhs: ?Sized> {
    type Implemented: ty_bool::Bool;

    fn into_ty_eq_with(self, ty_eq_witness: &TyEqWitness<Self, Rhs>) -> Rhs
    where
        Self: Sized,
        Rhs: Sized;

    fn from_ty_eq_with(rhs: Rhs, ty_eq_witness: &TyEqWitness<Self, Rhs>) -> Self
    where
        Self: Sized,
        Rhs: Sized;

    fn ty_eq_ref_with(&self, ty_eq_witness: &TyEqWitness<Self, Rhs>) -> &Rhs;

    fn from_ty_eq_ref_with<'a>(rhs: &'a Rhs, ty_eq_witness: &TyEqWitness<Self, Rhs>) -> &'a Self;

    fn ty_eq_mut_with(&mut self, ty_eq_witness: &TyEqWitness<Self, Rhs>) -> &mut Rhs;

    fn from_ty_eq_mut_with<'a>(
        rhs: &'a mut Rhs,
        ty_eq_witness: &TyEqWitness<Self, Rhs>,
    ) -> &'a mut Self;
}

impl<L: ?Sized, R: ?Sized> TyEqInstance<R> for L {
    default type Implemented = ty_bool::False;

    #[inline]
    default fn into_ty_eq_with(self, _ty_eq_witness: &TyEqWitness<L, R>) -> R
    where
        L: Sized,
        R: Sized,
    {
        unreachable!()
    }

    #[inline]
    default fn from_ty_eq_with(_rhs: R, _ty_eq_witness: &TyEqWitness<L, R>) -> Self
    where
        L: Sized,
        R: Sized,
    {
        unreachable!()
    }

    #[inline]
    default fn ty_eq_ref_with(&self, _ty_eq_witness: &TyEqWitness<L, R>) -> &R {
        unreachable!()
    }

    #[inline]
    default fn from_ty_eq_ref_with<'a>(
        _rhs: &'a R,
        _ty_eq_witness: &TyEqWitness<L, R>,
    ) -> &'a Self {
        unreachable!()
    }

    #[inline]
    default fn ty_eq_mut_with(&mut self, _ty_eq_witness: &TyEqWitness<L, R>) -> &mut R {
        unreachable!()
    }

    #[inline]
    default fn from_ty_eq_mut_with<'a>(
        _rhs: &'a mut R,
        _ty_eq_witness: &TyEqWitness<L, R>,
    ) -> &'a mut Self {
        unreachable!()
    }
}

impl<L: TyEq<Rhs = R> + ?Sized, R: ?Sized> TyEqInstance<R> for L {
    default type Implemented = ty_bool::False;

    #[inline]
    default fn into_ty_eq_with(self, _ty_eq_witness: &TyEqWitness<L, R>) -> R
    where
        L: Sized,
        R: Sized,
    {
        self.into_ty_eq()
    }

    #[inline]
    default fn from_ty_eq_with(rhs: R, _ty_eq_witness: &TyEqWitness<L, R>) -> Self
    where
        L: Sized,
        R: Sized,
    {
        Self::from_ty_eq(rhs)
    }

    #[inline]
    default fn ty_eq_ref_with(&self, _ty_eq_witness: &TyEqWitness<L, R>) -> &R {
        self.ty_eq_ref()
    }

    #[inline]
    default fn from_ty_eq_ref_with<'a>(rhs: &'a R, _ty_eq_witness: &TyEqWitness<L, R>) -> &'a Self {
        Self::from_ty_eq_ref(rhs)
    }

    #[inline]
    default fn ty_eq_mut_with(&mut self, _ty_eq_witness: &TyEqWitness<L, R>) -> &mut R {
        self.ty_eq_mut()
    }

    #[inline]
    default fn from_ty_eq_mut_with<'a>(
        rhs: &'a mut R,
        _ty_eq_witness: &TyEqWitness<L, R>,
    ) -> &'a mut Self {
        Self::from_ty_eq_mut(rhs)
    }
}

// function which makes use of `MyGADT` in pattern matching:
// by construction, a value of type `FooWitness<T>` can only be inhabited if `T: Foo`
// so `FooInstance::foo_with` will __always__ specialize to the one that doesn't panic
#[inline]
pub fn do_thing<T>(a: &MyGADT<T>, b: T) -> bool {
    match *a {
        MyGADT::SomeFoo(ref wit, ref a) => a.foo_with(wit),
        MyGADT::SomeInt(ref wit, ref a) => {
            // at this point, I _know_ what type `T` is `bool`, hence:
            let b = <T as TyEqInstance<bool>>::into_ty_eq_with(b, wit);

            // I can now directly return `b` for example
            b
        }
        MyGADT::Other(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::rust_gadt_playground::{do_thing, is_foo, ty_bool, FooInstance, MyGADT};

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
        assert!(do_thing(&gadt_value, 23)); // OK
    }
}

// We can use type-equality to implement type equality constraints
