use crate::rust_gadt_playground::TyEqWitness;

pub trait MyTrait {}
pub type MyTraitDyn = dyn MyTrait;

pub struct I32Ptr {
    ptr: i32,
}

// `data Op r where`
pub enum Op<R> {
    // `OpRead :: IntPtr -> Op Int`
    Read(I32Ptr, TyEqWitness<R, i32>),

    // `OpWrite :: IntPtr -> Int -> Op ()`
    Write(I32Ptr, i32, TyEqWitness<R, ()>),

    // `OpCAS :: IntPtr -> Int -> Int -> Op Bool`
    CAS(I32Ptr, i32, i32, TyEqWitness<R, bool>), // compare and swap
}

// `data Program a where`
pub enum Program<A, R = ()> {
    // Ideally, we would exclude `R` entirely, like in Haskell
    // `Return :: a -> Program a`
    Return(A),

    // `Step :: Op r -> (r -> Program a) -> Program a`
    Step(Op<R>, Box<dyn FnOnce(R) -> Program<A>>), // It is unfortunate that we have to box the continuation function
                                                   // Alternatively, it would be yet another generic type-parameter??
                                                   //
                                                   // also `Program<A> == Program<A, ()>` which is clearly incorrect??...
}

#[inline]
pub fn boxed_fn1<'a, T, R, F: Fn(T) -> R + 'a>(f: F) -> Box<dyn Fn(T) -> R + 'a> {
    Box::new(f)
}

pub fn lift<A>(i: Op<A>) -> Program<A> {
    let foo: Box<dyn Fn(A) -> Program<A>> = boxed_fn1(Program::Return);

    // Program::Step(i, todo!());

    todo!()
}
