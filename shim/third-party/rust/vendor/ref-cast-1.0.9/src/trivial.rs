use core::marker::PhantomData;

pub trait Trivial {}

impl Trivial for () {}
impl<T: ?Sized> Trivial for PhantomData<T> {}

pub fn assert_trivial<T: Trivial>() {}
