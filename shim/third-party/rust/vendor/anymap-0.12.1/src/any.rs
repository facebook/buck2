//! The different types of `Any` for use in a map.
//!
//! This stuff is all based on `std::any`, but goes a little further, with `CloneAny` being a
//! cloneable `Any` and with the `Send` and `Sync` bounds possible on both `Any` and `CloneAny`.

use std::fmt;
use std::any::Any as StdAny;

#[doc(hidden)]
pub trait CloneToAny {
    /// Clone `self` into a new `Box<CloneAny>` object.
    fn clone_to_any(&self) -> Box<CloneAny>;

    /// Clone `self` into a new `Box<CloneAny + Send>` object.
    fn clone_to_any_send(&self) -> Box<CloneAny + Send> where Self: Send;

    /// Clone `self` into a new `Box<CloneAny + Sync>` object.
    fn clone_to_any_sync(&self) -> Box<CloneAny + Sync> where Self: Sync;

    /// Clone `self` into a new `Box<CloneAny + Send + Sync>` object.
    fn clone_to_any_send_sync(&self) -> Box<CloneAny + Send + Sync> where Self: Send + Sync;
}

impl<T: Any + Clone> CloneToAny for T {
    #[inline]
    fn clone_to_any(&self) -> Box<CloneAny> {
        Box::new(self.clone())
    }

    #[inline]
    fn clone_to_any_send(&self) -> Box<CloneAny + Send> where Self: Send {
        Box::new(self.clone())
    }

    #[inline]
    fn clone_to_any_sync(&self) -> Box<CloneAny + Sync> where Self: Sync {
        Box::new(self.clone())
    }

    #[inline]
    fn clone_to_any_send_sync(&self) -> Box<CloneAny + Send + Sync> where Self: Send + Sync {
        Box::new(self.clone())
    }
}

macro_rules! define {
    (CloneAny) => {
        /// A type to emulate dynamic typing.
        ///
        /// Every type with no non-`'static` references implements `Any`.
        define!(CloneAny remainder);
    };
    (Any) => {
        /// A type to emulate dynamic typing with cloning.
        ///
        /// Every type with no non-`'static` references that implements `Clone` implements `Any`.
        define!(Any remainder);
    };
    ($t:ident remainder) => {
        /// See the [`std::any` documentation](https://doc.rust-lang.org/std/any/index.html) for
        /// more details on `Any` in general.
        ///
        /// This trait is not `std::any::Any` but rather a type extending that for this library’s
        /// purposes so that it can be combined with marker traits like 
        /// <code><a class=trait title=core::marker::Send
        /// href=http://doc.rust-lang.org/std/marker/trait.Send.html>Send</a></code> and
        /// <code><a class=trait title=core::marker::Sync
        /// href=http://doc.rust-lang.org/std/marker/trait.Sync.html>Sync</a></code>.
        ///
        define!($t trait);
    };
    (CloneAny trait) => {
        /// See also [`Any`](trait.Any.html) for a version without the `Clone` requirement.
        pub trait CloneAny: Any + CloneToAny { }
        impl<T: StdAny + Clone> CloneAny for T { }
    };
    (Any trait) => {
        /// See also [`CloneAny`](trait.CloneAny.html) for a cloneable version of this trait.
        pub trait Any: StdAny { }
        impl<T: StdAny> Any for T { }
    };
}

macro_rules! impl_clone {
    ($t:ty, $method:ident) => {
        impl Clone for Box<$t> {
            #[inline]
            fn clone(&self) -> Box<$t> {
                (**self).$method()
            }
        }
    }
}

#[allow(missing_docs)]  // Bogus warning (it’s not public outside the crate), ☹
pub trait UncheckedAnyExt: Any {
    unsafe fn downcast_ref_unchecked<T: Any>(&self) -> &T;
    unsafe fn downcast_mut_unchecked<T: Any>(&mut self) -> &mut T;
    unsafe fn downcast_unchecked<T: Any>(self: Box<Self>) -> Box<T>;
}

#[doc(hidden)]
/// A trait for the conversion of an object into a boxed trait object.
pub trait IntoBox<A: ?Sized + UncheckedAnyExt>: Any {
    /// Convert self into the appropriate boxed form.
    fn into_box(self) -> Box<A>;
}

macro_rules! implement {
    ($base:ident, $(+ $bounds:ident)*) => {
        impl fmt::Debug for $base $(+ $bounds)* {
            #[inline]
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.pad(stringify!($base $(+ $bounds)*))
            }
        }

        impl UncheckedAnyExt for $base $(+ $bounds)* {
            #[inline]
            unsafe fn downcast_ref_unchecked<T: 'static>(&self) -> &T {
                &*(self as *const Self as *const T)
            }

            #[inline]
            unsafe fn downcast_mut_unchecked<T: 'static>(&mut self) -> &mut T {
                &mut *(self as *mut Self as *mut T)
            }

            #[inline]
            unsafe fn downcast_unchecked<T: 'static>(self: Box<Self>) -> Box<T> {
                Box::from_raw(Box::into_raw(self) as *mut T)
            }
        }

        impl<T: $base $(+ $bounds)*> IntoBox<$base $(+ $bounds)*> for T {
            #[inline]
            fn into_box(self) -> Box<$base $(+ $bounds)*> {
                Box::new(self)
            }
        }
    }
}

define!(Any);
implement!(Any,);
implement!(Any, + Send);
implement!(Any, + Sync);
implement!(Any, + Send + Sync);
implement!(CloneAny,);
implement!(CloneAny, + Send);
implement!(CloneAny, + Sync);
implement!(CloneAny, + Send + Sync);

define!(CloneAny);
impl_clone!(CloneAny, clone_to_any);
impl_clone!((CloneAny + Send), clone_to_any_send);
impl_clone!((CloneAny + Sync), clone_to_any_sync);
impl_clone!((CloneAny + Send + Sync), clone_to_any_send_sync);
