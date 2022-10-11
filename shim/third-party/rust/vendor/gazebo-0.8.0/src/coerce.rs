/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A trait to represent zero-cost conversions.

use std::alloc::Layout;
use std::collections::HashMap;
use std::collections::HashSet;

pub use gazebo_derive::Coerce;

use crate::cast::transmute_unchecked;

/// A marker trait such that the existence of `From: Coerce<To>` implies
/// that `From` can be treat as `To` without any data manipulation.
/// Particularly useful for containers, e.g. `Vec<From>` can be treated as
/// `Vec<To>` in _O(1)_. If such an instance is available,
/// you can use [`coerce`] to perform the conversion.
///
/// Importantly, you must make sure Rust does not change the type representation
/// between the different types (typically using a `repr` directive),
/// and it must be safe for the `From` to be treated as `To`, namely same (or less restrictive) alignment,
/// no additional invariants, value can be dropped as `To`.
///
/// One use of `Coerce` is around newtype wrappers:
///
/// ```
/// use gazebo::coerce::{Coerce, coerce};
/// #[repr(transparent)]
/// #[derive(Debug, Coerce)]
/// struct Wrapper(String);
///
/// let value = vec![Wrapper("hello".to_owned()), Wrapper("world".to_owned())];
/// assert_eq!(
///     coerce::<_, &Vec<String>>(&value).join(" "),
///     "hello world"
/// );
/// let mut value = coerce::<_, Vec<String>>(value);
/// assert_eq!(value.pop(), Some("world".to_owned()));
/// ```
///
/// Another involves containers:
///
/// ```
/// use gazebo::coerce::{Coerce, coerce};
/// # #[derive(Coerce)]
/// # #[repr(transparent)]
/// # struct Wrapper(String);
/// #[derive(Coerce)]
/// #[repr(C)]
/// struct Container<T>(i32, T);
///
/// let value = Container(20, Wrapper("twenty".to_owned()));
/// assert_eq!(
///     coerce::<_, &Container<String>>(&value).1,
///     "twenty"
/// );
/// ```
///
/// If you only need [`coerce`] on newtype references,
/// then the [`ref-cast` crate](https://crates.io/crates/ref-cast)
/// provides that, along with automatic derivations (no `unsafe` required).
pub unsafe trait Coerce<To: ?Sized> {}

/// A marker trait such that the existence of `From: CoerceKey<To>` implies
/// that `From` can be treat as `To` without any data manipulation.
/// Furthermore, above and beyond [`Coerce`], any provided [`Hash`](std::hash::Hash),
/// [`Eq`], [`PartialEq`], [`Ord`] and [`PartialOrd`] traits must give identical results
/// on the `From` and `To` values.
///
/// This trait is mostly expected to be a requirement for the keys of associative-map
/// containers, hence the `Key` in the name.
pub unsafe trait CoerceKey<To: ?Sized>: Coerce<To> {}

unsafe impl<'a, From: ?Sized, To: ?Sized> Coerce<&'a To> for &'a From where From: Coerce<To> {}
unsafe impl<'a, From: ?Sized, To: ?Sized> CoerceKey<&'a To> for &'a From where From: CoerceKey<To> {}

unsafe impl<From, To> Coerce<[To]> for [From] where From: Coerce<To> {}
unsafe impl<From, To> CoerceKey<[To]> for [From] where From: CoerceKey<To> {}

unsafe impl<From, To> Coerce<Vec<To>> for Vec<From> where From: Coerce<To> {}
unsafe impl<From, To> CoerceKey<Vec<To>> for Vec<From> where From: CoerceKey<To> {}

unsafe impl<From: ?Sized, To: ?Sized> CoerceKey<Box<To>> for Box<From> where From: CoerceKey<To> {}
unsafe impl<From: ?Sized, To: ?Sized> Coerce<Box<To>> for Box<From> where From: Coerce<To> {}

unsafe impl<From, To> Coerce<HashSet<To>> for HashSet<From> where From: CoerceKey<To> {}

unsafe impl<FromK, FromV, ToK, ToV> Coerce<HashMap<ToK, ToV>> for HashMap<FromK, FromV>
where
    FromK: CoerceKey<ToK>,
    FromV: Coerce<ToV>,
{
}

unsafe impl<From1: Coerce<To1>, To1> Coerce<(To1,)> for (From1,) {}
unsafe impl<From1: CoerceKey<To1>, To1> CoerceKey<(To1,)> for (From1,) {}

unsafe impl<From1: Coerce<To1>, From2: Coerce<To2>, To1, To2> Coerce<(To1, To2)>
    for (From1, From2)
{
}
unsafe impl<From1: CoerceKey<To1>, From2: CoerceKey<To2>, To1, To2> CoerceKey<(To1, To2)>
    for (From1, From2)
{
}
unsafe impl<From: Coerce<To>, To, const N: usize> Coerce<[To; N]> for [From; N] {}
unsafe impl<From: CoerceKey<To>, To, const N: usize> CoerceKey<[To; N]> for [From; N] {}

// We can't define a blanket `Coerce<T> for T` because that conflicts with the specific traits above.
// Therefore, we define instances where we think they might be useful, rather than trying to do every concrete type.
unsafe impl Coerce<String> for String {}
unsafe impl CoerceKey<String> for String {}

unsafe impl Coerce<str> for str {}
unsafe impl CoerceKey<str> for str {}

unsafe impl Coerce<()> for () {}
unsafe impl CoerceKey<()> for () {}

/// Safely convert between types which have a `Coerce` relationship.
/// Often the second type argument will need to be given explicitly,
/// e.g. `coerce::<_, ToType>(x)`.
#[inline]
pub fn coerce<From, To>(x: From) -> To
where
    From: Coerce<To>,
{
    assert_eq!(Layout::new::<From>(), Layout::new::<To>());
    unsafe { transmute_unchecked(x) }
}

#[cfg(test)]
mod tests {
    use std::marker;

    use super::*;
    use crate as gazebo;

    #[test]
    fn test_ptr_coerce() {
        fn f<'v>(x: (&'static str,)) -> (&'v str,) {
            coerce(x)
        }

        let x = "test".to_owned();
        assert_eq!(f(("test",)), (x.as_str(),))
    }

    #[test]
    fn test_coerce_lifetime() {
        #[derive(Coerce)]
        #[repr(transparent)]
        struct NewtypeWithLifetime<'v>(&'v [usize]);

        let newtype = NewtypeWithLifetime(&[1, 2]);
        assert_eq!(&[1, 2], coerce(newtype))
    }

    #[test]
    fn test_coerce_type_and_lifetime_params() {
        #[derive(Coerce)]
        #[repr(C)]
        struct Aaa<'a>(&'a u32);
        #[derive(Coerce)]
        #[repr(C)]
        struct Bbb<'a>(&'a u32);

        unsafe impl<'a> Coerce<Bbb<'a>> for Aaa<'a> {}

        #[derive(Coerce)]
        #[repr(C)]
        struct StructWithLifetimeAndTypeParams<'a, X> {
            x: X,
            _marker: marker::PhantomData<&'a u32>,
        }

        let ten = 10;
        let old = StructWithLifetimeAndTypeParams::<Aaa> {
            x: Aaa(&ten),
            _marker: marker::PhantomData,
        };

        let new: StructWithLifetimeAndTypeParams<Bbb> = coerce(old);
        assert_eq!(10, *new.x.0);
    }
}
