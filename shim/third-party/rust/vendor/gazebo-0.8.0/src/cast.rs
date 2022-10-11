/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Cast between types with no conversion.
//!
//! Most of these operations are inherently unsafe, but provided as strongly-typed wrappers
//! to reduce the chance of typos ending up with even more unsafe functions. If you use the
//! result in incorrect ways, it will cause undefined behaviour.

// These are inherently unsafe in fairly obvious ways. Safety is left entirely to the user.
// So doc's wouldn't really help.
#![allow(clippy::missing_safety_doc)]

use std::mem;
use std::ptr;

#[inline(always)]
pub fn ptr_to_usize<T: ?Sized>(x: &T) -> usize {
    x as *const T as *const () as usize
}

/// Undefined behaviour if the argument is zero, or does not satisfy the alignment
/// of type `T`.
#[inline(always)]
pub unsafe fn usize_to_ptr<'a, T>(x: usize) -> &'a T {
    &*(x as *const T)
}

/// Undefined behaviour if the argument does not satisfy the alignment of type `To`.
#[inline(always)]
pub unsafe fn ptr<From, To>(x: &From) -> &To {
    &*(x as *const From as *const To)
}

/// Undefined behaviour if the argument does not satisfy the alignment of type `To`.
#[inline(always)]
pub unsafe fn ptr_mut<From, To>(x: &mut From) -> &mut To {
    &mut *(x as *mut From as *mut To)
}

#[inline(always)]
pub unsafe fn ptr_lifetime<'a, 'b, T: ?Sized>(x: &'a T) -> &'b T {
    &*(x as *const T)
}

/// Like normal [`transmute`](std::mem::transmute), but without the compile-time
/// check that the sizes of the input and output are the same. Despite the removal
/// of the compile-time check, this property _must still hold_, and there is
/// a [`debug_assert_eq`] which checks that. All the rules and warnings from
/// `transmute` still apply.
///
/// This function is mostly useful when you have types that are polymorphic,
/// e.g. `Vec<T>`, that `transmute` cannot be applied to.
#[inline]
pub unsafe fn transmute_unchecked<A, B>(x: A) -> B {
    assert_eq!(mem::size_of::<A>(), mem::size_of::<B>());
    debug_assert_eq!(0, (&x as *const A).align_offset(mem::align_of::<B>()));
    let b = ptr::read(&x as *const A as *const B);
    mem::forget(x);
    b
}

#[macro_export]
/// `transmute!(from-type, to-type, value)` will do a [`transmute`](std::mem::transmute),
/// but the original and result types must be specified.
macro_rules! transmute {
    ($from:ty, $to:ty, $e:expr) => {
        std::mem::transmute::<$from, $to>($e)
    };
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    use super::*;

    #[test]
    fn test_transmute() {
        #[allow(clippy::useless_transmute)]
        unsafe fn downcast_string<'a>(x: &'a str) -> &'static str {
            transmute!(&'a str, &'static str, x)
        }
        assert_eq!(unsafe { downcast_string("test") }, "test");
    }

    struct Foo<'a, T>(&'a Cell<isize>, &'a T);

    impl<T> Drop for Foo<'_, T> {
        fn drop(&mut self) {
            self.0.set(self.0.get() - 1)
        }
    }

    #[test]
    fn test_transmute_unchecked() {
        fn magic<'a, T>(x: T) -> Foo<'a, Vec<u8>> {
            unsafe { transmute_unchecked(x) }
        }

        let s = "magic".to_owned();
        let c = Cell::new(8);
        let input = Foo(&c, &s);
        let output = magic(input);
        assert_eq!(c.get(), 8);
        assert_eq!(output.1, "magic".as_bytes());
        mem::drop(output);
        assert_eq!(c.get(), 7);
    }

    #[test]
    fn test_transmute_unchecked_bytes() {
        fn to_bytes<T>(x: T) -> [u8; 8] {
            unsafe { transmute_unchecked(x) }
        }

        fn from_bytes<T>(x: [u8; 8]) -> T {
            unsafe { transmute_unchecked(x) }
        }

        let x: u64 = 8932489732;
        let y: u64 = from_bytes(to_bytes(x));
        assert_eq!(x, y)
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic]
    fn test_transmute_wrong_sizes() {
        let _: u64 = unsafe { transmute_unchecked(89u32) };
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic]
    fn test_transmute_wrong_alignment() {
        let xs: [u8; 8] = unsafe { transmute_unchecked(0u64) };
        let _: u32 = unsafe { transmute_unchecked(&xs[1..5]) };
    }
}
