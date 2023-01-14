/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Additions to the [`Ref`](Ref) mechanism.

// We used to implement `ARef` as an enum of `{Ptr(&'a T), Ref(Ref<'a, T>)}`.
// That works, but consumes 3 words and requires a branch on every access of the underlying
// pointer. We can optimise that by relying on the underlying details of `Ref`, which is
// (currently) defined as (after a bit of inlining):
//
// ```
// pub struct Ref<'a, T: ?Sized + 'a> {
//    value: &'a T,
//    borrow: &'a Cell<isize>,
// }
// ```
//
// Because the pointer must always be non-null, we can switch that out for:
//
// ```
// pub struct ARef<'a, T: ?Sized + 'a> {
//    value: &'a T,
//    borrow: Option<&'a Cell<isize>>,
// }
// ```
//
// And use `None` to represent the `Ptr` case. We do that with transmute trickery,
// but write some good tests that will break if the representation changes,
// and if necessary we can always switch to the enum representation.

use std::cell::BorrowError;
use std::cell::Ref;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;

#[derive(Debug)]
enum ARefImpl<'a, T: ?Sized + 'a> {
    Ptr(&'a T),
    Ref(Ref<'a, T>),
}

/// A [`Ref`](Ref) that might not actually be borrowed.
/// Either a `Ptr` (a normal & style reference), or a `Ref` (like from
/// [`RefCell`](std::cell::RefCell)), but exposes all the methods available on [`Ref`](Ref).
#[derive(Debug)]
pub struct ARef<'a, T: ?Sized + 'a>(ARefImpl<'a, T>);

impl<T: ?Sized> Deref for ARef<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        match &self.0 {
            ARefImpl::Ptr(p) => p,
            ARefImpl::Ref(r) => r.deref(),
        }
    }
}

impl<'a, T: ?Sized + 'a> ARef<'a, T> {
    /// Create a new [`ARef`] from a pointer.
    pub fn new_ptr(x: &'a T) -> Self {
        ARef(ARefImpl::Ptr(x))
    }

    /// Create a new [`ARef`] from a reference.
    pub fn new_ref(x: Ref<'a, T>) -> Self {
        ARef(ARefImpl::Ref(x))
    }

    /// See [`Ref.clone`](Ref::clone). Not a self method since that interferes with the [`Deref`](Deref).
    #[allow(clippy::should_implement_trait)]
    pub fn clone(orig: &Self) -> Self {
        match &orig.0 {
            ARefImpl::Ptr(p) => ARef(ARefImpl::Ptr(p)),
            ARefImpl::Ref(r) => ARef(ARefImpl::Ref(Ref::clone(r))),
        }
    }

    /// See [`Ref.map`](Ref::map). Not a self method since that interferes with the [`Deref`](Deref).
    pub fn map<U: ?Sized, F>(orig: ARef<'a, T>, f: F) -> ARef<'a, U>
    where
        F: FnOnce(&T) -> &U,
    {
        match orig.0 {
            ARefImpl::Ptr(p) => ARef(ARefImpl::Ptr(f(p))),
            ARefImpl::Ref(r) => ARef(ARefImpl::Ref(Ref::map(r, f))),
        }
    }

    /// See [`Ref.map_split`](Ref::map_split). Not a self method since that interferes with the
    /// [`Deref`](Deref).
    pub fn map_split<U: ?Sized, V: ?Sized, F>(orig: ARef<'a, T>, f: F) -> (ARef<'a, U>, ARef<'a, V>)
    where
        F: FnOnce(&T) -> (&U, &V),
    {
        match orig.0 {
            ARefImpl::Ptr(p) => {
                let (u, v) = f(p);
                (ARef(ARefImpl::Ptr(u)), ARef(ARefImpl::Ptr(v)))
            }
            ARefImpl::Ref(r) => {
                let (u, v) = Ref::map_split(r, f);
                (ARef(ARefImpl::Ref(u)), ARef(ARefImpl::Ref(v)))
            }
        }
    }

    /// See [`Ref.filter_map`](Ref::filter_map). Not a self method since that interferes with the
    /// [`Deref`](Deref).
    pub fn filter_map<U: ?Sized, F>(orig: ARef<'a, T>, f: F) -> Result<ARef<'a, U>, Self>
    where
        F: FnOnce(&T) -> Option<&U>,
    {
        match orig.0 {
            ARefImpl::Ptr(p) => match f(p) {
                Some(u) => Ok(ARef(ARefImpl::Ptr(u))),
                None => Err(ARef(ARefImpl::Ptr(p))),
            },
            ARefImpl::Ref(r) => match Ref::filter_map(r, f) {
                Ok(u) => Ok(ARef(ARefImpl::Ref(u))),
                Err(r) => Err(ARef(ARefImpl::Ref(r))),
            },
        }
    }
}

// `Ref` doesn't have many traits on it. I don't really know why - I think that's an oversight.
// & references do have many traits on them. Therefore, when being "either" we choose to do as many
// implementations as we can.

impl<T: Display + ?Sized> Display for ARef<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ARef::deref(self).fmt(f)
    }
}

impl<T: Hash + ?Sized> Hash for ARef<'_, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ARef::deref(self).hash(state)
    }
}

impl<A: PartialEq<B> + ?Sized, B: ?Sized> PartialEq<ARef<'_, B>> for ARef<'_, A> {
    fn eq(&self, other: &ARef<'_, B>) -> bool {
        ARef::deref(self).eq(ARef::deref(other))
    }
}

impl<A: Eq + ?Sized> Eq for ARef<'_, A> {}

impl<A: PartialOrd<B> + ?Sized, B: ?Sized> PartialOrd<ARef<'_, B>> for ARef<'_, A> {
    fn partial_cmp(&self, other: &ARef<'_, B>) -> Option<Ordering> {
        ARef::deref(self).partial_cmp(ARef::deref(other))
    }
}

impl<A: Ord + ?Sized> Ord for ARef<'_, A> {
    fn cmp(&self, other: &Self) -> Ordering {
        ARef::deref(self).cmp(ARef::deref(other))
    }
}

/// Obtain an [`ARef`] from either a normal pointer or a [`RefCell`](std::cell::RefCell).
pub trait AsARef<T: ?Sized> {
    /// Get an [`ARef`] pointing at this type.
    fn as_aref(this: &Self) -> ARef<T>;
    /// Try and get an [`ARef`] pointing at this type. Returns an [`Err`] if
    /// the type `Self` is a [`RefCell`] which is already mutably borrowed.
    fn try_as_aref(this: &Self) -> Result<ARef<T>, BorrowError>;
    /// Return the underlying [`RefCell`] if `Self` is one, otherwise [`None`].
    fn as_ref_cell(this: &Self) -> Option<&RefCell<T>>;
}

impl<T: ?Sized> AsARef<T> for T {
    fn as_aref(this: &Self) -> ARef<T> {
        ARef::new_ptr(this)
    }
    fn try_as_aref(this: &Self) -> Result<ARef<T>, BorrowError> {
        Ok(ARef::new_ptr(this))
    }
    fn as_ref_cell(_this: &Self) -> Option<&RefCell<T>> {
        None
    }
}

impl<T: ?Sized> AsARef<T> for RefCell<T> {
    fn as_aref(this: &Self) -> ARef<T> {
        ARef::new_ref(this.borrow())
    }
    fn try_as_aref(this: &Self) -> Result<ARef<T>, BorrowError> {
        Ok(ARef::new_ref(this.try_borrow()?))
    }
    fn as_ref_cell(this: &Self) -> Option<&RefCell<T>> {
        Some(this)
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::mem;

    use super::*;
    use crate::cast;

    #[test]
    fn test_from_ref_docs() {
        let c = RefCell::new((5, 'b'));
        let b1: ARef<(u32, char)> = ARef::new_ref(c.borrow());
        let b2: ARef<u32> = ARef::map(b1, |t| &t.0);
        assert_eq!(*b2, 5);

        let cell = RefCell::new([1, 2, 3, 4]);
        let borrow = ARef::new_ref(cell.borrow());
        let (begin, end) = ARef::map_split(borrow, |slice| slice.split_at(2));
        assert_eq!(*begin, [1, 2]);
        assert_eq!(*end, [3, 4]);
    }

    #[test]
    fn test_borrow_guards() {
        let c = RefCell::new(5);
        assert!(c.try_borrow_mut().is_ok());
        let r1 = ARef::new_ref(c.borrow());
        assert!(c.try_borrow_mut().is_err());
        let r2 = c.borrow();
        assert!(c.try_borrow_mut().is_err());
        mem::drop(r1);
        assert!(c.try_borrow_mut().is_err());
        mem::drop(r2);
        assert!(c.try_borrow_mut().is_ok());
    }

    #[test]
    fn test_pointer_basics() {
        let c = "test".to_owned();
        let p = ARef::new_ptr(&c);
        let p2 = ARef::map(p, |x| &x[1..3]);
        assert_eq!(&*p2, "es");
    }

    #[test]
    fn test_ref_map_dropping() {
        let c = RefCell::new("test".to_owned());
        let p = ARef::new_ref(c.borrow());
        let p = ARef::map(p, |x| &x[1..3]);
        assert_eq!(&*p, "es");
        mem::drop(p);
        assert!(c.try_borrow_mut().is_ok());
    }

    #[test]
    fn test_ref_filter_map_dropping() {
        let c = RefCell::new("test".to_owned());
        let p = ARef::new_ref(c.borrow());
        let p = ARef::filter_map(p, |x| Some(&x[1..3])).unwrap();
        assert_eq!(&*p, "es");
        mem::drop(p);
        assert!(c.try_borrow_mut().is_ok());
    }

    #[test]
    /// Test that the representation of ARef is what we expect
    fn test_ref_as_expected() {
        let orig = RefCell::new("test".to_owned());
        let p = orig.borrow();
        let p2 = Ref::clone(&p);
        let (pointer, cell): (usize, usize) = unsafe { mem::transmute(p) };
        // We expect the first to be a pointer to the underlying string
        assert_eq!(pointer, cast::ptr_to_usize(Ref::deref(&p2)));
        // We want to make sure the second is never zero
        assert_ne!(cell, 0);

        // Put it back as it was, to make sure our test doesn't leak memory
        let _ignore: Ref<String> = unsafe { mem::transmute((pointer, cell)) };
    }

    #[test]
    fn test_as_aref() {
        fn get_str(x: &impl AsARef<String>) -> ARef<str> {
            ARef::map(AsARef::as_aref(x), |x| x.as_str())
        }

        let a = RefCell::new("hello".to_owned());
        let b = "world".to_owned();
        assert_eq!(&*get_str(&a), "hello");
        assert_eq!(&*get_str(&b), "world");
    }
}
