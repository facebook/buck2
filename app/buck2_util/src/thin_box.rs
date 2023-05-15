/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::from_iter_instead_of_collect)]

use std::alloc;
use std::alloc::Layout;
use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem;
use std::mem::MaybeUninit;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ptr;
use std::ptr::NonNull;
use std::slice;

use allocative::Allocative;

/// `Box<[T]>` but thin pointer.
///
/// Statically allocated for empty slice.
pub struct ThinBoxSlice<T> {
    /// Pointer to the first element.
    ///
    /// Memory layout:
    /// ```ignore
    /// [len, T, T, ...]
    ///       ^ points here
    /// ```
    ptr: NonNull<T>,
}

unsafe impl<T: Sync> Sync for ThinBoxSlice<T> {}
unsafe impl<T: Send> Send for ThinBoxSlice<T> {}

impl<T> ThinBoxSlice<T> {
    const _ASSERTS: () = {
        // Otherwise empty slice is not aligned properly.
        assert!(mem::align_of::<T>() <= mem::align_of::<usize>());
    };

    #[inline]
    pub const fn empty() -> ThinBoxSlice<T> {
        const LEN_ZERO: usize = 0;
        unsafe {
            let ptr = (&LEN_ZERO as *const usize).add(1) as *mut T;
            let ptr = NonNull::new_unchecked(ptr);
            ThinBoxSlice { ptr }
        }
    }

    /// Allocation layout for a slice of length `len`.
    #[inline]
    fn layout_for_len(len: usize) -> Layout {
        let (layout, offset_of_data) = Layout::new::<usize>()
            .extend(Layout::array::<T>(len).unwrap())
            .unwrap();
        assert_eq!(mem::size_of::<usize>(), offset_of_data);
        layout
    }

    /// Length of the slice.
    // Not called `len` to avoid overload with `Deref::len`.
    #[inline]
    fn read_len(&self) -> usize {
        unsafe { *self.ptr.cast::<usize>().as_ptr().sub(1) }
    }

    /// Allocate uninitialized memory for a slice of length `len`.
    #[inline]
    pub fn new_uninit(len: usize) -> ThinBoxSlice<MaybeUninit<T>> {
        if len == 0 {
            ThinBoxSlice::empty()
        } else {
            let layout = Self::layout_for_len(len);
            unsafe {
                let alloc = alloc::alloc(layout);
                if alloc.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                ptr::write(alloc as *mut usize, len);
                let ptr = alloc.add(mem::size_of::<usize>()) as *mut MaybeUninit<T>;
                let ptr = NonNull::new_unchecked(ptr);
                ThinBoxSlice { ptr }
            }
        }
    }

    #[inline]
    pub fn new<const N: usize>(data: [T; N]) -> ThinBoxSlice<T> {
        ThinBoxSlice::from_iter(data)
    }
}

impl<T> Deref for ThinBoxSlice<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.read_len()) }
    }
}

impl<T> DerefMut for ThinBoxSlice<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), self.read_len()) }
    }
}

impl<T> ThinBoxSlice<MaybeUninit<T>> {
    #[inline]
    pub unsafe fn assume_init(self) -> ThinBoxSlice<T> {
        let result = ThinBoxSlice {
            ptr: self.ptr.cast::<T>(),
        };
        mem::forget(self);
        result
    }
}

impl<T> Drop for ThinBoxSlice<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            let len = self.read_len();
            if len != 0 {
                let slice = ptr::slice_from_raw_parts_mut(self.ptr.as_ptr(), len);
                ptr::drop_in_place(slice);
                let alloc = self.ptr.cast::<usize>().as_ptr().sub(1);
                alloc::dealloc(alloc as *mut u8, Self::layout_for_len(len));
            }
        }
    }
}

impl<T> Default for ThinBoxSlice<T> {
    #[inline]
    fn default() -> Self {
        ThinBoxSlice::empty()
    }
}

impl<T: Debug> Debug for ThinBoxSlice<T> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <[T] as Debug>::fmt(&**self, f)
    }
}

impl<T: PartialEq> PartialEq for ThinBoxSlice<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        <[T] as PartialEq>::eq(&**self, &**other)
    }
}

impl<T: Eq> Eq for ThinBoxSlice<T> {}

impl<T: PartialOrd> PartialOrd for ThinBoxSlice<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        <[T] as PartialOrd>::partial_cmp(&**self, &**other)
    }
}

impl<T: Ord> Ord for ThinBoxSlice<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        <[T] as Ord>::cmp(&**self, &**other)
    }
}

impl<T: Hash> Hash for ThinBoxSlice<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        <[T] as Hash>::hash(&**self, state)
    }
}

impl<T> FromIterator<T> for ThinBoxSlice<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let (lower, upper) = iter.size_hint();
        if Some(lower) == upper {
            let mut thin = ThinBoxSlice::<T>::new_uninit(lower);
            let mut i = 0;
            for item in iter {
                assert!(i < lower, "iterator produced more than promised");
                MaybeUninit::write(&mut thin[i], item);
                i += 1;
            }
            assert_eq!(i, lower, "iterator produced less than promised");
            unsafe { thin.assume_init() }
        } else {
            // TODO(nga): we can collect into partially initialized `ThinBoxSlice`
            //   to get a chance of avoiding last reallocation.
            let vec = Vec::from_iter(iter);
            Self::from_iter(vec)
        }
    }
}

impl<T: Allocative> Allocative for ThinBoxSlice<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        {
            let mut visitor =
                visitor.enter_unique(allocative::Key::new("ptr"), mem::size_of_val(&self.ptr));
            {
                let mut visitor = visitor.enter(
                    allocative::Key::new("alloc"),
                    Self::layout_for_len(self.len()).size(),
                );
                visitor.visit_simple(allocative::Key::new("len"), mem::size_of::<usize>());
                {
                    let mut visitor = visitor.enter(allocative::Key::new("data"), self.len());
                    visitor.visit_slice::<T>(self);
                    visitor.exit();
                }
                visitor.exit();
            }
            visitor.exit();
        }
        visitor.exit();
    }
}

#[cfg(test)]
mod tests {
    use crate::thin_box::ThinBoxSlice;

    #[test]
    fn test_empty() {
        assert_eq!(0, ThinBoxSlice::<String>::empty().len());
    }

    #[test]
    fn test_from_iter_sized() {
        let thin = ThinBoxSlice::from_iter(["a".to_owned(), "b".to_owned(), "c".to_owned()]);
        assert_eq!(["a".to_owned(), "b".to_owned(), "c".to_owned()], *thin);
    }

    #[test]
    fn test_from_iter_unknown_size() {
        let thin = ThinBoxSlice::from_iter(
            ["a".to_owned(), "b".to_owned(), "c".to_owned()]
                .into_iter()
                .filter(|_| true),
        );
        assert_eq!(["a".to_owned(), "b".to_owned(), "c".to_owned()], *thin);
    }

    /// If there are obvious memory violations, this test will catch them.
    #[test]
    fn test_stress() {
        for i in 0..1000 {
            let slice = ThinBoxSlice::from_iter((0..i).map(|j| j.to_string()));
            assert_eq!(i, slice.len());
            assert_eq!((0..i).map(|j| j.to_string()).collect::<Vec<_>>(), *slice);
        }
    }
}
