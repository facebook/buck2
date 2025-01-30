/*
 * Copyright 2018 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! This type is a copy-paste of `buck2_util::thin_box::ThinBoxSlice`, with some mild adjustments.
//!
//! Specifically:
//!  1. This type guarantees that it's always a pointer with the bottom bit zero.
//!  2. This type is not implicitly dropped - `run_drop` must be called explicitly.

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

#[repr(C)]
struct ThinBoxSliceLayout<T> {
    len: usize,
    data: [T; 0],
}

impl<T> ThinBoxSliceLayout<T> {
    fn offset_of_data() -> usize {
        mem::offset_of!(ThinBoxSliceLayout::<T>, data)
    }
}

/// `Box<[T]>` but thin pointer.
///
/// Statically allocated for empty slice.
// We don't really need `'static` here, but we hit type checker limitations.
pub(super) struct AllocatedThinBoxSlice<T: 'static> {
    /// Pointer to the first element, `ThinBoxSliceLayout.data`.
    ptr: NonNull<T>,
}

unsafe impl<T: Sync> Sync for AllocatedThinBoxSlice<T> {}
unsafe impl<T: Send> Send for AllocatedThinBoxSlice<T> {}

impl<T: 'static> AllocatedThinBoxSlice<T> {
    #[inline]
    pub(super) const fn empty() -> AllocatedThinBoxSlice<T> {
        const fn instance<T>() -> &'static ThinBoxSliceLayout<T> {
            assert!(mem::size_of::<ThinBoxSliceLayout<T>>() <= mem::size_of::<u128>());
            assert!(mem::align_of::<ThinBoxSliceLayout<T>>() <= mem::align_of::<u128>());
            // Instead of just statically allocating a `ThinBoxSliceLayout<T>` we allocate a
            // `0_u128`. The reason for this is a rustc bug around const UB checks that otherwise
            // incorrectly fires: https://github.com/rust-lang/rust/issues/133523
            //
            // SAFETY: We just checked that the layout is small enough to fit in a u128.
            unsafe { &*ptr::from_ref(&0u128).cast::<ThinBoxSliceLayout<T>>() }
        }

        unsafe {
            AllocatedThinBoxSlice {
                ptr: NonNull::new_unchecked(instance::<T>().data.as_ptr() as *mut T),
            }
        }
    }

    /// Allocation layout for a slice of length `len`.
    #[inline]
    fn layout_for_len(len: usize) -> Layout {
        let (layout, _offset_of_data) = Layout::new::<ThinBoxSliceLayout<T>>()
            .extend(Layout::array::<T>(len).unwrap())
            .unwrap();
        layout
    }

    /// Length of the slice.
    // Not called `len` to avoid overload with `Deref::len`.
    #[inline]
    fn read_len(&self) -> usize {
        unsafe {
            (*self
                .ptr
                .as_ptr()
                .cast::<u8>()
                .sub(ThinBoxSliceLayout::<T>::offset_of_data())
                .cast::<ThinBoxSliceLayout<T>>())
            .len
        }
    }

    /// Allocate uninitialized memory for a slice of length `len`.
    #[inline]
    pub(super) fn new_uninit(len: usize) -> AllocatedThinBoxSlice<MaybeUninit<T>> {
        if len == 0 {
            AllocatedThinBoxSlice::empty()
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
                AllocatedThinBoxSlice { ptr }
            }
        }
    }

    pub const unsafe fn into_inner(self) -> NonNull<T> {
        self.ptr
    }

    pub const unsafe fn from_inner(ptr: NonNull<T>) -> Self {
        Self { ptr }
    }
}

impl<T: 'static> Deref for AllocatedThinBoxSlice<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { slice::from_raw_parts(self.ptr.as_ptr(), self.read_len()) }
    }
}

impl<T: 'static> DerefMut for AllocatedThinBoxSlice<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr(), self.read_len()) }
    }
}

impl<T> AllocatedThinBoxSlice<MaybeUninit<T>> {
    #[inline]
    unsafe fn assume_init(self) -> AllocatedThinBoxSlice<T> {
        AllocatedThinBoxSlice {
            ptr: self.ptr.cast::<T>(),
        }
    }
}

impl<T: 'static> AllocatedThinBoxSlice<T> {
    #[inline]
    pub(super) fn run_drop(self) {
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

impl<T: 'static> Default for AllocatedThinBoxSlice<T> {
    #[inline]
    fn default() -> Self {
        AllocatedThinBoxSlice::empty()
    }
}

impl<T: Debug> Debug for AllocatedThinBoxSlice<T> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <[T] as Debug>::fmt(&**self, f)
    }
}

impl<T: PartialEq> PartialEq for AllocatedThinBoxSlice<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        <[T] as PartialEq>::eq(&**self, &**other)
    }
}

impl<T: Eq> Eq for AllocatedThinBoxSlice<T> {}

impl<T: PartialOrd> PartialOrd for AllocatedThinBoxSlice<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        <[T] as PartialOrd>::partial_cmp(&**self, &**other)
    }
}

impl<T: Hash> Hash for AllocatedThinBoxSlice<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        <[T] as Hash>::hash(&**self, state)
    }
}

impl<T> FromIterator<T> for AllocatedThinBoxSlice<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let iter = iter.into_iter();
        let (lower, upper) = iter.size_hint();
        if Some(lower) == upper {
            let mut thin = AllocatedThinBoxSlice::<T>::new_uninit(lower);
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

impl<T: Allocative> Allocative for AllocatedThinBoxSlice<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        {
            let ptr_key = allocative::Key::new("ptr");
            if self.len() == 0 {
                // Statically allocated data, so just report the pointer itself
                visitor.visit_simple(ptr_key, mem::size_of_val(&self.ptr));
            } else {
                let mut visitor =
                    visitor.enter_unique(allocative::Key::new("ptr"), mem::size_of_val(&self.ptr));
                {
                    let mut visitor = visitor.enter(
                        allocative::Key::new("alloc"),
                        Self::layout_for_len(self.len()).size(),
                    );
                    visitor.visit_simple(allocative::Key::new("len"), mem::size_of::<usize>());
                    {
                        let mut visitor = visitor
                            .enter(allocative::Key::new("data"), mem::size_of_val::<[_]>(self));
                        visitor.visit_slice::<T>(self);
                        visitor.exit();
                    }
                    visitor.exit();
                }
                visitor.exit();
            }
        }
        visitor.exit();
    }
}

#[cfg(test)]
mod tests {
    use super::AllocatedThinBoxSlice;

    #[test]
    fn test_empty() {
        let thin = AllocatedThinBoxSlice::<String>::empty();
        assert_eq!(0, thin.len());
        thin.run_drop();
    }

    #[test]
    fn test_from_iter_sized() {
        let thin =
            AllocatedThinBoxSlice::from_iter(["a".to_owned(), "bb".to_owned(), "ccc".to_owned()]);
        assert_eq!(["a".to_owned(), "bb".to_owned(), "ccc".to_owned()], *thin);
        thin.run_drop();
    }

    #[test]
    fn test_from_iter_unknown_size() {
        let thin = AllocatedThinBoxSlice::from_iter(
            ["a".to_owned(), "b".to_owned(), "c".to_owned()]
                .into_iter()
                .filter(|_| true),
        );
        assert_eq!(["a".to_owned(), "b".to_owned(), "c".to_owned()], *thin);
        thin.run_drop();
    }

    /// If there are obvious memory violations, this test will catch them.
    #[test]
    fn test_stress() {
        for i in 0..1000 {
            let thin = AllocatedThinBoxSlice::from_iter((0..i).map(|j| j.to_string()));
            assert_eq!(i, thin.len());
            assert_eq!((0..i).map(|j| j.to_string()).collect::<Vec<_>>(), *thin);
            thin.run_drop();
        }
    }
}
