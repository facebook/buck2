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
use std::marker::PhantomData;
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
    const fn offset_of_data() -> isize {
        // SAFETY: rust guarantees no allocated object can be larger than isize::MAX bytes.
        mem::offset_of!(ThinBoxSliceLayout::<T>, data) as isize
    }
}

/// `Box<[T]>` but thin pointer to FrozenValue(s)
///
/// Similar to `ThinBoxSlice`, but it ignores the lowest bit, allowing
/// PackedImpl to use that to store a single FrozenValue in place of this
/// object. Like `ThinBoxSlice`, the remaining unused pointer bits are used to
/// store an embedded length. If these bits are zero, the `ptr` points to the
/// `.data` of a ThinBoxSliceLayout, which stores the `.len`. Otherwise, ptr
/// points at the T[].
///
///
/// The current implementation returns what amounts to a null pointer for an
/// empty list. An alternative would be to return a valid pointer to a
/// statically allocated "long"-lengthed object with a length of 0. This would
/// reduce the number of representations, but testing at the time of this
/// writing shows that empty lists are common, and the pointer dereference in
/// reading the length causes a small performance hit. Changes in the future may
/// make this the preferred implementation.
//
// We don't really need `'static` here, but we hit type checker limitations.
pub(super) struct AllocatedThinBoxSlice<T: 'static> {
    /// Pointer to the first element, `ThinBoxSliceLayout.data`.
    ptr: usize,
    phantom: PhantomData<T>,
}

unsafe impl<T: Sync> Sync for AllocatedThinBoxSlice<T> {}
unsafe impl<T: Send> Send for AllocatedThinBoxSlice<T> {}

impl<T: 'static> AllocatedThinBoxSlice<T> {
    #[inline]
    pub(super) const fn empty() -> AllocatedThinBoxSlice<T> {
        AllocatedThinBoxSlice {
            ptr: 0,
            phantom: PhantomData,
        }
    }

    const fn get_reserved_tag_bit_count() -> usize {
        // The lower bit is reserved for use by PackedImpl.
        1
    }

    const fn get_unshifted_tag_bit_mask() -> usize {
        let align: usize = std::mem::align_of::<T>();
        assert!(align.is_power_of_two());
        align - 1
    }

    const fn get_tag_bit_mask() -> usize {
        let mask = Self::get_unshifted_tag_bit_mask()
            >> AllocatedThinBoxSlice::<T>::get_reserved_tag_bit_count();
        assert!(mask != 0);
        mask
    }

    const fn get_max_short_len() -> usize {
        Self::get_tag_bit_mask() + 1
    }

    /// Allocation layout for a slice of length `len`.
    #[inline]
    fn layout_for_len(len: usize) -> (bool, Layout) {
        if len != 0 && len != 1 && len <= Self::get_max_short_len() {
            (true, Layout::array::<T>(len).unwrap())
        } else {
            let (layout, _offset_of_data) = Layout::new::<ThinBoxSliceLayout<T>>()
                .extend(Layout::array::<T>(len).unwrap())
                .unwrap();
            (false, layout)
        }
    }

    #[inline]
    fn get_tag_bits(&self) -> usize {
        (self.ptr & Self::get_unshifted_tag_bit_mask()) >> Self::get_reserved_tag_bit_count()
    }

    #[inline]
    fn as_ptr(&self) -> *mut T {
        (self.ptr & !Self::get_unshifted_tag_bit_mask()) as *mut T
    }

    #[inline]
    fn as_nonnull_ptr(&self) -> *mut T {
        let ptr = self.as_ptr();
        if ptr.is_null() {
            NonNull::<T>::dangling().as_ptr()
        } else {
            ptr
        }
    }

    /// Length of the slice.
    // Not called `len` to avoid overload with `Deref::len`.
    #[inline]
    fn read_len(&self) -> usize {
        if self.as_ptr().is_null() {
            return 0;
        }

        let bits = self.get_tag_bits();
        if bits != 0 {
            bits + 1
        } else {
            unsafe {
                (*self
                    .as_ptr()
                    .byte_offset(-ThinBoxSliceLayout::<T>::offset_of_data())
                    .cast::<ThinBoxSliceLayout<T>>())
                .len
            }
        }
    }

    /// Allocate uninitialized memory for a slice of length `len`.
    #[inline]
    pub(super) fn new_uninit(len: usize) -> AllocatedThinBoxSlice<MaybeUninit<T>> {
        if len == 0 {
            AllocatedThinBoxSlice::empty()
        } else {
            let (is_short, layout) = Self::layout_for_len(len);
            unsafe {
                let alloc = alloc::alloc(layout);
                if alloc.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                if is_short {
                    assert!((alloc as usize) & Self::get_unshifted_tag_bit_mask() == 0);
                    AllocatedThinBoxSlice {
                        // Embed the length in the lower bits of ptr
                        ptr: (alloc as usize) | ((len - 1) << Self::get_reserved_tag_bit_count()),
                        phantom: PhantomData,
                    }
                } else {
                    let alloc = alloc as *mut ThinBoxSliceLayout<T>;
                    (*alloc).len = len;
                    let data_ptr = alloc.byte_offset(ThinBoxSliceLayout::<T>::offset_of_data());
                    AllocatedThinBoxSlice {
                        ptr: data_ptr as usize,
                        phantom: PhantomData,
                    }
                }
            }
        }
    }

    pub const unsafe fn into_inner(self) -> usize {
        self.ptr
    }

    pub unsafe fn from_inner(ptr: usize) -> Self {
        Self {
            ptr,
            phantom: PhantomData,
        }
    }
}

impl<T: 'static> Deref for AllocatedThinBoxSlice<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { slice::from_raw_parts(self.as_nonnull_ptr(), self.read_len()) }
    }
}

impl<T: 'static> DerefMut for AllocatedThinBoxSlice<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { slice::from_raw_parts_mut(self.as_nonnull_ptr(), self.read_len()) }
    }
}

impl<T> AllocatedThinBoxSlice<MaybeUninit<T>> {
    #[inline]
    unsafe fn assume_init(self) -> AllocatedThinBoxSlice<T> {
        AllocatedThinBoxSlice {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}

impl<T: 'static> AllocatedThinBoxSlice<T> {
    #[inline]
    pub(super) fn run_drop(self) {
        unsafe {
            let len = self.read_len();
            if len != 0 {
                let slice = ptr::slice_from_raw_parts_mut(self.as_nonnull_ptr(), len);
                ptr::drop_in_place(slice);
                let mut alloc = self.as_ptr().cast::<u8>();
                let (is_short, layout) = Self::layout_for_len(len);
                if !is_short {
                    alloc = alloc.byte_offset(-ThinBoxSliceLayout::<T>::offset_of_data());
                }
                alloc::dealloc(alloc, layout);
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
                    let (is_short, layout) = Self::layout_for_len(self.len());
                    let mut visitor = visitor.enter(allocative::Key::new("alloc"), layout.size());

                    if !is_short {
                        visitor.visit_simple(allocative::Key::new("len"), mem::size_of::<usize>());
                    }
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
