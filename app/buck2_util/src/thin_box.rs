/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

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
    fn offset_of_data() -> usize {
        mem::offset_of!(ThinBoxSliceLayout::<T>, data)
    }
}

/// `Box<[T]>` but thin pointer.
///
/// The lower N bits of the `ThinBoxSlice.ptr` value are used to embed a length
/// for short slices. N is determined by the `std::mem::align_of::<T>()`. These
/// are encoded as:
///
/// * `ptr == 0` (lower N bits == 0), then len = 0
///
/// * `ptr != 0`, lower N bits == 0, this is a "long" ThinBoxSlice, and ptr
///   points at the data member of a `ThinBoxSliceLayout<T>`. `len` can be read
///   from the pointed to layout. Note that this _could_ be used to represent a
///   zero-length `ThinBoxSlice``, but it's redundant with the null pointer
///   check in the case above.
///
/// * `ptr != 0`, lower N bits encode the length, and layout is a
///   `ThinBoxSliceLayout_short`` (data only, no len)
///
/// An alternative representation could have the`len=0` case be another instance
/// of a "long" ThinBoxSlice, but the pointed-to layout would be a static/const
/// allocation. Others are free to adopt this approach if they can show a
/// runtime improvement. At the time of this writing, the frequent calls to
/// `read_len` on every iteration loop along with the prevalence of zero-length
/// objects cause this to be slower than the current representation.
//
// We don't really need `'static` here, but we hit type checker limitations.
pub struct ThinBoxSlice<T: 'static> {
    /// Pointer to the first element, `ThinBoxSliceLayout.data`.
    ptr: usize,
    phantom: PhantomData<T>,
}

unsafe impl<T: Sync> Sync for ThinBoxSlice<T> {}
unsafe impl<T: Send> Send for ThinBoxSlice<T> {}

impl<T: 'static> ThinBoxSlice<T> {
    #[inline]
    pub const fn empty() -> ThinBoxSlice<T> {
        ThinBoxSlice {
            ptr: 0,
            phantom: PhantomData,
        }
    }

    /// Allocation layout for a slice of length `len`.
    /// Returns a tuple:
    /// * `[0]: bool``: "is_short_layout"
    /// * `[1]: Layout``: the required memory layout
    #[inline]
    fn layout_for_len(len: usize) -> (bool, Layout) {
        if len < std::mem::align_of::<T>() {
            (true, Layout::array::<T>(len).unwrap())
        } else {
            let (layout, _offset_of_data) = Layout::new::<ThinBoxSliceLayout<T>>()
                .extend(Layout::array::<T>(len).unwrap())
                .unwrap();
            (false, layout)
        }
    }

    fn get_tag_bit_mask(&self) -> usize {
        let align: usize = std::mem::align_of::<T>();
        assert!(align.is_power_of_two());
        align - 1
    }

    #[inline]
    fn get_tag_bits(&self) -> usize {
        self.ptr & self.get_tag_bit_mask()
    }

    #[inline]
    fn as_ptr(&self) -> *mut T {
        let ptr = self.ptr & !self.get_tag_bit_mask();
        ptr as *mut T
    }

    #[inline]
    fn as_nonnull_ptr(&self) -> *mut T {
        if self.ptr == 0 {
            NonNull::<T>::dangling().as_ptr()
        } else {
            self.as_ptr()
        }
    }

    /// Length of the slice.
    // Not called `len` to avoid overload with `Deref::len`.
    #[inline]
    fn read_len(&self) -> usize {
        if self.ptr == 0 {
            0
        } else {
            let short_len = self.get_tag_bits();
            if short_len != 0 {
                short_len
            } else {
                unsafe {
                    (*self
                        .as_ptr()
                        .cast::<u8>()
                        .sub(ThinBoxSliceLayout::<T>::offset_of_data())
                        .cast::<ThinBoxSliceLayout<T>>())
                    .len
                }
            }
        }
    }
    /// Allocate uninitialized memory for a slice of length `len`.
    #[inline]
    pub fn new_uninit(len: usize) -> ThinBoxSlice<MaybeUninit<T>> {
        if len == 0 {
            ThinBoxSlice::empty()
        } else {
            let (is_short, layout) = Self::layout_for_len(len);
            unsafe {
                let alloc = alloc::alloc(layout);
                if alloc.is_null() {
                    alloc::handle_alloc_error(layout);
                }
                if is_short {
                    ThinBoxSlice {
                        // Embed the length in the lower bits of ptr
                        ptr: (alloc as usize) | len,
                        phantom: PhantomData,
                    }
                } else {
                    let alloc = alloc.cast::<ThinBoxSliceLayout<MaybeUninit<T>>>();
                    (*alloc).len = len;
                    let data_ptr = alloc.byte_add(ThinBoxSliceLayout::<T>::offset_of_data());
                    ThinBoxSlice {
                        ptr: data_ptr as usize,
                        phantom: PhantomData,
                    }
                }
            }
        }
    }

    #[inline]
    pub fn new<const N: usize>(data: [T; N]) -> ThinBoxSlice<T> {
        ThinBoxSlice::from_iter(data)
    }
}

impl<T: 'static> Deref for ThinBoxSlice<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { slice::from_raw_parts(self.as_nonnull_ptr(), self.read_len()) }
    }
}

impl<T: 'static> DerefMut for ThinBoxSlice<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { slice::from_raw_parts_mut(self.as_nonnull_ptr(), self.read_len()) }
    }
}

impl<T> ThinBoxSlice<MaybeUninit<T>> {
    /// Converts the `ThinBoxSlice<MaybeUninit<T>>` to `ThinBoxSlice<T>`.
    ///
    /// # Safety
    ///
    /// The caller must ensure that all elements in the slice have been properly initialized
    /// before calling this method. Reading uninitialized memory is undefined behavior.
    #[inline]
    pub unsafe fn assume_init(self) -> ThinBoxSlice<T> {
        let result = ThinBoxSlice {
            ptr: self.ptr,
            phantom: PhantomData,
        };
        mem::forget(self);
        result
    }
}

impl<T: 'static> Drop for ThinBoxSlice<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            let len = self.read_len();
            if len != 0 {
                let slice = self.deref_mut();
                ptr::drop_in_place(slice);
                let mut alloc = self.as_ptr().cast::<u8>();
                let (is_short, layout) = Self::layout_for_len(len);
                if !is_short {
                    alloc = alloc.byte_sub(ThinBoxSliceLayout::<T>::offset_of_data());
                }
                alloc::dealloc(alloc, layout);
            }
        }
    }
}

impl<T: 'static> Default for ThinBoxSlice<T> {
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
            let ptr_key = allocative::Key::new("ptr");
            if self.is_empty() {
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
