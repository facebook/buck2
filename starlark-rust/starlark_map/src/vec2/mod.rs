/*
 * Copyright 2019 The Starlark in Rust Authors.
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

use std::alloc;
use std::alloc::Layout;
use std::cmp;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::mem;
use std::mem::MaybeUninit;
use std::ptr;
use std::ptr::NonNull;
use std::slice;

use allocative::Allocative;
use allocative::Visitor;

pub(crate) mod iter;

#[derive(Eq, PartialEq, Debug)]
struct Vec2Layout<K, V> {
    layout: Layout,
    offset_of_values: usize,
    _marker: PhantomData<*mut (K, V)>,
}

impl<K, V> Vec2Layout<K, V> {
    fn new(cap: usize) -> Vec2Layout<K, V> {
        debug_assert!(cap != 0);
        let k = Layout::array::<K>(cap).unwrap();
        let v = Layout::array::<V>(cap).unwrap();
        let (layout, offset_of_values) = k.extend(v).unwrap();

        debug_assert!(offset_of_values <= layout.size());
        debug_assert!(layout.align() >= k.align());
        debug_assert!(layout.align() >= v.align());
        debug_assert!(offset_of_values % k.align() == 0);

        Vec2Layout {
            layout,
            offset_of_values,
            _marker: PhantomData,
        }
    }

    unsafe fn alloc(&self) -> NonNull<V> {
        let ptr: *mut u8 = alloc::alloc(self.layout);
        let values_ptr: *mut V = ptr.add(self.offset_of_values).cast();
        NonNull::new_unchecked(values_ptr)
    }

    unsafe fn dealloc(&self, values_ptr: NonNull<V>) {
        let ptr: *mut u8 = values_ptr.as_ptr().cast::<u8>().sub(self.offset_of_values);
        alloc::dealloc(ptr, self.layout)
    }
}

/// Array of pairs (K, V), where K and V are stored separately.
/// This reduces memory consumption when K and V have different alignments.
pub(crate) struct Vec2<K, V> {
    // Layout is `[padding, K, K, ..., K, V, V, ..., V]`
    values_ptr: NonNull<V>,
    len: usize,
    cap: usize,
    _marker: PhantomData<(K, V)>,
}

unsafe impl<K: Send, V: Send> Send for Vec2<K, V> {}
unsafe impl<K: Sync, V: Sync> Sync for Vec2<K, V> {}

impl<K, V> Default for Vec2<K, V> {
    #[inline]
    fn default() -> Vec2<K, V> {
        Vec2::new()
    }
}

impl<K: Debug, V: Debug> Debug for Vec2<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<K: Clone, V: Clone> Clone for Vec2<K, V> {
    fn clone(&self) -> Vec2<K, V> {
        let mut r = Vec2::with_capacity(self.len());
        for (k, v) in self.iter() {
            r.push(k.clone(), v.clone());
        }
        r
    }
}

impl<K, V> Vec2<K, V> {
    #[inline]
    pub(crate) const fn new() -> Vec2<K, V> {
        Vec2 {
            values_ptr: NonNull::dangling(),
            len: 0,
            cap: 0,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub(crate) fn with_capacity(cap: usize) -> Vec2<K, V> {
        if cap == 0 {
            Vec2::new()
        } else {
            let values_ptr = unsafe { Vec2Layout::<K, V>::new(cap).alloc() };
            Vec2 {
                values_ptr,
                len: 0,
                cap,
                _marker: PhantomData,
            }
        }
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub(crate) fn capacity(&self) -> usize {
        self.cap
    }

    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    fn keys_ptr(&self) -> *mut K {
        unsafe { self.values_ptr.cast::<K>().as_ptr().sub(self.cap) }
    }

    #[inline]
    fn values_ptr(&self) -> *mut V {
        self.values_ptr.as_ptr()
    }

    #[inline]
    pub(crate) fn keys(&self) -> &[K] {
        unsafe { slice::from_raw_parts(self.keys_ptr(), self.len) }
    }

    #[inline]
    pub(crate) fn keys_mut(&mut self) -> &mut [K] {
        unsafe { slice::from_raw_parts_mut(self.keys_ptr(), self.len) }
    }

    #[inline]
    fn keys_uninit(&mut self) -> &mut [MaybeUninit<K>] {
        unsafe { slice::from_raw_parts_mut(self.keys_ptr() as *mut _, self.cap) }
    }

    #[inline]
    pub(crate) fn values(&self) -> &[V] {
        unsafe { slice::from_raw_parts(self.values_ptr(), self.len) }
    }

    #[inline]
    fn values_mut(&mut self) -> &mut [V] {
        unsafe { slice::from_raw_parts_mut(self.values_ptr(), self.len) }
    }

    #[inline]
    fn values_uninit(&mut self) -> &mut [MaybeUninit<V>] {
        unsafe { slice::from_raw_parts_mut(self.values_ptr() as *mut _, self.cap) }
    }

    // This is what `Vec` does.
    const MIN_NON_ZERO_CAP: usize = if mem::size_of::<(K, V)>() == 1 {
        8
    } else if mem::size_of::<(K, V)>() <= 1024 {
        4
    } else {
        1
    };

    #[allow(clippy::mem_forget)]
    #[cold]
    fn reserve_slow(&mut self, additional: usize) {
        debug_assert!(self.cap - self.len < additional);

        let required_cap = self.len.checked_add(additional).expect("capacity overflow");
        let new_cap = cmp::max(required_cap, Self::MIN_NON_ZERO_CAP);
        let new_cap = cmp::max(new_cap, self.cap * 2);
        let new = Self::with_capacity(new_cap);
        unsafe {
            ptr::copy_nonoverlapping(self.keys_ptr(), new.keys_ptr(), self.len);
            ptr::copy_nonoverlapping(self.values_ptr(), new.values_ptr(), self.len);
            self.dealloc();
            self.values_ptr = new.values_ptr;
            mem::forget(new);
            self.cap = new_cap;
        }
    }

    #[inline]
    pub(crate) fn reserve(&mut self, additional: usize) {
        if self.cap - self.len < additional {
            self.reserve_slow(additional);
        }
    }

    #[inline]
    unsafe fn dealloc_impl(data: NonNull<V>, cap: usize) {
        if cap != 0 {
            Vec2Layout::<K, V>::new(cap).dealloc(data);
        }
    }

    /// Deallocate, but do not call destructors.
    #[inline]
    unsafe fn dealloc(&mut self) {
        Self::dealloc_impl(self.values_ptr, self.cap);
    }

    unsafe fn drop_in_place(&mut self) {
        ptr::drop_in_place::<[K]>(self.keys_mut());
        ptr::drop_in_place::<[V]>(self.values_mut());
    }

    #[inline]
    pub(crate) fn push(&mut self, key: K, value: V) {
        self.reserve(1);
        let len = self.len;
        unsafe {
            self.keys_uninit().get_unchecked_mut(len).write(key);
            self.values_uninit().get_unchecked_mut(len).write(value);
        }
        self.len += 1;
    }

    #[inline]
    pub(crate) fn get(&self, index: usize) -> Option<(&K, &V)> {
        if index < self.len {
            unsafe {
                let k = self.keys().get_unchecked(index);
                let v = self.values().get_unchecked(index);
                Some((k, v))
            }
        } else {
            None
        }
    }

    #[inline]
    pub(crate) unsafe fn get_unchecked(&self, index: usize) -> (&K, &V) {
        debug_assert!(index < self.len);
        (
            self.keys().get_unchecked(index),
            self.values().get_unchecked(index),
        )
    }

    #[inline]
    pub(crate) unsafe fn get_unchecked_mut(&mut self, index: usize) -> (&mut K, &mut V) {
        debug_assert!(index < self.len);
        let k_ptr = self.keys_ptr();
        let v_ptr = self.values_ptr();
        (&mut *k_ptr.add(index), &mut *v_ptr.add(index))
    }

    #[inline]
    unsafe fn read(&self, index: usize) -> (K, V) {
        debug_assert!(index < self.len);
        let (k, v) = self.get_unchecked(index);
        (ptr::read(k), ptr::read(v))
    }

    pub(crate) fn remove(&mut self, index: usize) -> (K, V) {
        assert!(index < self.len);
        unsafe {
            let (k, v) = self.read(index);
            ptr::copy(
                self.keys_ptr().add(index + 1),
                self.keys_ptr().add(index),
                self.len - index - 1,
            );
            ptr::copy(
                self.values_ptr().add(index + 1),
                self.values_ptr().add(index),
                self.len - index - 1,
            );
            self.len -= 1;
            (k, v)
        }
    }

    #[inline]
    pub(crate) fn clear(&mut self) {
        unsafe {
            self.drop_in_place();
            self.len = 0;
        }
    }

    #[inline]
    pub(crate) fn pop(&mut self) -> Option<(K, V)> {
        let new_len = self.len.checked_sub(1)?;
        let (k, v) = unsafe { self.read(new_len) };
        self.len = new_len;
        Some((k, v))
    }

    #[inline]
    pub(crate) fn iter(&self) -> iter::Iter<'_, K, V> {
        iter::Iter {
            keys: self.keys().iter(),
            values: self.values_ptr(),
            _marker: PhantomData,
        }
    }

    #[allow(clippy::mem_forget)]
    #[inline]
    pub(crate) fn into_iter(self) -> iter::IntoIter<K, V> {
        let iter = iter::IntoIter {
            keys_begin: self.keys_ptr(),
            values_begin: self.values_ptr(),
            values_end: unsafe { self.values_ptr().add(self.len) },
            values_ptr: self.values_ptr,
            cap: self.cap,
        };
        mem::forget(self);
        iter
    }

    pub(crate) fn sort_by<F>(&mut self, mut compare: F)
    where
        F: FnMut((&K, &V), (&K, &V)) -> Ordering,
    {
        // TODO: sort without allocation.
        // TODO: drain.
        let mut entries: Vec<(K, V)> = mem::take(self).into_iter().collect();
        entries.sort_by(|(ak, av), (bk, bv)| compare((ak, av), (bk, bv)));
        for (k, v) in entries {
            self.push(k, v);
        }
    }
}

impl<K, V> Drop for Vec2<K, V> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            if self.cap != 0 {
                self.drop_in_place();
                self.dealloc();
            }
        }
    }
}

impl<'a, K, V> IntoIterator for &'a Vec2<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = iter::Iter<'a, K, V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<K: Allocative, V: Allocative> Allocative for Vec2<K, V> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        if self.cap != 0 {
            let mut visitor =
                visitor.enter_unique(allocative::Key::new("ptr"), mem::size_of::<*const ()>());
            {
                let mut visitor = visitor.enter(
                    allocative::Key::new("data"),
                    Vec2Layout::<K, V>::new(self.cap).layout.size(),
                );
                for (k, v) in self {
                    k.visit(&mut visitor);
                    v.visit(&mut visitor);
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
    use std::alloc::Layout;
    use std::marker::PhantomData;

    use crate::vec2::Vec2;
    use crate::vec2::Vec2Layout;

    #[test]
    fn test_layout_for() {
        assert_eq!(
            Vec2Layout {
                offset_of_values: 4,
                layout: Layout::from_size_align(8, 4).unwrap(),
                _marker: PhantomData,
            },
            Vec2Layout::<[u8; 3], u32>::new(1)
        );
    }

    #[test]
    fn test_alloc_dealloc() {
        unsafe {
            let layout = Vec2Layout::<[u8; 3], u32>::new(100);
            let data = layout.alloc();
            layout.dealloc(data);
        }
    }

    #[test]
    fn test_push() {
        let mut v = Vec2::new();
        v.push(1, 2);
        assert_eq!(1, v.len());
        assert_eq!(Some((&1, &2)), v.get(0));
    }

    #[test]
    fn test_push_many() {
        let mut v = Vec2::new();
        for i in 0..100 {
            v.push(i.to_string(), i * 2);
        }
        assert_eq!(100, v.len());
        for i in 0..100 {
            assert_eq!(Some((&i.to_string(), &(i * 2))), v.get(i));
        }
    }

    #[test]
    fn test_into_iter() {
        let mut v = Vec2::new();
        for i in 0..100 {
            v.push(i.to_string(), i * 2);
        }
        for (i, (k, v)) in v.into_iter().enumerate() {
            assert_eq!(i.to_string(), k);
            assert_eq!(i * 2, v);
        }
    }
}
