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

use std::marker::PhantomData;
use std::ptr;
use std::ptr::NonNull;
use std::slice;

use gazebo::prelude::*;

use crate::vec2::Vec2;

#[derive(Clone_)]
pub(crate) struct Iter<'a, K, V> {
    pub(crate) keys: slice::Iter<'a, K>,
    pub(crate) values: *const V,
    pub(crate) _marker: PhantomData<slice::Iter<'a, V>>,
}

unsafe impl<K: Sync, V: Sync> Sync for Iter<'_, K, V> {}
unsafe impl<K: Sync, V: Sync> Send for Iter<'_, K, V> {}

impl<'a, K, V> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let key = self.keys.next()?;
        let value = unsafe { &*self.values };
        self.values = unsafe { self.values.add(1) };
        Some((key, value))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.keys.size_hint()
    }
}

impl<'a, K, V> ExactSizeIterator for Iter<'a, K, V> {
    #[inline]
    fn len(&self) -> usize {
        self.keys.len()
    }
}

impl<'a, K, V> DoubleEndedIterator for Iter<'a, K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        let key = self.keys.next_back()?;
        let value = unsafe { &*self.values.add(self.keys.len()) };
        Some((key, value))
    }
}

pub(crate) struct IntoIter<K, V> {
    // TODO: make NonNull
    pub(crate) keys_begin: *mut K,
    pub(crate) values_begin: *mut V,
    pub(crate) values_end: *mut V,
    pub(crate) values_ptr: NonNull<V>,
    pub(crate) cap: usize,
}

unsafe impl<K: Send, V: Send> Send for IntoIter<K, V> {}
unsafe impl<K: Sync, V: Sync> Sync for IntoIter<K, V> {}

impl<K, V> Iterator for IntoIter<K, V> {
    type Item = (K, V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.values_begin == self.values_end {
            None
        } else {
            unsafe {
                let k = ptr::read(self.keys_begin);
                let v = ptr::read(self.values_begin);
                self.keys_begin = self.keys_begin.add(1);
                self.values_begin = self.values_begin.add(1);
                Some((k, v))
            }
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let rem = self.len();
        (rem, Some(rem))
    }
}

impl<K, V> Drop for IntoIter<K, V> {
    fn drop(&mut self) {
        unsafe {
            let rem = self.len();
            ptr::drop_in_place(slice::from_raw_parts_mut(self.keys_begin, rem));
            ptr::drop_in_place(slice::from_raw_parts_mut(self.values_begin, rem));
            Vec2::<K, V>::dealloc_impl(self.values_ptr, self.cap);
        }
    }
}

impl<K, V> ExactSizeIterator for IntoIter<K, V> {
    #[inline]
    fn len(&self) -> usize {
        unsafe { self.values_end.offset_from(self.values_begin) as usize }
    }
}

impl<K, V> DoubleEndedIterator for IntoIter<K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.values_begin == self.values_end {
            None
        } else {
            unsafe {
                self.values_end = self.values_end.sub(1);
                let k = ptr::read(self.keys_begin.add(self.len()));
                let v = ptr::read(self.values_end);
                Some((k, v))
            }
        }
    }
}
