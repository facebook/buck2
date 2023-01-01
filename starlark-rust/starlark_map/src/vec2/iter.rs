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
    pub(crate) values: NonNull<V>,
    pub(crate) _marker: PhantomData<slice::Iter<'a, V>>,
}

unsafe impl<K: Sync, V: Sync> Sync for Iter<'_, K, V> {}
unsafe impl<K: Sync, V: Sync> Send for Iter<'_, K, V> {}

impl<'a, K, V> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let key = self.keys.next()?;
        let value = unsafe { &*self.values.as_ptr() };
        self.values = unsafe { NonNull::new_unchecked(self.values.as_ptr().add(1)) };
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
        let value = unsafe { &*self.values.as_ptr().add(self.keys.len()) };
        Some((key, value))
    }
}

pub(crate) struct IntoIter<K, V> {
    /// Pointer to the next key. Updated as we iterate.
    pub(crate) keys_begin: NonNull<K>,
    /// Pointer to the next value. Updated as we iterate.
    pub(crate) values_begin: NonNull<V>,
    /// Pointer to the end of the values. Updated as we iterate.
    pub(crate) values_end: NonNull<V>,
    /// The layout of `Vec2` is `[padding, keys, values]`.
    /// This field is a pointer to the values. Used for `Drop`.
    pub(crate) values_ptr: NonNull<V>,
    /// `Vec2` capacity. Used for `Drop`.
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
                let k = ptr::read(self.keys_begin.as_ref());
                let v = ptr::read(self.values_begin.as_ref());
                self.keys_begin = NonNull::new_unchecked(self.keys_begin.as_ptr().add(1));
                self.values_begin = NonNull::new_unchecked(self.values_begin.as_ptr().add(1));
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
            ptr::drop_in_place(slice::from_raw_parts_mut(self.keys_begin.as_ptr(), rem));
            ptr::drop_in_place(slice::from_raw_parts_mut(self.values_begin.as_ptr(), rem));
            Vec2::<K, V>::dealloc_impl(self.values_ptr, self.cap);
        }
    }
}

impl<K, V> ExactSizeIterator for IntoIter<K, V> {
    #[inline]
    fn len(&self) -> usize {
        unsafe {
            self.values_end
                .as_ptr()
                .offset_from(self.values_begin.as_ptr()) as usize
        }
    }
}

impl<K, V> DoubleEndedIterator for IntoIter<K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.values_begin == self.values_end {
            None
        } else {
            unsafe {
                self.values_end = NonNull::new_unchecked(self.values_end.as_ptr().sub(1));
                let new_len = self.len();

                debug_assert!(ptr::eq(
                    self.values_begin.as_ptr().add(new_len),
                    self.values_end.as_ptr()
                ));

                let k = ptr::read(self.keys_begin.as_ptr().add(new_len));
                let v = ptr::read(self.values_end.as_ptr());

                Some((k, v))
            }
        }
    }
}
