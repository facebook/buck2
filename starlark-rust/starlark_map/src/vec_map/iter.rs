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

use gazebo::prelude::Clone_;

use crate::iter::def_double_ended_iter;
use crate::iter::def_iter;
use crate::vec_map::Bucket;
use crate::Hashed;

#[derive(Clone_)]
pub(crate) struct VMKeys<'a, K: 'a, V: 'a> {
    pub(crate) iter: std::slice::Iter<'a, Bucket<K, V>>,
}

impl<'a, K: 'a, V: 'a> VMKeys<'a, K, V> {
    fn map(b: &'a Bucket<K, V>) -> <Self as Iterator>::Item {
        &b.key
    }
}

impl<'a, K: 'a, V: 'a> Iterator for VMKeys<'a, K, V> {
    type Item = &'a K;

    def_iter!();
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for VMKeys<'a, K, V> {
    def_double_ended_iter!();
}

impl<'a, K: 'a, V: 'a> ExactSizeIterator for VMKeys<'a, K, V> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

#[derive(Clone_)]
pub(crate) struct VMValues<'a, K: 'a, V: 'a> {
    pub(crate) iter: std::slice::Iter<'a, Bucket<K, V>>,
}

impl<'a, K: 'a, V: 'a> VMValues<'a, K, V> {
    fn map(b: &'a Bucket<K, V>) -> <Self as Iterator>::Item {
        &b.value
    }
}

impl<'a, K: 'a, V: 'a> Iterator for VMValues<'a, K, V> {
    type Item = &'a V;

    def_iter!();
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for VMValues<'a, K, V> {
    def_double_ended_iter!();
}

impl<'a, K: 'a, V: 'a> ExactSizeIterator for VMValues<'a, K, V> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

pub(crate) struct VMValuesMut<'a, K: 'a, V: 'a> {
    pub(crate) iter: std::slice::IterMut<'a, Bucket<K, V>>,
}

impl<'a, K: 'a, V: 'a> VMValuesMut<'a, K, V> {
    fn map(b: &'a mut Bucket<K, V>) -> <Self as Iterator>::Item {
        &mut b.value
    }
}

impl<'a, K: 'a, V: 'a> Iterator for VMValuesMut<'a, K, V> {
    type Item = &'a mut V;

    def_iter!();
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for VMValuesMut<'a, K, V> {
    def_double_ended_iter!();
}

impl<'a, K: 'a, V: 'a> ExactSizeIterator for VMValuesMut<'a, K, V> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

#[derive(Clone_)]
pub struct Iter<'a, K: 'a, V: 'a> {
    pub(crate) iter: std::slice::Iter<'a, Bucket<K, V>>,
}

impl<'a, K: 'a, V: 'a> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    def_iter!();
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Iter<'a, K, V> {
    def_double_ended_iter!();
}

impl<'a, K: 'a, V: 'a> ExactSizeIterator for Iter<'a, K, V> {}

impl<'a, K: 'a, V: 'a> Iter<'a, K, V> {
    #[inline]
    fn map(b: &Bucket<K, V>) -> (&K, &V) {
        (&b.key, &b.value)
    }
}

pub(crate) struct VMIterHash<'a, K: 'a, V: 'a> {
    pub(crate) iter: std::slice::Iter<'a, Bucket<K, V>>,
}

impl<'a, K: 'a, V: 'a> VMIterHash<'a, K, V> {
    #[inline]
    fn map(b: &'a Bucket<K, V>) -> (Hashed<&'a K>, &'a V) {
        (Hashed::new_unchecked(b.hash, &b.key), &b.value)
    }
}

impl<'a, K: 'a, V: 'a> Iterator for VMIterHash<'a, K, V> {
    type Item = (Hashed<&'a K>, &'a V);

    def_iter!();
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for VMIterHash<'a, K, V> {
    def_double_ended_iter!();
}

impl<'a, K: 'a, V: 'a> ExactSizeIterator for VMIterHash<'a, K, V> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

pub struct IterMut<'a, K: 'a, V: 'a> {
    pub(crate) iter: std::slice::IterMut<'a, Bucket<K, V>>,
}

impl<'a, K: 'a, V: 'a> IterMut<'a, K, V> {
    #[inline]
    fn map(b: &mut Bucket<K, V>) -> (&K, &mut V) {
        (&b.key, &mut b.value)
    }
}

impl<'a, K: 'a, V: 'a> Iterator for IterMut<'a, K, V> {
    type Item = (&'a K, &'a mut V);

    def_iter!();
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for IterMut<'a, K, V> {
    def_double_ended_iter!();
}

impl<'a, K: 'a, V: 'a> ExactSizeIterator for IterMut<'a, K, V> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

pub(crate) struct VMIntoIterHash<K, V> {
    pub(crate) iter: std::vec::IntoIter<Bucket<K, V>>,
}

impl<K, V> Iterator for VMIntoIterHash<K, V> {
    type Item = (Hashed<K>, V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|b| (Hashed::new_unchecked(b.hash, b.key), b.value))
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.iter
            .nth(n)
            .map(|b| (Hashed::new_unchecked(b.hash, b.key), b.value))
    }

    fn last(mut self) -> Option<Self::Item> {
        // Since these are all double-ended iterators we can skip to the end quickly
        self.iter
            .next_back()
            .map(|b| (Hashed::new_unchecked(b.hash, b.key), b.value))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }

    fn count(self) -> usize {
        self.iter.len()
    }

    fn collect<C>(self) -> C
    where
        C: std::iter::FromIterator<Self::Item>,
    {
        self.iter
            .map(|b| (Hashed::new_unchecked(b.hash, b.key), b.value))
            .collect()
    }
}

impl<K, V> ExactSizeIterator for VMIntoIterHash<K, V> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

pub struct IntoIter<K, V> {
    pub(crate) iter: std::vec::IntoIter<Bucket<K, V>>,
}

impl<K, V> IntoIter<K, V> {
    #[inline]
    fn map(b: Bucket<K, V>) -> (K, V) {
        (b.key, b.value)
    }
}

impl<'a, K: 'a, V: 'a> Iterator for IntoIter<K, V> {
    type Item = (K, V);

    def_iter!();
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for IntoIter<K, V> {
    def_double_ended_iter!();
}

impl<'a, K: 'a, V: 'a> ExactSizeIterator for IntoIter<K, V> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}
