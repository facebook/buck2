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

mod iter;

use std::hash::Hash;
use std::hash::Hasher;
use std::mem;

use gazebo::prelude::*;

use crate::equivalent::Equivalent;
use crate::hash_value::StarlarkHashValue;
use crate::hashed::Hashed;
pub use crate::vec_map::iter::IntoIter;
pub use crate::vec_map::iter::Iter;
pub use crate::vec_map::iter::IterMut;
use crate::vec_map::iter::VMIntoIterHash;
use crate::vec_map::iter::VMIterHash;
use crate::vec_map::iter::VMKeys;
use crate::vec_map::iter::VMValues;
use crate::vec_map::iter::VMValuesMut;

/// Bucket in [`VecMap`].
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct Bucket<K, V> {
    pub(crate) hash: StarlarkHashValue,
    pub(crate) key: K,
    pub(crate) value: V,
}

#[derive(Debug, Clone, Eq, PartialEq, Default_)]
pub struct VecMap<K, V> {
    pub(crate) buckets: Vec<Bucket<K, V>>,
}

impl<K, V> VecMap<K, V> {
    #[inline]
    pub(crate) const fn new() -> Self {
        VecMap {
            buckets: Vec::new(),
        }
    }

    #[inline]
    pub(crate) fn with_capacity(n: usize) -> Self {
        VecMap {
            buckets: Vec::with_capacity(n),
        }
    }

    pub(crate) fn reserve(&mut self, additional: usize) {
        self.buckets.reserve(additional);
    }

    #[inline]
    pub(crate) fn capacity(&self) -> usize {
        self.buckets.capacity()
    }

    pub(crate) fn extra_memory(&self) -> usize {
        self.buckets.capacity() * mem::size_of::<Bucket<K, V>>()
    }

    #[inline]
    pub(crate) fn get_full<Q>(&self, key: Hashed<&Q>) -> Option<(usize, &K, &V)>
    where
        Q: ?Sized + Equivalent<K>,
    {
        // This method is _very_ hot. There are three ways to implement this scan:
        // 1) Checked index operations.
        // 2) Unchecked index operations.
        // 3) Iterators.
        // Iterators would be best, but is significantly slower, so go with unchecked.
        // (25% on a benchmark which did a lot of other stuff too).
        let mut i = 0;
        #[allow(clippy::explicit_counter_loop)] // we are paranoid about performance
        for b in &self.buckets {
            // We always have at least as many hashes as value, so this index is safe.
            if b.hash == key.hash() && key.key().equivalent(&b.key) {
                return Some((i, &b.key, &b.value));
            }
            i += 1;
        }
        None
    }

    #[inline]
    pub(crate) fn get_index_of_hashed<Q>(&self, key: Hashed<&Q>) -> Option<usize>
    where
        Q: ?Sized + Equivalent<K>,
    {
        self.get_full(key).map(|(i, _, _)| i)
    }

    #[inline]
    pub(crate) fn get_index(&self, index: usize) -> Option<(&K, &V)> {
        self.buckets.get(index).map(|x| (&x.key, &x.value))
    }

    #[inline]
    pub(crate) unsafe fn get_unchecked(&self, index: usize) -> &Bucket<K, V> {
        debug_assert!(index < self.buckets.len());
        self.buckets.get_unchecked(index)
    }

    #[inline]
    pub(crate) unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut Bucket<K, V> {
        debug_assert!(index < self.buckets.len());
        self.buckets.get_unchecked_mut(index)
    }

    #[inline]
    pub(crate) fn insert_unique_unchecked(&mut self, key: Hashed<K>, value: V) {
        self.buckets.push(Bucket {
            hash: key.hash(),
            key: key.into_key(),
            value,
        });
    }

    pub(crate) fn remove_hashed_entry<Q>(&mut self, key: Hashed<&Q>) -> Option<(K, V)>
    where
        Q: ?Sized + Equivalent<K>,
    {
        let len = self.buckets.len();
        if len == 0 {
            return None;
        }

        for i in 0..len {
            if self.buckets[i].hash == key.hash() && key.key().equivalent(&self.buckets[i].key) {
                let b = self.buckets.remove(i);
                return Some((b.key, b.value));
            }
        }
        None
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.buckets.len()
    }

    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.buckets.is_empty()
    }

    pub(crate) fn clear(&mut self) {
        self.buckets.clear();
    }

    #[inline]
    pub(crate) fn values(&self) -> VMValues<K, V> {
        VMValues {
            iter: self.buckets.iter(),
        }
    }

    #[inline]
    pub(crate) fn values_mut(&mut self) -> VMValuesMut<K, V> {
        VMValuesMut {
            iter: self.buckets.iter_mut(),
        }
    }

    #[inline]
    pub(crate) fn keys(&self) -> VMKeys<K, V> {
        VMKeys {
            iter: self.buckets.iter(),
        }
    }

    #[inline]
    pub(crate) fn into_iter(self) -> IntoIter<K, V> {
        IntoIter {
            iter: self.buckets.into_iter(),
        }
    }

    #[inline]
    pub(crate) fn iter(&self) -> Iter<K, V> {
        Iter {
            iter: self.buckets.iter(),
        }
    }

    #[inline]
    pub(crate) fn iter_hashed(&self) -> VMIterHash<K, V> {
        VMIterHash {
            // Values go first since they terminate first and we can short-circuit
            iter: self.buckets.iter(),
        }
    }

    #[inline]
    pub(crate) fn into_iter_hashed(self) -> VMIntoIterHash<K, V> {
        // See the comments on VMIntoIterHash for why this one looks different
        VMIntoIterHash {
            iter: self.buckets.into_iter(),
        }
    }

    #[inline]
    pub(crate) fn iter_mut(&mut self) -> IterMut<K, V> {
        IterMut {
            iter: self.buckets.iter_mut(),
        }
    }

    /// Apply a function to each element in the map.
    ///
    /// # Panics
    ///
    /// This function panics if for any key hash is different after function application.
    pub fn into_try_map<E, K1, V1, F>(self, f: F) -> Result<VecMap<K1, V1>, E>
    where
        F: Fn(Hashed<K>, V) -> Result<(Hashed<K1>, V1), E>,
    {
        Ok(VecMap {
            buckets: self.buckets.into_try_map(|Bucket { hash, key, value }| {
                let hashed_key = Hashed::new_unchecked(hash, key);
                let (new_hashed_key, new_value) = f(hashed_key, value)?;
                assert!(hash == new_hashed_key.hash());
                Ok(Bucket {
                    hash: new_hashed_key.hash(),
                    key: new_hashed_key.into_key(),
                    value: new_value,
                })
            })?,
        })
    }

    pub(crate) fn sort_keys(&mut self)
    where
        K: Ord,
    {
        self.buckets.sort_by(|a, b| a.key.cmp(&b.key));
    }

    /// Equal if entries are equal in the iterator order.
    pub(crate) fn eq_ordered(&self, other: &Self) -> bool
    where
        K: Eq,
        V: Eq,
    {
        self.buckets.eq(&other.buckets)
    }

    /// Hash values in the iterator order.
    pub(crate) fn hash_ordered<H: Hasher>(&self, state: &mut H)
    where
        K: Hash,
        V: Hash,
    {
        self.buckets.hash(state);
    }
}
