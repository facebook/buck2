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

use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::Index;

/// Wrapper around `HashMap` that does not expose the order of the entries.
#[derive(Debug)]
pub(crate) struct UnorderedMap<K, V> {
    map: HashMap<K, V>,
}

impl<K, V> Default for UnorderedMap<K, V> {
    fn default() -> Self {
        UnorderedMap {
            map: HashMap::default(),
        }
    }
}

impl<K: Eq + Hash, V> UnorderedMap<K, V> {
    pub(crate) fn into_hash_map(self) -> HashMap<K, V> {
        self.map
    }

    pub(crate) fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(key)
    }

    pub(crate) fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get_mut(key)
    }

    pub(crate) fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.map.insert(key, value)
    }
}

impl<K: Eq + Hash, V> Index<&K> for UnorderedMap<K, V> {
    type Output = V;

    fn index(&self, index: &K) -> &Self::Output {
        &self.map[index]
    }
}

impl<K: Eq + Hash, V> FromIterator<(K, V)> for UnorderedMap<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        UnorderedMap {
            map: HashMap::from_iter(iter),
        }
    }
}
