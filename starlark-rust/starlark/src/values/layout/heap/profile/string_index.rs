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

use gazebo::dupe::Dupe;
use gazebo::prelude::*;
use starlark_map::small_set::SmallSet;

/// Map strings to integers 0, 1, 2, ...
#[derive(Default)]
pub(crate) struct StringIndex {
    strings: SmallSet<String>,
}

#[derive(Copy, Clone, Dupe, Debug, Eq, PartialEq, Hash)]
pub(crate) struct StringId(
    /// Index in strings index.
    pub(crate) usize,
);

impl StringIndex {
    pub(crate) fn index(&mut self, s: &str) -> StringId {
        if let Some(index) = self.strings.get_index_of(s) {
            return StringId(index);
        }

        let inserted = self.strings.insert(s.to_owned());
        assert!(inserted);
        StringId(self.strings.len() - 1)
    }

    pub(crate) fn get(&self, id: StringId) -> &str {
        self.strings.get_index(id.0).expect("invalid string id")
    }
}

#[derive(Debug, Default_)]
pub(crate) struct StringIndexMap<V> {
    values: Vec<Option<V>>,
}

impl<V> StringIndexMap<V> {
    pub(crate) fn or_insert(&mut self, id: StringId) -> &mut V
    where
        V: Default,
    {
        // Resize.
        while self.values.get(id.0).is_none() {
            self.values.push(None);
        }
        self.values[id.0].get_or_insert_with(Default::default)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (StringId, &V)> {
        self.values
            .iter()
            .enumerate()
            .filter_map(|(id, value)| value.as_ref().map(|value| (StringId(id), value)))
    }

    pub(crate) fn values(&self) -> impl Iterator<Item = &V> {
        self.values.iter().filter_map(Option::as_ref)
    }
}
