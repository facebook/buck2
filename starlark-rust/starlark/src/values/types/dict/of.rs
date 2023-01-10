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

use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;

use starlark_map::small_map::SmallMap;

use crate as starlark;
use crate::values::dict::Dict;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::UnpackValue;
use crate::values::Value;

/// Like [`ValueOf`](crate::values::ValueOf), but only validates key and value types; does not construct
/// or store a map. Use `to_dict` to get at the map.
#[derive(Debug, Trace)]
pub struct DictOf<'v, K: UnpackValue<'v>, V: UnpackValue<'v>> {
    value: Value<'v>,
    phantom: PhantomData<(K, V)>,
}

impl<'v, K: UnpackValue<'v>, V: UnpackValue<'v>> DictOf<'v, K, V> {
    /// Get all the elements.
    // This should return an iterator, but it is not trivial to do with `ARef`.
    pub fn collect_entries(&self) -> Vec<(K, V)> {
        Dict::from_value(self.value)
            .expect("already validated as a dict")
            .iter()
            .map(|(k, v)| {
                (
                    K::unpack_value(k).expect("already validated key"),
                    V::unpack_value(v).expect("already validated value"),
                )
            })
            .collect()
    }
}

impl<'v, K: UnpackValue<'v> + Hash + Eq, V: UnpackValue<'v>> DictOf<'v, K, V> {
    /// Collect all the elements to a fresh `SmallMap`.
    pub fn to_dict(&self) -> SmallMap<K, V> {
        Dict::from_value(self.value)
            .expect("already validated as a dict")
            .iter()
            .map(|(k, v)| {
                (
                    K::unpack_value(k).expect("already validated key"),
                    V::unpack_value(v).expect("already validated value"),
                )
            })
            .collect()
    }
}

impl<'v, K: UnpackValue<'v>, V: UnpackValue<'v>> StarlarkTypeRepr for DictOf<'v, K, V> {
    fn starlark_type_repr() -> String {
        format!(
            "{{{}: {}}}",
            K::starlark_type_repr(),
            V::starlark_type_repr()
        )
    }
}

impl<'v, K: UnpackValue<'v>, V: UnpackValue<'v>> UnpackValue<'v> for DictOf<'v, K, V> {
    fn expected() -> String {
        format!("dict mapping {} to {}", K::expected(), V::expected())
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        let dict = Dict::from_value(value)?;
        let all_valid = dict
            .iter()
            .all(|(k, v)| K::unpack_value(k).is_some() && V::unpack_value(v).is_some());
        if all_valid {
            Some(DictOf {
                value,
                phantom: PhantomData,
            })
        } else {
            None
        }
    }
}

impl<'v, K: UnpackValue<'v> + Hash, V: UnpackValue<'v>> Deref for DictOf<'v, K, V> {
    type Target = Value<'v>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
