/*
 * Copyright 2026 The Starlark in Rust Authors.
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

//! Lookup function for static value identifiers.
//!
//! This module provides the main entry point for checking if a FrozenValue
//! is a static value and retrieving its identifier.

use std::collections::HashMap;
use std::hash::Hasher;
use std::sync::LazyLock;

use super::registry::StaticValueEntry;
use super::static_string::STATIC_STRING_ADDR_TO_ID;
use crate::values::FrozenValue;

/// A unique identifier for a statically-allocated Starlark value.
///
/// This is a hash-based ID computed from deterministic keys
/// (e.g., synthetic keys for short strings, or `(file, line)` for
/// inventory-registered values).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StaticValueId(u64);

impl StaticValueId {
    /// Create a `StaticValueId` from a raw u64 hash.
    #[inline]
    pub(crate) fn new(hash: u64) -> Self {
        Self(hash)
    }
}

/// Compute a deterministic `StaticValueId` for a single ASCII char string.
///
/// `byte` is the ASCII byte value (0x00..0x7F).
pub(crate) fn hash_short_string(byte: u8) -> StaticValueId {
    let mut hasher = starlark_map::StarlarkHasher::new();
    hasher.write(b"__short_string__");
    hasher.write_u8(byte);
    StaticValueId::new(hasher.finish())
}

/// Compute a deterministic `StaticValueId` for the empty string.
pub(crate) fn hash_empty_string() -> StaticValueId {
    let mut hasher = starlark_map::StarlarkHasher::new();
    hasher.write(b"__empty_string__");
    StaticValueId::new(hasher.finish())
}

/// Compute a deterministic `StaticValueId` from a (file, line) pair.
pub(crate) fn hash_file_line(file: &str, line: u32) -> StaticValueId {
    let mut hasher = starlark_map::StarlarkHasher::new();
    hasher.write(file.as_bytes());
    hasher.write_u32(line);
    StaticValueId::new(hasher.finish())
}

/// Static map from inventory-registered static value addresses to their IDs.
///
/// This map is lazily initialized at runtime from inventory-registered entries.
/// It contains mappings for values registered via `const_frozen_string!`,
/// `static_starlark_value!`, or manual `inventory::submit!`.
static INVENTORY_ADDR_TO_ID: LazyLock<HashMap<usize, StaticValueId>> = LazyLock::new(|| {
    inventory::iter::<StaticValueEntry>()
        .map(|e| {
            let addr = (e.get_value)().ptr_value().ptr_value_untagged();
            let id = hash_file_line(e.file, e.line);
            (addr, id)
        })
        .collect()
});

/// Check if a FrozenValue points to a static value.
///
/// Returns `Some(StaticValueId)` if the value is a registered static value,
/// `None` otherwise.
#[allow(dead_code)]
pub(crate) fn get_static_value_id(fv: FrozenValue) -> Option<StaticValueId> {
    // Only check for pointer values, not inline integers
    if fv.ptr_value().is_int() {
        return None;
    }

    let addr = fv.ptr_value().ptr_value_untagged();

    // Check static strings (empty string and single ASCII characters)
    if let Some(id) = STATIC_STRING_ADDR_TO_ID.get(&addr) {
        return Some(*id);
    }

    // Check inventory-registered values (const_frozen_string!, static_starlark_value!, etc.)
    if let Some(id) = INVENTORY_ADDR_TO_ID.get(&addr) {
        return Some(*id);
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::const_frozen_string;
    use crate::values::layout::static_string::constant_string;

    #[test]
    fn test_empty_string_has_id() {
        let empty = constant_string("").unwrap();
        let id = get_static_value_id(empty.to_frozen_value());
        assert!(id.is_some(), "empty string should be registered");
    }

    #[test]
    fn test_single_char_has_id() {
        let a = constant_string("a").unwrap();
        let id = get_static_value_id(a.to_frozen_value());
        assert!(id.is_some(), "'a' should be registered");
    }

    #[test]
    fn test_multi_char_string_is_registered() {
        let s = const_frozen_string!("hello");
        let id = get_static_value_id(s.to_frozen_value());
        assert!(id.is_some(), "const_frozen_string! should be registered");
    }

    #[test]
    fn test_all_ascii_chars_registered() {
        for i in 0u8..128 {
            let bytes = [i];
            let s = std::str::from_utf8(&bytes).unwrap();
            let fv = constant_string(s).unwrap();
            let id = get_static_value_id(fv.to_frozen_value());
            assert!(id.is_some(), "ASCII char {} should be registered", i);
        }
    }

    #[test]
    fn test_all_short_string_ids_unique() {
        let mut ids = std::collections::HashSet::new();
        // empty string
        let empty = constant_string("").unwrap();
        ids.insert(get_static_value_id(empty.to_frozen_value()).unwrap());
        // all 128 ASCII chars
        for i in 0u8..128 {
            let bytes = [i];
            let s = std::str::from_utf8(&bytes).unwrap();
            let fv = constant_string(s).unwrap();
            ids.insert(get_static_value_id(fv.to_frozen_value()).unwrap());
        }
        assert_eq!(
            ids.len(),
            129,
            "all 129 short strings should have unique IDs"
        );
    }

    #[test]
    fn test_determinism() {
        let a1 = constant_string("a").unwrap();
        let a2 = constant_string("a").unwrap();
        let id1 = get_static_value_id(a1.to_frozen_value());
        let id2 = get_static_value_id(a2.to_frozen_value());
        assert_eq!(id1, id2, "same value should produce same ID");
    }

    #[test]
    fn test_singleton_none_is_registered() {
        let none_val = FrozenValue::new_none();
        let id = get_static_value_id(none_val);
        assert!(id.is_some(), "None should be registered");
    }

    #[test]
    fn test_singleton_bools_are_registered() {
        let false_val = FrozenValue::new_bool(false);
        let true_val = FrozenValue::new_bool(true);
        let false_id = get_static_value_id(false_val);
        let true_id = get_static_value_id(true_val);
        assert!(false_id.is_some(), "false should be registered");
        assert!(true_id.is_some(), "true should be registered");
        assert_ne!(
            false_id, true_id,
            "true and false should have different IDs"
        );
    }

    #[test]
    fn test_singleton_empty_tuple_is_registered() {
        let empty_tuple = FrozenValue::new_empty_tuple();
        let id = get_static_value_id(empty_tuple);
        assert!(id.is_some(), "empty tuple should be registered");
    }

    #[test]
    fn test_singleton_empty_array_is_registered() {
        use crate::values::types::array::VALUE_EMPTY_ARRAY;

        let empty_array = VALUE_EMPTY_ARRAY.unpack().to_frozen_value();
        let id = get_static_value_id(empty_array);
        assert!(id.is_some(), "empty array should be registered");
    }
}
