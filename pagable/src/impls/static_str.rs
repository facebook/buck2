/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Pagable implementation for static strings.
//!
//! This module provides [`StaticStr`], a wrapper for `&'static str` that supports
//! pagable serialization. Use the [`static_str!`] macro to create and register instances.
//!
//! Serialization uses an index into the registry instead of the actual string content,
//! making it more compact. The index is cached in each registered entry for O(1) access.

use std::fmt;
use std::ops::Deref;

use allocative::Allocative;
use allocative::Visitor;
use dupe::Dupe;
use once_cell::sync::Lazy;
use serde::Deserialize;
use serde::Serialize;

use crate::PagableDeserialize;
use crate::PagableDeserializer;
use crate::PagableSerialize;
use crate::PagableSerializer;

/// A registered static string entry. Created by `static_str!` macro.
/// The index is populated when the registry is initialized.
pub struct StaticStrEntry {
    pub s: &'static str,
    /// Index is set once during registry initialization.
    /// Using Cell is safe because initialization happens once at startup
    /// before any concurrent access.
    /// Public for macro access.
    pub index: std::cell::Cell<u32>,
}

// SAFETY: StaticStrEntry is Sync because:
// - The Cell<u32> is only written once during single-threaded static initialization
// - After initialization, it's only read
unsafe impl Sync for StaticStrEntry {}

impl PartialEq for StaticStrEntry {
    fn eq(&self, other: &Self) -> bool {
        self.s == other.s
    }
}

impl Eq for StaticStrEntry {}

impl std::hash::Hash for StaticStrEntry {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.s.hash(state);
    }
}

inventory::collect!(&'static StaticStrEntry);

/// A registered static string for pagable serialization/deserialization.
///
/// Use the `static_str!` macro to create and register instances:
///
/// ```ignore
/// pagable::static_str!(MY_STRING = "my_known_string");
/// ```
///
/// During serialization, an index is used instead of the string content.
/// During deserialization, the index is looked up to recover the static string.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Dupe)]
pub struct StaticStr(pub &'static StaticStrEntry);

impl StaticStr {
    pub const fn as_str(self) -> &'static str {
        self.0.s
    }
}

impl Deref for StaticStr {
    type Target = &'static str;

    fn deref(&self) -> &Self::Target {
        &self.0.s
    }
}

impl fmt::Debug for StaticStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.0.s, f)
    }
}

impl fmt::Display for StaticStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.0.s, f)
    }
}

impl Allocative for StaticStr {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        // StaticStr is just a reference to static data, no heap allocation
        let _ = visitor;
    }
}

/// Create and register a static string constant.
///
/// This macro creates a const and registers it for pagable deserialization.
///
/// # Example
///
/// ```ignore
/// pagable::static_str!(MY_NAME = "john");
/// pagable::static_str!(pub MY_PUBLIC_NAME = "jane");
///
/// fn example() {
///     let name: StaticStr = MY_NAME;
///     assert_eq!(name.as_str(), "john");
/// }
/// ```
#[macro_export]
macro_rules! static_str {
    ($vis:vis $name:ident = $s:expr) => {
        $vis static $name: $crate::StaticStr = {
            static ENTRY: $crate::__internal::StaticStrEntry = $crate::__internal::StaticStrEntry {
                s: $s,
                index: $crate::__internal::Cell::new(u32::MAX),
            };
            $crate::__internal::inventory::submit! { &ENTRY }
            $crate::StaticStr(&ENTRY)
        };
    };
}

/// Registry mapping indices to static strings, built once at startup.
struct StaticStrRegistry {
    /// Map from index to entry (for deserialization)
    entries: Vec<&'static StaticStrEntry>,
}

static REGISTRY: Lazy<StaticStrRegistry> = Lazy::new(|| {
    let mut entries: Vec<&'static StaticStrEntry> = inventory::iter::<&'static StaticStrEntry>
        .into_iter()
        .copied()
        .collect();
    // Sort for deterministic ordering across runs
    entries.sort_by_key(|e| e.s);
    // Assign indices to each entry
    for (i, entry) in entries.iter().enumerate() {
        entry.index.set(i as u32);
    }
    StaticStrRegistry { entries }
});

/// Ensure the registry is initialized. Called automatically on first serialize/deserialize.
fn ensure_registry_initialized() {
    Lazy::force(&REGISTRY);
}

impl PagableSerialize for StaticStr {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> crate::Result<()> {
        ensure_registry_initialized();
        let index = self.0.index.get();
        if index == u32::MAX {
            panic!(
                "Unregistered static string: {:?}. Register it with: pagable::static_str!(NAME = {:?})",
                self.0.s, self.0.s
            );
        }
        Ok(index.serialize(serializer.serde())?)
    }
}

impl<'de> PagableDeserialize<'de> for StaticStr {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> crate::Result<Self> {
        ensure_registry_initialized();
        let index = u32::deserialize(deserializer.serde())?;

        match REGISTRY.entries.get(index as usize) {
            Some(&entry) => Ok(StaticStr(entry)),
            None => panic!(
                "Invalid static string index: {}. Registry has {} entries.",
                index,
                REGISTRY.entries.len()
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::TestingDeserializer;
    use crate::testing::TestingSerializer;

    crate::static_str!(STRING_ALPHA = "alpha");
    crate::static_str!(STRING_BETA = "beta");
    crate::static_str!(STRING_GAMMA = "gamma");
    crate::static_str!(STRING_DELTA = "delta");

    #[test]
    fn test_indices_are_unique_and_sorted_alphabetically() {
        ensure_registry_initialized();
        // Strings sorted: alpha < beta < delta < gamma
        let strings = [STRING_ALPHA, STRING_BETA, STRING_DELTA, STRING_GAMMA];
        let indices: Vec<_> = strings.iter().map(|s| s.0.index.get()).collect();

        for (i, &idx) in indices.iter().enumerate() {
            assert_ne!(idx, u32::MAX);
            if i > 0 {
                assert!(
                    indices[i - 1] < idx,
                    "indices should be strictly increasing"
                );
            }
        }
    }

    #[test]
    fn test_serialize_multiple() -> crate::Result<()> {
        let input = [STRING_GAMMA, STRING_ALPHA, STRING_DELTA, STRING_BETA];

        let mut serializer = TestingSerializer::new();
        for s in &input {
            s.pagable_serialize(&mut serializer)?;
        }
        let bytes = serializer.finish();

        let mut deserializer = TestingDeserializer::new(&bytes);
        for expected in &input {
            let restored = StaticStr::pagable_deserialize(&mut deserializer)?;
            assert_eq!(expected.as_str(), restored.as_str());
        }
        Ok(())
    }
}
