/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_util::strong_hasher::StrongHash128;
use buck2_util::vec2;
use buck2_util::vec2::Vec2;
use dupe::Dupe;
use pagable::Pagable;

use crate::attrs::arena::AttrToken;
use crate::attrs::arena::AttrWord;
use crate::attrs::arena::CoercedAttrArenaBuilder;
use crate::attrs::arena::CoercedAttrRef;
use crate::attrs::arena::OwnedCoercedAttr;
use crate::attrs::arena::PackageAttrStorage;
use crate::attrs::spec::AttributeId;

/// One explicitly-set attribute, by arena ref. Hashing and equality read
/// the identity-grade strong hash from the arena's root table, so the row
/// carries no per-row hash of its own.
#[derive(Debug, Copy, Clone, Dupe, Allocative, Pagable)]
pub struct ArenaAttrValue {
    r: AttrWord,
}

/// Per-target attribute values, backed by the package's shared attr arena.
#[derive(Debug, Clone, Allocative, Pagable)]
pub struct AttrValues {
    storage: Arc<PackageAttrStorage>,
    sorted: Vec2<AttributeId, ArenaAttrValue>,
}

impl AttrValues {
    pub fn with_capacity(storage: Arc<PackageAttrStorage>, capacity: usize) -> AttrValues {
        AttrValues {
            storage,
            sorted: Vec2::with_capacity(capacity),
        }
    }

    pub(crate) fn get_by_index(&self, index: usize) -> Option<(AttributeId, CoercedAttrRef<'_>)> {
        self.sorted
            .get(index)
            .map(|(id, v)| (*id, self.storage.arena().view(v.r)))
    }

    pub(crate) fn get_owned_by_index(
        &self,
        index: usize,
    ) -> Option<(AttributeId, OwnedCoercedAttr)> {
        self.sorted
            .get(index)
            .map(|(id, v)| (*id, OwnedCoercedAttr::from_parts(self.storage.dupe(), v.r)))
    }

    #[expect(
        clippy::len_without_is_empty,
        reason = "`len` exists to size pre-allocations; no caller needs an emptiness check"
    )]
    pub fn len(&self) -> usize {
        self.sorted.len()
    }

    pub fn get(&self, id: AttributeId) -> Option<CoercedAttrRef<'_>> {
        // Could use binary search here, but for small attr map like 20
        // linear search is faster.
        for (next_id, next_value) in self.sorted.iter() {
            if next_id >= &id {
                if next_id == &id {
                    return Some(self.storage.arena().view(next_value.r));
                }

                // Attributes are sorted in `self`, no need to check the rest.
                return None;
            }
        }
        None
    }

    /// Like `get`, but as an owned handle sharing this target's storage.
    pub fn get_owned(&self, id: AttributeId) -> Option<OwnedCoercedAttr> {
        for (next_id, next_value) in self.sorted.iter() {
            if next_id >= &id {
                if next_id == &id {
                    return Some(OwnedCoercedAttr::from_parts(
                        self.storage.dupe(),
                        next_value.r,
                    ));
                }
                return None;
            }
        }
        None
    }

    pub fn shrink_to_fit(&mut self) {
        self.sorted.shrink_to_fit();
    }

    /// Record `value` for `id` as a root of the package arena. The builder
    /// must be the one that minted `value` and whose output will seal this
    /// value's storage.
    pub fn push_sorted(
        &mut self,
        builder: &mut CoercedAttrArenaBuilder,
        id: AttributeId,
        value: AttrToken,
    ) {
        if let Some((last_id, _)) = self.sorted.last() {
            assert!(&id > last_id, "attributed must be sorted");
        }
        let r = builder.root(value);
        self.sorted.push(id, ArenaAttrValue { r });
    }

    /// Adopt a full, sorted set of already-encoded per-target rows (coercion
    /// encodes values as they are parsed). The rows must be sorted by id
    /// (parse order guarantees it).
    pub fn from_encoded_pairs(
        storage: Arc<PackageAttrStorage>,
        pairs: Vec<(AttributeId, AttrWord)>,
    ) -> AttrValues {
        let mut values = AttrValues::with_capacity(storage, pairs.len());
        for (id, r) in pairs {
            if let Some((last_id, _)) = values.sorted.last() {
                assert!(&id > last_id, "attributes must be sorted");
            }
            values.sorted.push(id, ArenaAttrValue { r });
        }
        values.shrink_to_fit();
        values
    }

    /// The identity-grade strong hash of one row's value, from the arena's
    /// root table.
    fn strong(&self, v: ArenaAttrValue) -> StrongHash128 {
        self.storage
            .arena()
            .root_strong_hash(v.r)
            .expect("should be a root: rows are interned as roots")
    }

    pub fn ids_and_refs(&self) -> impl Iterator<Item = (AttributeId, CoercedAttrRef<'_>)> {
        let arena = self.storage.arena();
        self.sorted
            .iter()
            .map(move |(id, v)| (*id, arena.view(v.r)))
    }
}

impl PartialEq for AttrValues {
    fn eq(&self, other: &Self) -> bool {
        if self.sorted.len() != other.sorted.len() {
            return false;
        }
        // Refs are arena-local: compare ids, then identity hashes. The
        // 128-bit blake3 content hash IS equality (`CoercedAttrArena::strong_of`);
        // debug builds keep verifying that structurally.
        self.sorted
            .iter()
            .zip(other.sorted.iter())
            .all(|((id_a, a), (id_b, b))| {
                let eq = id_a == id_b && self.strong(*a) == other.strong(*b);
                if eq {
                    debug_assert!(
                        self.storage
                            .arena()
                            .view(a.r)
                            .content_eq(&other.storage.arena().view(b.r)),
                        "identity hashes matched but content differs"
                    );
                }
                eq
            })
    }
}

impl Eq for AttrValues {}

impl Hash for AttrValues {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (id, v) in self.sorted.iter() {
            id.hash(state);
            self.strong(*v).hash(state);
        }
    }
}

/// Attribute values sorted by [`AttributeId`], stored directly (not arena
/// encoded).
///
/// Invariant: entries are sorted by strictly ascending id. [`Self::new`]
/// establishes this and no mutation is possible afterwards. The
/// deterministic order is relied upon by `Eq`/`Hash`, by [`Self::get`], and
/// by merge walks against id-ordered `AttributeSpec::attr_specs()` iteration.
///
/// What is stored depends on the use: `AnonTarget` stores every
/// non-internal attribute (`V = AnonTargetAttr`) with defaults
/// materialized; `TargetNode` stores arena refs through the arena-backed
/// [`AttrValues`] instead.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable)]
pub struct SortedAttrValues<V> {
    sorted: Vec2<AttributeId, V>,
}

impl<V> SortedAttrValues<V> {
    /// Sorts `entries` by id and shrinks them to fit (attr values are
    /// typically retained for the lifetime of a node). Ids must be unique.
    ///
    /// Callers typically build entries in id order already (walking the
    /// `AttributeSpec`); construction then costs only one verification pass
    /// over the ids, with no sorting or copying.
    pub fn new(mut entries: Vec2<AttributeId, V>) -> SortedAttrValues<V> {
        if !entries.iter().is_sorted_by(|a, b| a.0 < b.0) {
            entries.sort_by(|(a_id, _), (b_id, _)| a_id.cmp(b_id));
            assert!(
                entries.iter().is_sorted_by(|a, b| a.0 < b.0),
                "attribute ids must be unique"
            );
        }
        entries.shrink_to_fit();
        SortedAttrValues { sorted: entries }
    }

    #[expect(
        clippy::len_without_is_empty,
        reason = "`len` exists to size pre-allocations; no caller needs an emptiness check"
    )]
    pub fn len(&self) -> usize {
        self.sorted.len()
    }

    pub fn get(&self, id: AttributeId) -> Option<&V> {
        // Could use binary search here, but for small attr map like 20
        // linear search is faster.
        for (next_id, next_value) in self {
            if next_id >= &id {
                if next_id == &id {
                    return Some(next_value);
                }

                // Attributes are sorted in `self`, no need to check the rest.
                return None;
            }
        }
        None
    }

    /// Iterate over the entries, in id order.
    pub fn iter(&self) -> vec2::Iter<'_, AttributeId, V> {
        self.sorted.iter()
    }

    /// Iterate over the values, in id order.
    pub fn values(&self) -> impl ExactSizeIterator<Item = &V> {
        self.iter().map(|(_, value)| value)
    }
}

impl<V> Default for SortedAttrValues<V> {
    fn default() -> SortedAttrValues<V> {
        SortedAttrValues {
            sorted: Vec2::new(),
        }
    }
}

impl<'a, V> IntoIterator for &'a SortedAttrValues<V> {
    type Item = (&'a AttributeId, &'a V);
    type IntoIter = vec2::Iter<'a, AttributeId, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.sorted.iter()
    }
}
