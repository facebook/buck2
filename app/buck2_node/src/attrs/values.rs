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
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use pagable::Pagable;
use starlark_map::vec2;
use starlark_map::vec2::Vec2;

use super::attr_type::any_matches::AnyMatches;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::spec::AttributeId;

/// Attribute values sorted by [`AttributeId`].
///
/// Invariant: entries are sorted by strictly ascending id. [`Self::new`]
/// establishes this and no mutation is possible afterwards. The
/// deterministic order is relied upon by `Eq`/`Hash`, by [`Self::get`], and
/// by merge walks against id-ordered `AttributeSpec::attr_specs()` iteration.
///
/// What is stored depends on the use: `TargetNode` stores only explicitly
/// set attributes (`V = CoercedAttr`, the default) and looks defaults up
/// through the `AttributeSpec`; anon targets store every non-internal
/// attribute with defaults materialized.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable)]
pub struct AttrValues<V = CoercedAttr> {
    sorted: Vec2<AttributeId, V>,
}

impl<V> AttrValues<V> {
    /// Sorts `entries` by id and shrinks them to fit (attr values are
    /// typically retained for the lifetime of a node). Ids must be unique.
    ///
    /// Callers typically build entries in id order already (walking the
    /// `AttributeSpec`); construction then costs only one verification pass
    /// over the ids, with no sorting or copying.
    pub fn new(mut entries: Vec2<AttributeId, V>) -> AttrValues<V> {
        if !entries.iter().is_sorted_by(|a, b| a.0 < b.0) {
            entries.sort_by(|(a_id, _), (b_id, _)| a_id.cmp(b_id));
            assert!(
                entries.iter().is_sorted_by(|a, b| a.0 < b.0),
                "attribute ids must be unique"
            );
        }
        entries.shrink_to_fit();
        AttrValues { sorted: entries }
    }

    pub(crate) fn get_by_index(&self, index: usize) -> Option<(AttributeId, &V)> {
        self.sorted.get(index).map(|(id, v)| (*id, v))
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

impl<V> Default for AttrValues<V> {
    fn default() -> AttrValues<V> {
        AttrValues {
            sorted: Vec2::new(),
        }
    }
}

impl<'a, V> IntoIterator for &'a AttrValues<V> {
    type Item = (&'a AttributeId, &'a V);
    type IntoIter = vec2::Iter<'a, AttributeId, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.sorted.iter()
    }
}

#[derive(
    Debug,
    Dupe,
    Eq,
    PartialEq,
    Hash,
    Clone,
    Allocative,
    Default,
    derive_more::Display,
    Pagable
)]
#[display("{}", self.0.as_ref())]
pub struct TargetModifiersValue(Arc<serde_json::Value>);

impl TargetModifiersValue {
    pub fn new(v: serde_json::Value) -> Self {
        Self(Arc::new(v))
    }

    pub fn to_value(&self) -> serde_json::Value {
        (*self.0).clone()
    }

    pub fn as_json(&self) -> Arc<serde_json::Value> {
        self.0.dupe()
    }

    pub fn is_empty(&self) -> bool {
        match self.0.as_ref() {
            serde_json::Value::Null => true,
            serde_json::Value::Bool(_) => false,
            serde_json::Value::Number(_) => false,
            serde_json::Value::String(_) => false,
            serde_json::Value::Array(vec) => vec.is_empty(),
            serde_json::Value::Object(map) => map.is_empty(),
        }
    }
}

impl AnyMatches for TargetModifiersValue {
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        self.0.any_matches(filter)
    }
}
