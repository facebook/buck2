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

#[derive(Debug, Eq, PartialEq, Hash, Default, Allocative, Pagable)]
pub struct AttrValues {
    sorted: Vec2<AttributeId, CoercedAttr>,
}

impl AttrValues {
    pub fn with_capacity(capacity: usize) -> AttrValues {
        AttrValues {
            sorted: Vec2::with_capacity(capacity),
        }
    }

    pub(crate) fn get_by_index(&self, index: usize) -> Option<(AttributeId, &CoercedAttr)> {
        self.sorted.get(index).map(|(id, v)| (*id, v))
    }

    pub fn get(&self, id: AttributeId) -> Option<&CoercedAttr> {
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

    pub fn shrink_to_fit(&mut self) {
        self.sorted.shrink_to_fit();
    }

    pub fn push_sorted(&mut self, id: AttributeId, value: CoercedAttr) {
        if let Some((last_id, _)) = self.sorted.last() {
            assert!(&id > last_id, "attributed must be sorted");
        }
        self.sorted.push(id, value)
    }
}

impl<'a> IntoIterator for &'a AttrValues {
    type Item = (&'a AttributeId, &'a CoercedAttr);
    type IntoIter = vec2::Iter<'a, AttributeId, CoercedAttr>;

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
