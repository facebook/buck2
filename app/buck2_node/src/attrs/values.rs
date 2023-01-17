/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::slice;

use allocative::Allocative;

use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::id::AttributeId;

#[derive(Debug, Eq, PartialEq, Hash, Default, Allocative)]
pub struct AttrValues {
    sorted: Vec<(AttributeId, CoercedAttr)>,
}

impl AttrValues {
    pub fn with_capacity(capacity: usize) -> AttrValues {
        AttrValues {
            sorted: Vec::with_capacity(capacity),
        }
    }

    pub(crate) fn get_by_index(&self, index: usize) -> Option<&(AttributeId, CoercedAttr)> {
        self.sorted.get(index)
    }

    pub(crate) fn get(&self, id: AttributeId) -> Option<&CoercedAttr> {
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
        self.sorted.push((id, value))
    }
}

impl<'a> IntoIterator for &'a AttrValues {
    type Item = &'a (AttributeId, CoercedAttr);
    type IntoIter = slice::Iter<'a, (AttributeId, CoercedAttr)>;

    fn into_iter(self) -> Self::IntoIter {
        self.sorted.iter()
    }
}
