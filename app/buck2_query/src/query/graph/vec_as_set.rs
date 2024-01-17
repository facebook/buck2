/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)] // Used later in the stack.

use std::fmt;
use std::fmt::Debug;

use crate::query::graph::vec_as_map::VecAsMap;

#[derive(Default)]
pub(crate) struct VecAsSet {
    // Can use bitset here.
    vec: VecAsMap<()>,
}

impl Debug for VecAsSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.vec.keys()).finish()
    }
}

impl VecAsSet {
    pub(crate) fn contains(&self, index: u32) -> bool {
        self.vec.contains_key(index)
    }

    /// Return true if the index was not already present.
    pub(crate) fn insert(&mut self, index: u32) -> bool {
        self.vec.insert(index, ()).is_none()
    }
}
