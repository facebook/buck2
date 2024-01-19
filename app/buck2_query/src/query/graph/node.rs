/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use dupe::Dupe;
use starlark_map::Hashed;

pub trait NodeKey: Clone + Hash + PartialEq + Eq + Debug + Display + Send + Sync + 'static {}

pub trait LabeledNode: Dupe + Send + Sync {
    type Key: NodeKey;

    fn node_key(&self) -> &Self::Key;

    fn hashed_node_key(&self) -> Hashed<&Self::Key> {
        Hashed::new(self.node_key())
    }
}
