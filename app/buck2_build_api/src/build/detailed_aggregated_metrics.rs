/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod dice;
pub mod events;
mod implementation;
pub mod types;

pub type FxMultiMap<K, V> = multimap::MultiMap<K, V, fxhash::FxBuildHasher>;

pub mod testing {
    pub use super::implementation::traverse::traverse_partial_action_graph;
    pub use super::implementation::traverse::traverse_target_graph;
}
