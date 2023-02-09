/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;

use fnv::FnvHasher;

pub(crate) fn key_hash<K: Hash>(key: &K) -> u64 {
    let mut hasher = FnvHasher::default();
    key.hash(&mut hasher);
    hasher.finish()
}
