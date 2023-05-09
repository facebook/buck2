/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::TypeId;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem;

use fnv::FnvHasher;

pub(crate) fn key_hash<K: Hash + 'static>(key: &K) -> u64 {
    let mut hasher = FnvHasher::default();
    if mem::size_of::<K>() == 0 {
        // Hashing `TypeId` unconditionally measurably slows down hashing.
        TypeId::of::<K>().hash(&mut hasher);
    } else {
        key.hash(&mut hasher);
    }
    hasher.finish()
}
