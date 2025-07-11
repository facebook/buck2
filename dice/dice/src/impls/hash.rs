/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::any::TypeId;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem;

use fxhash::FxHasher;

pub(crate) fn key_hash<K: Hash + 'static>(key: &K) -> u64 {
    let mut hasher = FxHasher::default();
    if mem::size_of::<K>() == 0 {
        // Hashing `TypeId` unconditionally measurably slows down hashing.
        TypeId::of::<K>().hash(&mut hasher);
    } else {
        key.hash(&mut hasher);
    }
    hasher.finish()
}
