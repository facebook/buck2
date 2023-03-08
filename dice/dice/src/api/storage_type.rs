/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;

/// Storage type for a cached entry.
/// The oldest entry will be evicted once the cache stores more than N entries of the same key
/// request to compute them. TODO think about whether or not we can
/// optimize to delete injected keys when no more computation will request that version
#[derive(UnpackVariants, Debug, Clone, Copy, Dupe, Allocative)]
pub enum StorageType {
    LastN(usize),
}
