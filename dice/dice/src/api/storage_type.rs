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
///
/// For an Injected entry, we must store all values that are still reachable because we
/// cannot recompute them.
#[derive(UnpackVariants, Debug, Clone, Copy, Dupe, Allocative)]
pub enum StorageType {
    Normal,
    Injected,
}
