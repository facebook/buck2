/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use gazebo::dupe::Dupe;

#[derive(Debug, Clone, Copy, Dupe)]
pub(crate) enum DiceTaskStateForDebugging {
    /// Async task finished.
    AsyncReady,
    /// Async task in progress.
    AsyncInProgress,
    /// Weak handle is empty.
    AsyncDropped,
    /// Sync task finished.
    SyncReady,
    /// Sync in progress.
    SyncInProgress,
}

/// Marker trait for a task currently executed in `IncrementalEngine`.
pub(crate) trait DiceTask: Allocative + Send + Sync + 'static {
    fn state_for_debugging(&self) -> DiceTaskStateForDebugging;
}
