/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use parking_lot::Mutex;

use crate::traits::StorageState;

/// Deferred reads that failed on stored data, kept for the storage's lifetime.
///
/// A value paged back in keeps deferred fields that read storage on demand.
/// When such a read fails the value's owner is already resident and cannot be
/// repaired in place, unlike an eager page-in, which falls back to
/// recomputing. A host that finds entries here can no longer trust the values
/// it holds, and keeps finding them until it is replaced.
#[derive(Default)]
pub struct DeferredReadFailures {
    failed: Mutex<Vec<String>>,
}

impl StorageState for DeferredReadFailures {}

impl DeferredReadFailures {
    /// Records one failed read: what was being read and why it failed.
    pub fn record(&self, description: String) {
        self.failed.lock().push(description);
    }

    /// Every failure recorded so far.
    pub fn snapshot(&self) -> Vec<String> {
        self.failed.lock().clone()
    }
}
