/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

/// Provides informative stats over all dice instances.
pub struct GlobalStats {
    /// A count of how many computations have been cancelled.
    pub cancellations: u64,
}

pub static CANCEL_COUNT: AtomicU64 = AtomicU64::new(0);

impl GlobalStats {
    pub fn get() -> Self {
        Self {
            cancellations: CANCEL_COUNT.load(Ordering::Relaxed),
        }
    }

    pub(crate) fn record_cancellation() {
        CANCEL_COUNT.fetch_add(1, Ordering::Relaxed);
    }
}
