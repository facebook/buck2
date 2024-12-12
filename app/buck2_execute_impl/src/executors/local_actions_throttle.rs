/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::memory_tracker::MemoryTracker;
use tokio::sync::Mutex;
use tokio::sync::MutexGuard;

#[cfg_attr(windows, allow(dead_code))]
pub struct LocalActionsThrottle {
    memory_tracker: Arc<MemoryTracker>,
    memory_limit_gibibytes: u64,
    lock: Mutex<()>,
}

impl LocalActionsThrottle {
    pub fn new(
        memory_tracker: Option<Arc<MemoryTracker>>,
        memory_limit_gibibytes: Option<u64>,
    ) -> Option<Arc<Self>> {
        if let (Some(memory_tracker), Some(memory_limit_gibibytes)) =
            (memory_tracker, memory_limit_gibibytes)
        {
            Some(Arc::new(Self {
                memory_tracker,
                memory_limit_gibibytes,
                lock: Mutex::new(()),
            }))
        } else {
            None
        }
    }

    /// When memory pressure only allow a single action to be scheduled.
    pub(crate) async fn throttle(&self) -> Option<MutexGuard<()>> {
        tokio::select! {
            _ = self.ensure_low_memory_pressure() => {
                None
            }
            guard = self.lock.lock() => {
                Some(guard)
            }
        }
    }

    /// Waits until memory pressure is cleared.
    pub(crate) async fn ensure_low_memory_pressure(&self) {
        #[cfg(unix)]
        {
            use buck2_common::memory_tracker::TrackedMemoryState;

            let mut rx = self.memory_tracker.subscribe().await;
            // If there is any problem with a tracker play it safe and don't block the execution.
            let _res = rx
                .wait_for(|x| match x {
                    TrackedMemoryState::Uninitialized | TrackedMemoryState::Failure => true,
                    TrackedMemoryState::Reading { memory_current } => {
                        *memory_current < self.memory_limit_gibibytes * 1024 * 1024 * 1024
                    }
                })
                .await;
        }
    }
}
