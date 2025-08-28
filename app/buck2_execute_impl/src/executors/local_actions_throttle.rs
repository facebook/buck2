/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_resource_control::memory_tracker::TrackedMemorySender;
use tokio::sync::Mutex;
use tokio::sync::MutexGuard;

#[cfg_attr(windows, allow(dead_code))]
pub struct LocalActionsThrottle {
    memory_tracker: TrackedMemorySender,
    lock: Mutex<()>,
}

impl LocalActionsThrottle {
    pub fn new(memory_tracker: Option<TrackedMemorySender>) -> Option<Arc<Self>> {
        memory_tracker.map(|memory_tracker| {
            Arc::new(Self {
                memory_tracker,
                lock: Mutex::new(()),
            })
        })
    }

    /// When memory pressure only allow a single action to be scheduled.
    pub(crate) async fn throttle(&self) -> Option<MutexGuard<'_, ()>> {
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
            use buck2_resource_control::memory_tracker::MemoryReading;
            use buck2_resource_control::memory_tracker::MemoryState;
            use buck2_resource_control::memory_tracker::TrackedMemoryState;

            let mut rx = self.memory_tracker.subscribe();
            // If there is any problem with the tracker play it safe and don't block the execution.
            let _res = rx
                .wait_for(|x| match x {
                    TrackedMemoryState::Uninitialized | TrackedMemoryState::Failure => true,
                    TrackedMemoryState::Reading(MemoryReading {
                        state: MemoryState::BelowLimit,
                        ..
                    }) => true,
                    TrackedMemoryState::Reading(MemoryReading {
                        state: MemoryState::AboveLimit,
                        ..
                    }) => false,
                })
                .await;
        }
    }
}
