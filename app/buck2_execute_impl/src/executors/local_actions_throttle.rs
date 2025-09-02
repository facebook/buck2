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

use buck2_resource_control::memory_tracker::MemoryTrackerHandle;
use tokio::sync::Mutex;
use tokio::sync::MutexGuard;

#[cfg_attr(windows, allow(dead_code))]
pub struct LocalActionsThrottle {
    memory_tracker: MemoryTrackerHandle,
    lock: Mutex<()>,
}

impl LocalActionsThrottle {
    pub fn new(memory_tracker: Option<MemoryTrackerHandle>) -> Option<Arc<Self>> {
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
            use buck2_resource_control::memory_tracker::MemoryCurrentState;
            use buck2_resource_control::memory_tracker::MemoryStates;
            use buck2_resource_control::memory_tracker::TrackedMemoryState;

            fn should_throttle(state: TrackedMemoryState) -> bool {
                match state {
                    TrackedMemoryState::Reading(MemoryStates {
                        memory_current_state: MemoryCurrentState::AboveLimit,
                        ..
                    }) => true,
                    _ => false,
                }
            }

            let mut rx = self.memory_tracker.state_sender.subscribe();
            // wait_for will run the closure before blocking so no need to check the value first.
            let _res = rx.wait_for(|x| !should_throttle(*x)).await;
        }
    }
}
