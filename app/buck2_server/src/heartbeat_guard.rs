/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;

use buck2_events::dispatch::EventDispatcher;
use tokio::task::JoinHandle;

use crate::snapshot::SnapshotCollector;

// Spawns a thread to occasionally output snapshots of resource utilization.
pub(crate) struct HeartbeatGuard {
    handle: Option<JoinHandle<()>>,
    collector: SnapshotCollector,
    events: EventDispatcher,
}

impl HeartbeatGuard {
    pub(crate) fn new(events: EventDispatcher, collector: SnapshotCollector) -> Self {
        // NOTE: This doesn't use the ambient dispatcher wrappers because we want to control the
        // exact lifetime of the dispatcher.
        let handle = tokio::spawn(buck2_util::async_move_clone!(events, collector, {
            let mut interval = tokio::time::interval(Duration::from_secs(1));
            interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
            loop {
                let snapshot = collector.create_snapshot().await;
                events.instant_event(Box::new(snapshot));
                interval.tick().await;
            }
        }));

        Self {
            handle: Some(handle),
            collector,
            events,
        }
    }

    pub(crate) async fn finalize(mut self) {
        // Make sure we stop sending new snapshots
        let handle = self.handle.take().unwrap();
        handle.abort();
        drop(handle.await);
        // Send one last snapshot.
        self.events
            .instant_event(Box::new(self.collector.create_snapshot().await));
    }
}

impl Drop for HeartbeatGuard {
    fn drop(&mut self) {
        if let Some(handle) = &mut self.handle {
            // We normally expect `finalize` to be called but maybe in the case of cancellation it isn't
            // and then its best to be sure
            handle.abort();
        }
    }
}
