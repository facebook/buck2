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
use std::time::Instant;

use buck2_core::soft_error;
use buck2_events::dispatch::EventDispatcher;
use tokio::task::JoinHandle;

use crate::snapshot::SnapshotCollector;

// Spawns a thread to occasionally output snapshots of resource utilization.
pub(crate) struct HeartbeatGuard {
    handle: Option<JoinHandle<()>>,
    collector: SnapshotCollector,
    events: EventDispatcher,
}

fn check_slow_snapshot(elapsed: Duration, consecutive_slow: &mut u32) {
    // Slow snapshots are generally a sign of DICE core thread queue being backed up.
    // A single slow snapshot is expected if a large number of DICE requests are received in a short time.
    // Consecutive slow snapshots means the queue isn't clearing and the command could hang.
    if elapsed > Duration::from_secs(1) {
        *consecutive_slow += 1;
        if (*consecutive_slow).is_multiple_of(10) {
            soft_error!(
                "slow_snapshot",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Snapshot collection exceeded 1s for {} consecutive snapshots (last: {:.1}s). It's likely that the DICE core thread is stalled.",
                    *consecutive_slow,
                    elapsed.as_secs_f64()
                ),
                quiet: false
            )
            .ok();
        }
    } else {
        *consecutive_slow = 0;
    }
}

impl HeartbeatGuard {
    pub(crate) fn new(events: EventDispatcher, collector: SnapshotCollector) -> Self {
        // NOTE: This doesn't use the ambient dispatcher wrappers because we want to control the
        // exact lifetime of the dispatcher.
        let handle = tokio::spawn(buck2_util::async_move_clone!(events, collector, {
            let mut interval = tokio::time::interval(Duration::from_secs(1));
            interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
            let mut consecutive_slow: u32 = 0;
            loop {
                let start = Instant::now();
                let snapshot = collector.create_snapshot().await;
                events.instant_event(Box::new(snapshot));
                check_slow_snapshot(Instant::now() - start, &mut consecutive_slow);
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
        // Send one last snapshot, with the page-in breakdown so this command's
        // page-in overhead is captured in the invocation record.
        self.events.instant_event(Box::new(
            self.collector.create_snapshot_with_page_in_metrics().await,
        ));
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
