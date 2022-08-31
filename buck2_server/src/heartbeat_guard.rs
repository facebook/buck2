/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::significant_drop_in_scrutinee)]

use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;

use buck2_events::dispatch::EventDispatcher;
use gazebo::dupe::Dupe;
use tokio::task::JoinHandle;

use crate::ctx::BaseServerCommandContext;
use crate::snapshot;

// Spawns a thread to occasionally output snapshots of resource utilization.
pub struct HeartbeatGuard {
    handle: JoinHandle<()>,
    collector: snapshot::SnapshotCollector,
    events: Arc<Mutex<Option<EventDispatcher>>>,
}

impl HeartbeatGuard {
    pub fn new(ctx: &BaseServerCommandContext) -> Self {
        let events = Arc::new(Mutex::new(Some(ctx.events.dupe())));
        let collector = snapshot::SnapshotCollector::new(
            ctx.re_client_manager.dupe(),
            ctx.blocking_executor.dupe(),
        );

        // NOTE: This doesn't use the ambient dispatcher wrappers because we want to control the
        // exact lifetime of the dispatcher.
        let handle = tokio::spawn({
            let events = events.dupe();
            let collector = collector.dupe();
            async move {
                let mut interval = tokio::time::interval(Duration::from_secs(1));
                interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
                loop {
                    let snapshot = collector.create_snapshot();
                    match events.lock().expect("Poisoned lock").as_ref() {
                        Some(events) => events.instant_event(snapshot),
                        None => break,
                    }
                    interval.tick().await;
                }
            }
        });

        Self {
            handle,
            collector,
            events,
        }
    }
}

impl Drop for HeartbeatGuard {
    fn drop(&mut self) {
        let mut maybe_events = self.events.lock().expect("Poisoned lock");
        // Synchronously remove access for sending new heartbeats.
        if let Some(events) = maybe_events.take() {
            // Send one last snapshot.
            events.instant_event(self.collector.create_snapshot());
        }
        // Cancel the task as well.
        self.handle.abort();
    }
}
