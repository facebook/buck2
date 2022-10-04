/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::significant_drop_in_scrutinee)] // FIXME?

use std::collections::HashMap;
use std::time::Duration;

use buck2_data::*;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_events::dispatch::EventDispatcher;
use dice::DiceEvent;
use dice::DiceTracker;
use futures::channel::mpsc;
use futures::channel::mpsc::UnboundedReceiver;
use futures::channel::mpsc::UnboundedSender;
use futures::StreamExt;
use gazebo::dupe::Dupe;

/// The BuckDiceTracker keeps track of the started/finished events for a dice computation and periodically sends a snapshot to the client.
///
/// There are too many events coming out of dice for us to forward them all to the client, so we need to aggregate
/// them in some way in the daemon.
///
/// The tracker will send a snapshot event every 500ms (only if there have been changes since the last snapshot).
///
/// A client won't necessarily get a final snapshot before a command returns.
pub struct BuckDiceTracker {
    event_forwarder: UnboundedSender<DiceEvent>,
}

const DICE_SNAPSHOT_INTERVAL: Duration = Duration::from_millis(500);

impl BuckDiceTracker {
    pub fn new(events: EventDispatcher) -> Self {
        let (event_forwarder, receiver) = mpsc::unbounded();

        std::thread::spawn(move || {
            let runtime = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            runtime.block_on(with_dispatcher_async(
                events.dupe(),
                Self::run_task(events, receiver),
            ))
        });

        Self { event_forwarder }
    }

    async fn run_task(events: EventDispatcher, mut receiver: UnboundedReceiver<DiceEvent>) {
        let mut needs_update = false;
        let mut states = HashMap::new();
        let mut interval = tokio::time::interval(DICE_SNAPSHOT_INTERVAL);
        interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
        // This will loop until the sender side of the channel is dropped.
        loop {
            tokio::select! {
                ev = receiver.next() => {
                    needs_update = true;
                    match ev {
                        Some(DiceEvent::Started{key_type}) => {
                            states.entry(key_type).or_insert_with(DiceKeyState::default).started += 1;
                        }
                        Some(DiceEvent::Finished{key_type}) => {
                            states.entry(key_type).or_insert_with(DiceKeyState::default).finished += 1;
                        }
                        None => {
                            // This indicates that the sender side has been dropped and we can exit.
                            break;
                        }
                    }
                }
                _ = interval.tick() => {
                    if needs_update {
                        needs_update = false;
                        events.instant_event(DiceStateSnapshot {
                            key_states: states
                                .iter()
                                .map(|(k, v)| ((*k).to_owned(), v.clone()))
                                .collect(),
                        });
                    }
                }
            }
        }
    }
}

impl DiceTracker for BuckDiceTracker {
    fn event(&self, event: DiceEvent) {
        let _ = self.event_forwarder.unbounded_send(event);
    }
}
