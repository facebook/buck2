/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;

use buck2_events::dispatch::EventDispatcher;
use buck2_events::trace::TraceId;
use dupe::Dupe;
use once_cell::sync::Lazy;
use tokio::sync::oneshot;

static ACTIVE_COMMANDS: Lazy<Mutex<HashMap<TraceId, ActiveCommandHandle>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

/// Return the active commands, if you know what they are
pub fn active_commands() -> Option<HashMap<TraceId, ActiveCommandHandle>> {
    // Note that this function is accessed during panic, so have to be super careful
    Some(ACTIVE_COMMANDS.try_lock().ok()?.clone())
}

pub fn broadcast_instant_event<E: Into<buck2_data::instant_event::Data> + Clone>(event: &E) {
    for cmd in ACTIVE_COMMANDS.lock().unwrap().values() {
        cmd.dispatcher.instant_event(event.clone())
    }
}

pub fn broadcast_shutdown(shutdown: &buck2_data::DaemonShutdown) {
    for cmd in ACTIVE_COMMANDS.lock().unwrap().values() {
        cmd.notify_shutdown(shutdown.clone());
    }
}

/// Allows interactions with commands found via active_commands().
#[derive(Clone, Dupe)]
pub struct ActiveCommandHandle {
    dispatcher: EventDispatcher,

    /// A separate channel to broadcast shutdown events. This is separate from the EventDispatcher
    /// because we want to allow shutdown events to jump the queue.
    daemon_shutdown_channel: Arc<Mutex<Option<oneshot::Sender<buck2_data::DaemonShutdown>>>>,
}

impl ActiveCommandHandle {
    fn notify_shutdown(&self, shutdown: buck2_data::DaemonShutdown) {
        let channel = self
            .daemon_shutdown_channel
            .lock()
            .unwrap_or_else(|e| e.into_inner())
            .take();

        if let Some(channel) = channel {
            let _ignored = channel.send(shutdown); // Nothing to do if receiver hung up.
        }
    }
}

pub struct ActiveCommandDropGuard {
    trace_id: TraceId,
}

impl Drop for ActiveCommandDropGuard {
    fn drop(&mut self) {
        ACTIVE_COMMANDS.lock().unwrap().remove(&self.trace_id);
    }
}

pub struct ActiveCommand {
    pub guard: ActiveCommandDropGuard,
    pub daemon_shutdown_channel: oneshot::Receiver<buck2_data::DaemonShutdown>,
}

impl ActiveCommand {
    pub fn new(event_dispatcher: &EventDispatcher) -> Self {
        let (sender, receiver) = oneshot::channel();

        let trace_id = event_dispatcher.trace_id().dupe();
        let result = {
            // Scope the guard so it's locked as little as possible
            let mut active_commands = ACTIVE_COMMANDS.lock().unwrap();

            let existing_active_commands = if active_commands.len() > 1 {
                Some(active_commands.clone())
            } else {
                None
            };

            active_commands.insert(
                trace_id.dupe(),
                ActiveCommandHandle {
                    dispatcher: event_dispatcher.dupe(),
                    daemon_shutdown_channel: Arc::new(Mutex::new(Some(sender))),
                },
            );

            existing_active_commands
        };

        if let Some(commands) = result {
            // Notify our command it is running concurrently with others.
            event_dispatcher.instant_event(buck2_data::TagEvent {
                tags: commands
                    .keys()
                    .map(|cmd| format!("concurrent_commands:{}", cmd))
                    .collect(),
            });

            // Notify other commands that they are concurrent with ours.
            for cmd in commands.values() {
                cmd.dispatcher.instant_event(buck2_data::TagEvent {
                    tags: vec![format!("concurrent_commands:{}", trace_id)],
                });
            }
        }

        Self {
            guard: ActiveCommandDropGuard { trace_id },
            daemon_shutdown_channel: receiver,
        }
    }
}
