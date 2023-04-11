/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Mutex;

use buck2_cli_proto::ClientContext;
use buck2_event_observer::span_tracker;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::span::SpanId;
use buck2_events::BuckEvent;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use once_cell::sync::Lazy;
use tokio::sync::oneshot;

static ACTIVE_COMMANDS: Lazy<Mutex<HashMap<TraceId, ActiveCommandHandle>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

/// Return the active commands, if you can access them.
pub fn try_active_commands() -> Option<HashMap<TraceId, ActiveCommandHandle>> {
    // Note that this function is accessed during panic, so have to be super careful
    Some(ACTIVE_COMMANDS.try_lock().ok()?.clone())
}

/// Broadcasts an instant event, returns whether any subscribers were connected.
pub fn broadcast_instant_event<E: Into<buck2_data::instant_event::Data> + Clone>(
    event: &E,
) -> bool {
    let mut has_subscribers = false;

    for cmd in ACTIVE_COMMANDS.lock().unwrap().values() {
        cmd.dispatcher.instant_event(event.clone());
        has_subscribers = true;
    }

    has_subscribers
}

pub fn broadcast_shutdown(shutdown: &buck2_data::DaemonShutdown) {
    for cmd in ACTIVE_COMMANDS.lock().unwrap().values() {
        cmd.notify_shutdown(shutdown.clone());
    }
}

/// Allows interactions with commands found via active_commands().
#[derive(Clone, Dupe)]
pub struct ActiveCommandHandle {
    /// A channel to send notifications to this command.
    dispatcher: EventDispatcher,

    /// A separate channel to broadcast shutdown events. This is separate from the EventDispatcher
    /// because we want to allow shutdown events to jump the queue.
    daemon_shutdown_channel: Arc<Mutex<Option<oneshot::Sender<buck2_data::DaemonShutdown>>>>,

    /// State for this command. This is used to expose what this command is doing to other clients.
    state: Arc<ActiveCommandState>,
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

/// A handle to the stats for this command. We use this to broadcast state about this command.
struct ActiveCommandState {
    #[allow(unused)]
    argv: Vec<String>,

    active_spans: AtomicU64,
}

/// A wrapper around ActiveCommandState that allows 1 client to write to it.
pub struct ActiveCommandStateWriter {
    active_spans: HashSet<SpanId>,
    shared: Arc<ActiveCommandState>,
}

impl ActiveCommandStateWriter {
    fn new(shared: Arc<ActiveCommandState>) -> Self {
        Self {
            active_spans: HashSet::new(),
            shared,
        }
    }

    pub fn peek_event(&mut self, buck_event: &BuckEvent) {
        let span_id = match buck_event.span_id() {
            Some(id) => id,
            None => return,
        };

        if buck_event.span_end_event().is_some() {
            if self.active_spans.remove(&span_id) {
                self.shared.active_spans.fetch_sub(1, Ordering::Relaxed);
            }
        } else if span_tracker::is_span_shown(buck_event) {
            self.active_spans.insert(span_id);
            self.shared.active_spans.fetch_add(1, Ordering::Relaxed);
        }
    }
}

pub struct ActiveCommand {
    pub guard: ActiveCommandDropGuard,
    pub state: ActiveCommandStateWriter,
    pub daemon_shutdown_channel: oneshot::Receiver<buck2_data::DaemonShutdown>,
}

impl ActiveCommand {
    pub fn new(event_dispatcher: &EventDispatcher, client_ctx: &ClientContext) -> Self {
        let (sender, receiver) = oneshot::channel();

        let state = Arc::new(ActiveCommandState {
            argv: client_ctx.sanitized_argv.clone(),
            active_spans: AtomicU64::new(0),
        });

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
                    state: state.dupe(),
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
            state: ActiveCommandStateWriter::new(state),
        }
    }
}
