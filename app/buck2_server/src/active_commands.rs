/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use buck2_cli_proto::ClientContext;
use buck2_event_observer::span_tracker;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::span::SpanId;
use buck2_events::BuckEvent;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use parking_lot::MutexGuard;
use tokio::sync::oneshot;

static ACTIVE_COMMANDS: Lazy<Mutex<HashMap<TraceId, ActiveCommandHandle>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

/// Return the active commands, if you can access them.
pub fn try_active_commands() -> Option<HashMap<TraceId, ActiveCommandHandle>> {
    // Note that this function is accessed during panic, so have to be super careful
    Some(ACTIVE_COMMANDS.try_lock()?.clone())
}

pub fn active_commands() -> MutexGuard<'static, HashMap<TraceId, ActiveCommandHandle>> {
    ACTIVE_COMMANDS.lock()
}

/// Broadcasts an instant event, returns whether any subscribers were connected.
pub fn broadcast_instant_event<E: Into<buck2_data::instant_event::Data> + Clone>(
    event: &E,
) -> bool {
    let mut has_subscribers = false;

    for cmd in ACTIVE_COMMANDS.lock().values() {
        cmd.dispatcher.instant_event(event.clone());
        has_subscribers = true;
    }

    has_subscribers
}

pub fn broadcast_shutdown(shutdown: &buck2_data::DaemonShutdown) {
    for cmd in ACTIVE_COMMANDS.lock().values() {
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
        let channel = self.daemon_shutdown_channel.lock().take();

        if let Some(channel) = channel {
            let _ignored = channel.send(shutdown); // Nothing to do if receiver hung up.
        }
    }

    pub fn state(&self) -> &ActiveCommandState {
        self.state.as_ref()
    }
}

pub struct ActiveCommandDropGuard {
    trace_id: TraceId,
}

impl Drop for ActiveCommandDropGuard {
    fn drop(&mut self) {
        ACTIVE_COMMANDS.lock().remove(&self.trace_id);
    }
}

/// A handle to the stats for this command. We use this to broadcast state about this command.
pub struct ActiveCommandState {
    #[allow(unused)]
    pub argv: Vec<String>,

    /// Top 32 bits for total spans. Lower 32 bits for closed spans.
    /// We don't have that many spans that we might exceed this.
    spans: AtomicU64,
}

impl ActiveCommandState {
    pub fn spans(&self) -> Spans {
        Spans::unpack(self.spans.load(Ordering::Relaxed))
    }

    fn new(argv: Vec<String>) -> Self {
        Self {
            argv,
            spans: AtomicU64::new(0),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Spans {
    pub open: u32,
    pub closed: u32,
}

impl Spans {
    const INCREMENT_FINISHED: u64 = 1;
    const INCREMENT_ACTIVE: u64 = 1 << 32;

    fn unpack(val: u64) -> Self {
        let closed: u32 = val as u32;
        let total: u32 = (val >> 32) as u32;

        Self {
            open: total - closed,
            closed,
        }
    }
}

/// A wrapper around ActiveCommandState that allows 1 client to write to it.
pub struct ActiveCommandStateWriter {
    /// Maps a SpanId to whether it is a root (i.e. no parent)
    active_spans: HashMap<SpanId, bool>,
    shared: Arc<ActiveCommandState>,
}

impl ActiveCommandStateWriter {
    fn new(shared: Arc<ActiveCommandState>) -> Self {
        Self {
            active_spans: HashMap::new(),
            shared,
        }
    }

    pub fn peek_event(&mut self, buck_event: &BuckEvent) {
        let span_id = match buck_event.span_id() {
            Some(id) => id,
            None => return,
        };

        if buck_event.span_end_event().is_some() {
            // If it's a root, then we decrement.
            if self.active_spans.remove(&span_id) == Some(true) {
                self.shared
                    .spans
                    .fetch_add(Spans::INCREMENT_FINISHED, Ordering::Relaxed);
            }
        } else if span_tracker::is_span_shown(buck_event) {
            let is_root = buck_event
                .parent_id()
                .map_or(true, |id| !self.active_spans.contains_key(&id));
            self.active_spans.insert(span_id, is_root);
            assert!(self.active_spans.len() as u64 <= u32::MAX as u64);

            if is_root {
                self.shared
                    .spans
                    .fetch_add(Spans::INCREMENT_ACTIVE, Ordering::Relaxed);
            }
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

        let state = Arc::new(ActiveCommandState::new(client_ctx.sanitized_argv.clone()));

        let trace_id = event_dispatcher.trace_id().dupe();
        let result = {
            // Scope the guard so it's locked as little as possible
            let mut active_commands = ACTIVE_COMMANDS.lock();

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

#[cfg(test)]
mod tests {
    use std::time::SystemTime;

    use super::*;

    #[test]
    fn test_spans() {
        let mut val = 0;

        val += Spans::INCREMENT_ACTIVE;
        assert_eq!(Spans::unpack(val), Spans { open: 1, closed: 0 });

        val += Spans::INCREMENT_ACTIVE;
        assert_eq!(Spans::unpack(val), Spans { open: 2, closed: 0 });

        val += Spans::INCREMENT_FINISHED;
        assert_eq!(Spans::unpack(val), Spans { open: 1, closed: 1 });

        val += Spans::INCREMENT_FINISHED;
        assert_eq!(Spans::unpack(val), Spans { open: 0, closed: 2 });
    }

    #[test]
    fn test_active_command_state() {
        let mut writer =
            ActiveCommandStateWriter::new(Arc::new(ActiveCommandState::new(Vec::new())));

        let root = SpanId::new();
        let child = SpanId::new();
        let trace = TraceId::new();

        writer.peek_event(&BuckEvent::new(
            SystemTime::now(),
            trace.clone(),
            Some(root),
            None,
            buck2_data::SpanStartEvent {
                data: Some(buck2_data::AnalysisStart::default().into()),
            }
            .into(),
        ));

        assert_eq!(writer.shared.spans(), Spans { open: 1, closed: 0 });

        writer.peek_event(&BuckEvent::new(
            SystemTime::now(),
            trace.clone(),
            Some(child),
            Some(root),
            buck2_data::SpanStartEvent {
                data: Some(buck2_data::AnalysisStageStart::default().into()),
            }
            .into(),
        ));

        assert_eq!(writer.shared.spans(), Spans { open: 1, closed: 0 });

        writer.peek_event(&BuckEvent::new(
            SystemTime::now(),
            trace.clone(),
            Some(child),
            Some(root),
            buck2_data::SpanEndEvent {
                data: Some(buck2_data::AnalysisStageEnd::default().into()),
                ..Default::default()
            }
            .into(),
        ));

        assert_eq!(writer.shared.spans(), Spans { open: 1, closed: 0 });

        writer.peek_event(&BuckEvent::new(
            SystemTime::now(),
            trace,
            Some(root),
            None,
            buck2_data::SpanEndEvent {
                data: Some(buck2_data::AnalysisEnd::default().into()),
                ..Default::default()
            }
            .into(),
        ));

        assert_eq!(writer.shared.spans(), Spans { open: 0, closed: 1 });
    }
}
