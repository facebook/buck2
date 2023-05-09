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
use std::sync::Arc;

use buck2_cli_proto::ClientContext;
use buck2_event_observer::dice_state::DiceState;
use buck2_event_observer::pending_estimate::pending_estimate;
use buck2_event_observer::span_tracker;
use buck2_event_observer::span_tracker::RootData;
use buck2_event_observer::span_tracker::Roots;
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

    spans: Mutex<SpansSnapshot>,
}

impl ActiveCommandState {
    pub fn spans(&self) -> SpansSnapshot {
        *self.spans.lock()
    }

    fn new(argv: Vec<String>) -> Self {
        Self {
            argv,
            spans: Mutex::new(SpansSnapshot::default()),
        }
    }
}

#[derive(PartialEq, Debug, Default, Copy, Clone, Dupe)]
pub struct SpansSnapshot {
    pub open: u64,
    pub closed: u64,
    pub pending: u64,
}

/// A wrapper around ActiveCommandState that allows 1 client to write to it.
pub struct ActiveCommandStateWriter {
    /// Maps a SpanId to whether it is a root (i.e. no parent)
    roots: Roots<Arc<BuckEvent>>,
    non_roots: HashSet<SpanId>,
    dice_state: DiceState,
    closed: u64,
    shared: Arc<ActiveCommandState>,
}

impl ActiveCommandStateWriter {
    fn new(shared: Arc<ActiveCommandState>) -> Self {
        Self {
            roots: Roots::default(),
            non_roots: HashSet::new(),
            dice_state: DiceState::new(),
            closed: 0,
            shared,
        }
    }

    pub fn peek_event(&mut self, buck_event: &BuckEvent) {
        use buck2_data::buck_event::Data::*;

        let mut changed = false;

        match buck_event.data() {
            SpanStart(..) => {
                let span_id = match buck_event.span_id() {
                    Some(id) => id,
                    None => return,
                };

                if !span_tracker::is_span_shown(buck_event) {
                    return;
                }

                let is_root = buck_event.parent_id().map_or(true, |id| {
                    !self.roots.contains(id) && !self.non_roots.contains(&id)
                });

                if is_root {
                    self.roots.insert(span_id, false, RootData::new(buck_event));
                    changed = true;
                } else {
                    self.non_roots.insert(span_id);
                }
            }
            SpanEnd(..) => {
                let span_id = match buck_event.span_id() {
                    Some(id) => id,
                    None => return,
                };

                // If it's a root, then we increment closed.
                if self.roots.remove(span_id).is_some() {
                    self.closed += 1;
                    changed = true;
                } else {
                    self.non_roots.remove(&span_id);
                }
            }
            Instant(instant) => {
                use buck2_data::instant_event::Data::*;

                match instant.data.as_ref() {
                    Some(DiceStateSnapshot(snapshot)) => {
                        self.dice_state.update(snapshot);
                        changed = true;
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        if changed {
            let open = self.roots.len() as u64;
            let pending = pending_estimate(&self.roots, &self.dice_state);

            *self.shared.spans.lock() = SpansSnapshot {
                open,
                closed: self.closed,
                pending,
            };
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

        assert_eq!(
            writer.shared.spans(),
            SpansSnapshot {
                open: 1,
                closed: 0,
                pending: 0
            }
        );

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

        assert_eq!(
            writer.shared.spans(),
            SpansSnapshot {
                open: 1,
                closed: 0,
                pending: 0
            }
        );

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

        assert_eq!(
            writer.shared.spans(),
            SpansSnapshot {
                open: 1,
                closed: 0,
                pending: 0
            }
        );

        writer.peek_event(&BuckEvent::new(
            SystemTime::now(),
            trace.clone(),
            Some(root),
            None,
            buck2_data::SpanEndEvent {
                data: Some(buck2_data::AnalysisEnd::default().into()),
                ..Default::default()
            }
            .into(),
        ));

        assert_eq!(
            writer.shared.spans(),
            SpansSnapshot {
                open: 0,
                closed: 1,
                pending: 0
            }
        );

        writer.peek_event(&BuckEvent::new(
            SystemTime::now(),
            trace,
            None,
            None,
            buck2_data::InstantEvent {
                data: Some(
                    buck2_data::DiceStateSnapshot {
                        key_states: {
                            let mut map = HashMap::new();
                            map.insert(
                                "BuildKey".to_owned(),
                                buck2_data::DiceKeyState {
                                    started: 4,
                                    finished: 2,
                                    check_deps_started: 0,
                                    check_deps_finished: 0,
                                },
                            );
                            map
                        },
                    }
                    .into(),
                ),
            }
            .into(),
        ));

        assert_eq!(
            writer.shared.spans(),
            SpansSnapshot {
                open: 0,
                closed: 1,
                pending: 2
            }
        );
    }
}
