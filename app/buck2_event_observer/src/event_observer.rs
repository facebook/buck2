/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Instant;

use anyhow::Context;
use buck2_events::BuckEvent;

use crate::action_stats::ActionStats;
use crate::debug_events::DebugEventsState;
use crate::dice_state::DiceState;
use crate::io_state::IoState;
use crate::re_state::ReState;
use crate::session_info::SessionInfo;
use crate::span_tracker::BuckEventSpanTracker;
use crate::test_state::TestState;
use crate::two_snapshots::TwoSnapshots;

pub struct EventObserver<E> {
    span_tracker: BuckEventSpanTracker,
    action_stats: ActionStats,
    re_state: ReState,
    two_snapshots: TwoSnapshots, // NOTE: We got many more copies of this than we should.
    session_info: SessionInfo,
    io_state: IoState,
    test_state: TestState,
    /// When running without the Superconsole, we skip some state that we don't need. This might be
    /// premature optimization.
    extra: E,
}

impl<E> EventObserver<E>
where
    E: EventObserverExtra,
{
    pub fn new() -> Self {
        Self {
            span_tracker: BuckEventSpanTracker::new(),
            action_stats: ActionStats::default(),
            re_state: ReState::new(),
            two_snapshots: TwoSnapshots::default(),
            session_info: SessionInfo::default(),
            io_state: IoState::default(),
            test_state: TestState::default(),
            extra: E::new(),
        }
    }

    pub fn observe(&mut self, receive_time: Instant, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.span_tracker.handle_event(event)?;
        if self.session_info.trace_id.is_none() {
            self.session_info.trace_id = Some(event.trace_id()?);
        }

        {
            use buck2_data::buck_event::Data::*;

            match event.data() {
                SpanEnd(end) => {
                    use buck2_data::span_end_event::Data::*;

                    match end.data.as_ref().context("Missing `data` in SpanEnd")? {
                        ActionExecution(action_execution_end) => {
                            self.action_stats.update(action_execution_end);
                        }
                        _ => {}
                    }
                }
                Instant(instant) => {
                    use buck2_data::instant_event::Data::*;

                    match instant
                        .data
                        .as_ref()
                        .context("Missing `data` in `Instant`")?
                    {
                        ReSession(re_session) => {
                            self.re_state.add_re_session(re_session);
                        }
                        Snapshot(snapshot) => {
                            self.re_state.update(event.timestamp(), snapshot);
                            self.two_snapshots.update(event.timestamp(), snapshot);
                            self.io_state.update(event.timestamp(), snapshot);
                        }
                        TestDiscovery(discovery) => {
                            use buck2_data::test_discovery::Data::*;

                            match discovery
                                .data
                                .as_ref()
                                .context("Missing `data` in `TestDiscovery`")?
                            {
                                Session(session) => {
                                    self.session_info.test_session = Some(session.clone());
                                }
                                Tests(tests) => {
                                    self.test_state.discovered += tests.test_names.len() as u64
                                }
                            }
                        }
                        TestResult(result) => {
                            self.test_state.update(result)?;
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        self.extra.observe(receive_time, event)?;

        Ok(())
    }

    pub fn spans(&self) -> &BuckEventSpanTracker {
        &self.span_tracker
    }

    pub fn action_stats(&self) -> &ActionStats {
        &self.action_stats
    }

    pub fn re_state(&self) -> &ReState {
        &self.re_state
    }

    pub fn two_snapshots(&self) -> &TwoSnapshots {
        &self.two_snapshots
    }

    pub fn session_info(&self) -> &SessionInfo {
        &self.session_info
    }

    pub fn io_state(&self) -> &IoState {
        &self.io_state
    }

    pub fn test_state(&self) -> &TestState {
        &self.test_state
    }

    pub fn extra(&self) -> &E {
        &self.extra
    }
}

pub trait EventObserverExtra: Send {
    fn new() -> Self;

    fn observe(&mut self, receive_time: Instant, event: &Arc<BuckEvent>) -> anyhow::Result<()>;
}

/// This has more fields for debug info. We don't always capture those.
pub struct DebugEventObserverExtra {
    dice_state: DiceState,
    debug_events: DebugEventsState,
}

impl EventObserverExtra for DebugEventObserverExtra {
    fn new() -> Self {
        Self {
            dice_state: DiceState::new(),
            debug_events: DebugEventsState::new(),
        }
    }

    fn observe(&mut self, receive_time: Instant, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.debug_events.handle_event(receive_time, event)?;

        {
            use buck2_data::buck_event::Data::*;

            match event.data() {
                Instant(instant) => {
                    use buck2_data::instant_event::Data::*;

                    match instant
                        .data
                        .as_ref()
                        .context("Missing `data` in `Instant`")?
                    {
                        DiceStateSnapshot(dice) => {
                            self.dice_state.update(dice);
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }
}

impl DebugEventObserverExtra {
    pub fn dice_state(&self) -> &DiceState {
        &self.dice_state
    }

    pub fn debug_events(&self) -> &DebugEventsState {
        &self.debug_events
    }
}

pub struct NoopEventObserverExtra;

impl EventObserverExtra for NoopEventObserverExtra {
    fn new() -> Self {
        Self
    }

    fn observe(&mut self, _receive_time: Instant, _event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        // Noop
        Ok(())
    }
}
