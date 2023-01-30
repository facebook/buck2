/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context;
use buck2_events::BuckEvent;

use crate::action_stats::ActionStats;
use crate::io_state::IoState;
use crate::re_state::ReState;
use crate::session_info::SessionInfo;
use crate::span_tracker::BuckEventSpanTracker;
use crate::two_snapshots::TwoSnapshots;

pub struct EventObserver {
    span_tracker: BuckEventSpanTracker,
    action_stats: ActionStats,
    re_state: ReState,
    two_snapshots: TwoSnapshots, // NOTE: We got many more copies of this than we should.
    session_info: SessionInfo,
    io_state: IoState,
}

impl EventObserver {
    pub fn new() -> Self {
        Self {
            span_tracker: BuckEventSpanTracker::new(),
            action_stats: ActionStats::default(),
            re_state: ReState::new(),
            two_snapshots: TwoSnapshots::default(),
            session_info: SessionInfo::default(),
            io_state: IoState::default(),
        }
    }

    pub fn observe(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.span_tracker.handle_event(event)?;

        {
            use buck2_data::buck_event::Data::*;

            match event.data() {
                SpanStart(start) => {
                    use buck2_data::span_start_event::Data::*;

                    match start.data.as_ref().context("Missing `data` in SpanStart")? {
                        Command(_command_start) => {
                            self.session_info.trace_id = Some(event.trace_id()?);
                        }
                        _ => {}
                    }
                }
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
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

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
}
