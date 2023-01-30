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
use crate::re_state::ReState;
use crate::span_tracker::BuckEventSpanTracker;

pub struct EventObserver {
    span_tracker: BuckEventSpanTracker,
    action_stats: ActionStats,
    re_state: ReState,
}

impl EventObserver {
    pub fn new() -> Self {
        Self {
            span_tracker: BuckEventSpanTracker::new(),
            action_stats: ActionStats::default(),
            re_state: ReState::new(),
        }
    }

    pub fn observe(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.span_tracker.handle_event(event)?;

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
}
