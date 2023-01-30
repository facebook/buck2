/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use buck2_event_observer::debug_events::DebugEventsState;
use gazebo::prelude::*;
use superconsole::Component;

use crate::subscribers::superconsole::SuperConsoleConfig;

#[derive(Debug)]
pub(crate) struct DebugEventsComponent;

impl Component for DebugEventsComponent {
    fn draw_unchecked(
        &self,
        state: &superconsole::State,
        _dimensions: superconsole::Dimensions,
        _mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        let config = state.get::<SuperConsoleConfig>()?;
        let state = state.get::<DebugEventsState>()?;

        if !config.enable_debug_events {
            return Ok(vec![]);
        }

        let mut lines: Vec<String> = Vec::new();
        lines.push(format!(
            "Events...  total: {} maximum delay: {:.3}ms average delay last {}: {:.3}ms",
            state.event_count,
            state.max_delay.as_secs_f64() * 1000.0,
            state.recent_delays.len(),
            if state.recent_delays.is_empty() {
                0.0
            } else {
                state.recent_delays.iter().sum::<Duration>().as_secs_f64()
                    / (state.recent_delays.len() as f64)
                    * 1000.0
            }
        ));

        if !state.spans.is_empty() {
            let header_line = format!(
                "  {:<32}  {:>10}  {:>10}  {:>9}  {:>13}  {:>14}",
                "Span Events", "started", "finished", "duration", "poll time", "avg max poll time"
            );
            let header_len = header_line.len();
            lines.push(header_line);
            lines.push("-".repeat(header_len));
            for (k, v) in state.spans.iter() {
                lines.push(format!(
                    "    {:<30} |{:>10} |{:>10} |{:8.3}s |{:>12.3}s |{:>12}us",
                    k,
                    v.started,
                    v.finished,
                    v.total_duration.as_secs_f64(),
                    v.total_poll_time.as_secs_f64(),
                    if v.finished == 0 {
                        0
                    } else {
                        v.total_max_poll_time.as_micros() / (v.finished as u128)
                    },
                ));
            }
            lines.push("-".repeat(header_len));
        }

        if !state.instants.is_empty() {
            let header_line = format!("  {:<32}  {:>12}", "Instant Events", "count");
            let header_len = header_line.len();
            lines.push(header_line);
            lines.push("-".repeat(header_len));
            for (k, v) in state.instants.iter() {
                lines.push(format!("    {:<30} |{:>12}", k, v.count));
            }
            lines.push("-".repeat(header_len));
        }

        lines.into_try_map(|v| vec![v].try_into())
    }
}
