/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use buck2_events::BuckEvent;
use gazebo::prelude::*;
use gazebo::variants::VariantName;
use superconsole::Component;

use crate::subscribers::subscriber::unpack_event;
use crate::subscribers::subscriber::UnpackedBuckEvent;

static NUM_DELAYS_FOR_AVERAGE: usize = 10;

#[derive(Default)]
struct SpanData {
    started: u64,
    finished: u64,
    total_duration: Duration,
    total_poll_time: Duration,
    // We display this as average max poll time, but easiest to track it as total.
    total_max_poll_time: Duration,
}

#[derive(Default)]
struct InstantData {
    count: u64,
}

pub(crate) struct DebugEventsState {
    pub enabled: bool,
    event_count: u64,
    spans: BTreeMap<String, SpanData>,
    instants: BTreeMap<String, InstantData>,
    /// events we receive have SystemTime timestamps and may be based on a different time than
    /// we are running in. This is used to adjust them to our local Instant-based time.
    events_start_time: Option<SystemTime>,
    recent_delays: VecDeque<Duration>,
    max_delay: Duration,
}

impl DebugEventsState {
    pub(crate) fn new(enabled: bool) -> Self {
        Self {
            enabled,
            event_count: 0,
            spans: BTreeMap::new(),
            instants: BTreeMap::new(),
            events_start_time: None,
            recent_delays: VecDeque::new(),
            max_delay: Duration::ZERO,
        }
    }

    pub(crate) fn handle_event(
        &mut self,
        start_time: Instant,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        let event_time: SystemTime = event.timestamp();
        let elapsed = start_time.elapsed();
        let events_start_time = match self.events_start_time {
            Some(v) => v,
            None => {
                let start_time = event_time - elapsed;
                self.events_start_time = Some(start_time);
                start_time
            }
        };

        self.event_count += 1;
        // event_elapsed represents the time from start_time to when this event was timestamped in the server
        let event_elapsed = event_time.duration_since(events_start_time)?;
        // if events are being delayed, the real elapsed time will be greater than the "event_elapsed" time
        let delay = elapsed.checked_sub(event_elapsed).unwrap_or(Duration::ZERO);

        self.max_delay = std::cmp::max(delay, self.max_delay);
        self.recent_delays.push_back(delay);
        while self.recent_delays.len() > NUM_DELAYS_FOR_AVERAGE {
            self.recent_delays.pop_front();
        }

        match unpack_event(event)? {
            UnpackedBuckEvent::SpanStart(_, _, data) => {
                let name = data.variant_name();

                let entry = {
                    match self.spans.get_mut(name) {
                        Some(v) => v,
                        None => {
                            self.spans.insert(name.to_owned(), SpanData::default());
                            self.spans.get_mut(name).unwrap()
                        }
                    }
                };
                entry.started += 1;
            }
            UnpackedBuckEvent::SpanEnd(_, span_end, data) => {
                // Right now, matching these end events to the start events depends on the field names in the protobufs
                // matching. That works but is fragile, if it breaks at some point we can do the match and explicitly
                // match them ourselves.
                let name = data.variant_name();

                let entry = {
                    match self.spans.get_mut(name) {
                        Some(v) => v,
                        None => {
                            self.spans.insert(name.to_owned(), SpanData::default());
                            self.spans.get_mut(name).unwrap()
                        }
                    }
                };
                entry.finished += 1;
                if let Some(v) = &span_end.stats {
                    entry.total_poll_time += Duration::from_micros(v.max_poll_time_us);
                    entry.total_max_poll_time += Duration::from_micros(v.max_poll_time_us);
                }
            }
            UnpackedBuckEvent::Instant(_, _, data) => {
                let name = data.variant_name();

                let entry = {
                    match self.instants.get_mut(name) {
                        Some(v) => v,
                        None => {
                            self.instants
                                .insert(name.to_owned(), InstantData::default());
                            self.instants.get_mut(name).unwrap()
                        }
                    }
                };

                entry.count += 1;
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct DebugEventsComponent;

impl Component for DebugEventsComponent {
    fn draw_unchecked(
        &self,
        state: &superconsole::State,
        _dimensions: superconsole::Dimensions,
        _mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        let state = state.get::<DebugEventsState>()?;

        if !state.enabled {
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
