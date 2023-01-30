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
use gazebo::variants::VariantName;

use crate::unpack_event::unpack_event;
use crate::unpack_event::UnpackedBuckEvent;

static NUM_DELAYS_FOR_AVERAGE: usize = 10;

#[derive(Default)]
pub struct SpanData {
    pub started: u64,
    pub finished: u64,
    pub total_duration: Duration,
    pub total_poll_time: Duration,
    // We display this as average max poll time, but easiest to track it as total.
    pub total_max_poll_time: Duration,
}

#[derive(Default)]
pub struct InstantData {
    pub count: u64,
}

pub struct DebugEventsState {
    pub event_count: u64,
    pub spans: BTreeMap<String, SpanData>,
    pub instants: BTreeMap<String, InstantData>,
    /// events we receive have SystemTime timestamps and may be based on a different time than
    /// we are running in. This is used to adjust them to our local Instant-based time.
    pub events_start_time: Option<SystemTime>,
    pub recent_delays: VecDeque<Duration>,
    pub max_delay: Duration,
}

impl DebugEventsState {
    pub fn new() -> Self {
        Self {
            event_count: 0,
            spans: BTreeMap::new(),
            instants: BTreeMap::new(),
            events_start_time: None,
            recent_delays: VecDeque::new(),
            max_delay: Duration::ZERO,
        }
    }

    pub fn handle_event(&mut self, start_time: Instant, event: &BuckEvent) -> anyhow::Result<()> {
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
