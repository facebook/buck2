/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;

use buck2_event_observer::span_tracker::EventTimestamp;

use crate::ticker::Tick;

pub trait Clock: Send + Sync {
    fn event_timestamp_for_tick(&mut self, tick: Tick) -> EventTimestamp;

    fn elapsed_since_command_start(&mut self, tick: Tick) -> Duration;
}

pub struct RealtimeClock;

impl Clock for RealtimeClock {
    fn event_timestamp_for_tick(&mut self, tick: Tick) -> EventTimestamp {
        EventTimestamp(tick.current_realtime.into())
    }

    fn elapsed_since_command_start(&mut self, tick: Tick) -> Duration {
        tick.elapsed_time
    }
}

/// Manages a view of virtual time that is used to display elapsed times in superconsole.
///
/// This primarily exists to allow `log replay` to work reliably and correctly.
pub(crate) struct Timekeeper {
    clock: Box<dyn Clock>,
    event_timestamp_for_last_tick: EventTimestamp,
    elapsed_since_command_start_for_last_tick: Duration,
}

impl Timekeeper {
    pub(crate) fn new(mut clock: Box<dyn Clock>, current_tick: Tick) -> buck2_error::Result<Self> {
        Ok(Timekeeper {
            event_timestamp_for_last_tick: clock.event_timestamp_for_tick(current_tick),
            elapsed_since_command_start_for_last_tick: clock
                .elapsed_since_command_start(current_tick),
            clock,
        })
    }

    pub(crate) fn tick(&mut self, current_tick: Tick) {
        self.event_timestamp_for_last_tick = self.clock.event_timestamp_for_tick(current_tick);
        self.elapsed_since_command_start_for_last_tick =
            self.clock.elapsed_since_command_start(current_tick);
    }

    pub(crate) fn elapsed_since(&self, start: EventTimestamp) -> Duration {
        duration_between_timestamps(start.0, self.event_timestamp_for_last_tick.0)
    }

    pub(crate) fn elapsed_since_command_start(&self) -> Duration {
        self.elapsed_since_command_start_for_last_tick
    }
}

pub fn duration_between_timestamps(
    start: prost_types::Timestamp,
    end: prost_types::Timestamp,
) -> Duration {
    let mut diff_secs = end.seconds - start.seconds;
    let mut diff_nanos = end.nanos - start.nanos;
    if diff_nanos < 0 {
        diff_nanos += 1_000_000_000;
        diff_secs -= 1;
    }
    // Guaranteed positive by the above check
    let diff_nanos = diff_nanos as u32;
    if diff_secs < 0 {
        // Duration went backwards, saturate to zero
        Duration::ZERO
    } else {
        Duration::new(diff_secs as u64, diff_nanos)
    }
}
