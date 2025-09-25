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

use buck2_error::buck2_error;
use buck2_event_observer::span_tracker::EventTimestamp;

use crate::ticker::Tick;

pub trait Clock: Send + Sync {
    fn event_timestamp_for_tick(&mut self, tick: Tick) -> EventTimestamp;

    fn elapsed_since_command_start(&mut self, tick: Tick) -> Duration;
}

pub struct RealtimeClock;

impl Clock for RealtimeClock {
    fn event_timestamp_for_tick(&mut self, tick: Tick) -> EventTimestamp {
        EventTimestamp(tick.start_time + tick.elapsed_time)
    }

    fn elapsed_since_command_start(&mut self, tick: Tick) -> Duration {
        tick.elapsed_time
    }
}

/// Manages a view of virtual time that is used to display elapsed times in superconsole.
///
/// This primarily exists to allow `log replay` to work reliably and correctly.
pub(crate) struct Timekeeper {
    speed: f64,
    clock: Box<dyn Clock>,
    event_timestamp_for_last_tick: EventTimestamp,
    elapsed_since_command_start_for_last_tick: Duration,
}

const TIMESPEED_DEFAULT: f64 = 1.0;

impl Timekeeper {
    pub(crate) fn new(
        mut clock: Box<dyn Clock>,
        speed_value: Option<f64>,
        current_tick: Tick,
    ) -> buck2_error::Result<Self> {
        let speed = speed_value.unwrap_or(TIMESPEED_DEFAULT);

        if speed <= 0.0 {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Time speed cannot be negative!"
            ));
        }
        Ok(Timekeeper {
            speed,
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
        self.event_timestamp_for_last_tick
            .0
            .checked_duration_since(start.0)
            .unwrap_or(Duration::ZERO)
            .mul_f64(self.speed)
    }

    pub(crate) fn elapsed_since_command_start(&self) -> Duration {
        self.elapsed_since_command_start_for_last_tick
            .mul_f64(self.speed)
    }
}
