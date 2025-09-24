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

/// Manages a view of virtual time that is used to display elapsed times in superconsole.
///
/// This primarily exists to allow `log replay` to work reliably and correctly.
#[derive(Debug)]
pub(crate) struct Timekeeper {
    speed: f64,
    current_tick: Tick,
}

const TIMESPEED_DEFAULT: f64 = 1.0;

impl Timekeeper {
    pub(crate) fn new(speed_value: Option<f64>, current_tick: Tick) -> buck2_error::Result<Self> {
        let speed = speed_value.unwrap_or(TIMESPEED_DEFAULT);

        if speed <= 0.0 {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Time speed cannot be negative!"
            ));
        }
        Ok(Timekeeper {
            speed,
            current_tick,
        })
    }

    pub(crate) fn tick(&mut self, current_tick: Tick) {
        self.current_tick = current_tick;
    }

    pub(crate) fn elapsed_since(&self, start: EventTimestamp) -> Duration {
        (self.current_tick.start_time + self.current_tick.elapsed_time)
            .saturating_duration_since(start.0)
            .mul_f64(self.speed)
    }

    pub(crate) fn elapsed_since_command_start(&self) -> Duration {
        self.current_tick.elapsed_time.mul_f64(self.speed)
    }
}
