/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_error::buck2_error;
use dupe::Dupe;

#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) struct TimeSpeed {
    speed: f64,
}

const TIMESPEED_DEFAULT: f64 = 1.0;

impl TimeSpeed {
    pub(crate) fn new(speed_value: Option<f64>) -> buck2_error::Result<Self> {
        let speed = speed_value.unwrap_or(TIMESPEED_DEFAULT);

        if speed <= 0.0 {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Time speed cannot be negative!"
            ));
        }
        Ok(TimeSpeed { speed })
    }

    pub(crate) fn speed(self) -> f64 {
        self.speed
    }
}
