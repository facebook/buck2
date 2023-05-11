/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

pub fn fmt_duration(elapsed: Duration, time_speed: f64) -> String {
    format!("{:.1}s", elapsed.mul_f64(time_speed).as_secs_f64())
}
