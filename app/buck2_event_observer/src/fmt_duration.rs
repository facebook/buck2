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
    let elapsed = elapsed.mul_f64(time_speed);
    let nanos = elapsed.as_nanos().try_into().unwrap_or(u64::MAX);
    let millis = nanos.saturating_add(50_000_000); // Round up.
    let subsec = millis % 1_000_000_000;
    let secs = millis / 1_000_000_000;
    let mins = secs / 60;
    let secs_of_min = secs % 60;
    let hours = mins / 60;
    let mins_of_hour = mins % 60;
    if hours != 0 {
        format!(
            "{}:{:02}:{:02}.{}s",
            hours,
            mins_of_hour,
            secs_of_min,
            subsec / 100_000_000
        )
    } else if mins != 0 {
        format!("{}:{:02}.{}s", mins, secs_of_min, subsec / 100_000_000)
    } else {
        format!("{}.{}s", secs, subsec / 100_000_000)
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use crate::fmt_duration::fmt_duration;

    #[test]
    fn test_fmt_duration() {
        fn hmss(h: u64, m: u64, s: u64, ms: u64) -> Duration {
            Duration::from_millis(h * 3_600_000 + m * 60_000 + s * 1000 + ms)
        }

        assert_eq!("0.0s", fmt_duration(hmss(0, 0, 0, 0), 1.0));
        assert_eq!("0.0s", fmt_duration(hmss(0, 0, 0, 49), 1.0));
        assert_eq!("0.1s", fmt_duration(hmss(0, 0, 0, 50), 1.0));
        assert_eq!("0.1s", fmt_duration(hmss(0, 0, 0, 99), 1.0));
        assert_eq!("0.1s", fmt_duration(hmss(0, 0, 0, 100), 1.0));
        assert_eq!("0.9s", fmt_duration(hmss(0, 0, 0, 949), 1.0));
        assert_eq!("1.0s", fmt_duration(hmss(0, 0, 0, 999), 1.0));
        assert_eq!("1.0s", fmt_duration(hmss(0, 0, 1, 0), 1.0));
        assert_eq!("59.9s", fmt_duration(hmss(0, 0, 59, 949), 1.0));
        assert_eq!("1:00.0s", fmt_duration(hmss(0, 0, 59, 999), 1.0));
        assert_eq!("1:00.0s", fmt_duration(hmss(0, 1, 0, 0), 1.0));
        assert_eq!("59:59.9s", fmt_duration(hmss(0, 59, 59, 949), 1.0));
        assert_eq!("1:00:00.0s", fmt_duration(hmss(0, 59, 59, 950), 1.0));
        assert_eq!("1:00:00.0s", fmt_duration(hmss(1, 0, 0, 0), 1.0));
        assert_eq!("9876:54:32.1s", fmt_duration(hmss(9876, 54, 32, 100), 1.0));
    }
}
