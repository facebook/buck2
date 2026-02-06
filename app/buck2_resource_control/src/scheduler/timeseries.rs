/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::VecDeque;
use std::time::Duration;
use std::time::Instant;

// Type alias in case we want to change this at some point
type Value = f64;

/// A timeseries of f64s.
pub(crate) struct Timeseries {
    /// The points in the timeseries.
    ///
    /// Ordered from oldest to newest. Invariant: non-empty.
    points: VecDeque<(Instant, Value)>,
    /// How far into the past we keep data
    window_size: Duration,
}

impl Timeseries {
    pub(crate) fn new(window_size: Duration, init_time: Instant, init_point: Value) -> Self {
        Self {
            points: VecDeque::from([(init_time, init_point)]),
            window_size,
        }
    }

    pub(crate) fn add_sample(&mut self, timestamp: Instant, value: Value) {
        assert!(
            self.points.back().unwrap().0 <= timestamp,
            "Non-monotonic time"
        );
        self.points.push_back((timestamp, value));

        while timestamp - self.points.front().unwrap().0 > self.window_size {
            self.points.pop_front();
        }
    }

    /// Returns the average value of the timeseries over some timespan ending at the last data point
    /// and having length the given duration.
    pub(crate) fn average_over_last(&self, duration: Duration) -> Value {
        if duration.is_zero() {
            // We maybe don't expect this but lets just be safe and return a reasonable value
            return self.points.back().unwrap().1;
        }

        let start_time = self.points.back().unwrap().0 - duration;

        let mut total = 0.0;
        let mut segment_end_idx = self.points.len() - 1;
        while segment_end_idx > 0 && self.points[segment_end_idx - 1].0 >= start_time {
            // The entire segment is included in our interval
            let segment_start = self.points[segment_end_idx - 1];
            let segment_end = self.points[segment_end_idx];
            let segment_duration = segment_end.0 - segment_start.0;
            total += segment_duration.as_secs_f64() * (segment_end.1 + segment_start.1) / 2.0;
            segment_end_idx -= 1;
        }
        let first_sample_in_range = self.points[segment_end_idx];
        total += (first_sample_in_range.0 - start_time).as_secs_f64() * first_sample_in_range.1;
        total / duration.as_secs_f64()
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;
    use std::time::Instant;

    use crate::scheduler::timeseries::Timeseries;

    struct TestTime(Instant);

    impl TestTime {
        fn new() -> Self {
            Self(Instant::now())
        }

        fn at(&self, t: u64) -> Instant {
            self.0 + Duration::from_secs(t)
        }
    }

    macro_rules! assert_float_eq {
        ($actual:expr, $expected:expr) => {{
            let actual: f64 = $actual;
            let expected: f64 = $expected;
            let rel_diff = (actual - expected).abs() / expected.abs();
            assert!(rel_diff < 1e-5, "{} != {}", actual, expected)
        }};
    }

    #[test]
    fn test_eviction_length_one() {
        let t = TestTime::new();
        let mut ts = Timeseries::new(Duration::from_secs(10), t.at(0), 0.0);

        ts.add_sample(t.at(11), 1.0);
        // Check that the timeseries doesn't know about the 0 anymore
        assert_float_eq!(ts.average_over_last(Duration::from_secs(100)), 1.0);
    }

    #[test]
    fn test_eviction_multi() {
        let t = TestTime::new();
        let mut ts = Timeseries::new(Duration::from_secs(10), t.at(0), 0.0);
        ts.add_sample(t.at(1), 0.0);
        ts.add_sample(t.at(2), 0.0);
        ts.add_sample(t.at(3), 0.0);

        ts.add_sample(t.at(15), 1.0);
        // Check that the timeseries doesn't know about the 0s anymore
        assert_float_eq!(ts.average_over_last(Duration::from_secs(100)), 1.0);
    }

    #[test]
    fn test_average_length_one() {
        let t = TestTime::new();
        let ts = Timeseries::new(Duration::from_secs(10), t.at(0), 134.0);
        assert_float_eq!(ts.average_over_last(Duration::from_secs(10)), 134.0);
    }

    #[test]
    fn test_average_off_by_ones() {
        let t = TestTime::new();
        let mut ts = Timeseries::new(Duration::from_secs(1000), t.at(0), 0.0);
        for i in 1..=10 {
            ts.add_sample(t.at(i), i as f64);
        }

        // This exercises a pretty wide variety of potential edge cases
        for i in 1..=10 {
            assert_float_eq!(
                ts.average_over_last(Duration::from_secs(i)),
                10.0 - (i as f64) / 2.0
            );
        }
    }

    #[test]
    fn test_average_duration_zero() {
        let t = TestTime::new();
        let ts = Timeseries::new(Duration::from_secs(1000), t.at(0), 100.0);
        assert_float_eq!(ts.average_over_last(Duration::ZERO), 100.0);
    }

    #[test]
    fn test_average_between_points() {
        let t = TestTime::new();
        let mut ts = Timeseries::new(Duration::from_secs(1000), t.at(0), 1.0);
        ts.add_sample(t.at(2), 2.0);

        // Obviously maybe not 100% accurate, could do linear interpolation to get a bit more precise
        assert_float_eq!(ts.average_over_last(Duration::from_secs(1)), 2.0);
        assert_float_eq!(ts.average_over_last(Duration::from_secs(3)), 4.0 / 3.0);
    }
}
