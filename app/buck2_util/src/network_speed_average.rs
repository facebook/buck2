/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;
use std::time::Duration;
use std::time::SystemTime;

const MICROS_IN_SEC: u64 = 1_000_000;

struct Snapshot {
    timestamp: SystemTime,
    value: u64,
}

#[derive(Default)]
pub struct NetworkSpeedAverage {
    total_duration: Duration,
    total_value: u64,
    last_snapshot: Option<Snapshot>,
}

/// Maintains an average in ascending sequence of values ignoring repeating values.
/// The reason is If we take all bytes downloaded over a build time it won't show a correct number.
/// We need to calculate a number of bytes over time an artefacts has been downloaded.
impl NetworkSpeedAverage {
    /// Report the value at a new timestamp
    ///
    /// Each update value must be greater than or equal to the previous one.
    /// Equal values are ignored during average calculation.
    pub fn update(&mut self, timestamp: SystemTime, value: u64) {
        let last = mem::replace(&mut self.last_snapshot, Some(Snapshot { timestamp, value }));
        if let Some((duration, value)) =
            NetworkSpeedAverage::elapsed_if_value_changed(&last, &Snapshot { timestamp, value })
        {
            self.total_duration += duration;
            self.total_value += value;
        }
    }

    pub fn avg_per_second(&self) -> Option<u64> {
        let micros = self.total_duration.as_micros();
        if micros == 0 {
            return None;
        }
        Some(self.total_value * MICROS_IN_SEC / micros as u64)
    }

    fn elapsed_if_value_changed(
        last: &Option<Snapshot>,
        current: &Snapshot,
    ) -> Option<(Duration, u64)> {
        let last = last.as_ref()?;
        let duration = current.timestamp.duration_since(last.timestamp).ok()?;
        if duration.is_zero() {
            return None;
        }
        let diff = current.value - last.value;
        if diff == 0 {
            return None;
        }
        Some((duration, diff))
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Add;
    use std::time::Duration;
    use std::time::SystemTime;

    use super::NetworkSpeedAverage;

    #[test]
    fn test_network_speed_average() {
        let t0 = SystemTime::UNIX_EPOCH.add(Duration::from_secs(100000));

        let mut avg = NetworkSpeedAverage::default();
        assert_eq!(None, avg.avg_per_second());

        avg.update(t0, 100);
        assert_eq!(None, avg.avg_per_second());

        avg.update(t0.add(Duration::from_secs(1)), 200);
        assert_eq!(Some(100), avg.avg_per_second());

        avg.update(t0.add(Duration::from_secs(2)), 400);
        assert_eq!(Some(150), avg.avg_per_second());

        // if value hasn't changed then update should be no op
        avg.update(t0.add(Duration::from_secs(3)), 400);
        assert_eq!(Some(150), avg.avg_per_second());

        avg.update(t0.add(Duration::from_secs(4)), 700);
        assert_eq!(Some(200), avg.avg_per_second());
    }
}
