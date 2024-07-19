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

#[derive(Default)]
pub struct TwoSnapshots {
    pub penultimate: Option<(SystemTime, buck2_data::Snapshot)>,
    pub last: Option<(SystemTime, buck2_data::Snapshot)>,
}

impl TwoSnapshots {
    pub fn update(&mut self, timestamp: SystemTime, snapshot: &buck2_data::Snapshot) {
        self.penultimate = mem::replace(&mut self.last, Some((timestamp, snapshot.clone())));
    }

    fn non_zero_duration(&self) -> Option<Duration> {
        let (penultimate_time, _) = self.penultimate.as_ref()?;
        let (last_time, _) = self.last.as_ref()?;
        let duration = last_time.duration_since(*penultimate_time).ok()?;
        if duration.is_zero() {
            return None;
        }
        Some(duration)
    }

    fn per_micro_second(&self, field: impl Fn(&buck2_data::Snapshot) -> u64) -> Option<u64> {
        let (_, penultimate_snapshot) = self.penultimate.as_ref()?;
        let (_, last_snapshot) = self.last.as_ref()?;
        let duration = self.non_zero_duration()?;
        let last_value = field(last_snapshot);
        let penultimate_value = field(penultimate_snapshot);
        let delta_value = last_value.checked_sub(penultimate_value)?;
        Some(delta_value / duration.as_micros() as u64)
    }

    /// User + system CPU time between two snapshots in percents.
    pub fn cpu_percents(&self) -> Option<u64> {
        self.per_micro_second(|s| (s.buck2_user_cpu_us + s.buck2_system_cpu_us) * 100)
    }

    /// User CPU time between two snapshots in percents.
    pub fn user_cpu_percents(&self) -> Option<u64> {
        self.per_micro_second(|s| s.buck2_user_cpu_us * 100)
    }

    /// System CPU time between two snapshots in percents.
    pub fn system_cpu_percents(&self) -> Option<u64> {
        self.per_micro_second(|s| s.buck2_system_cpu_us * 100)
    }

    /// Measure bytes-per-second rate between two snapshots for some field.
    fn bytes_per_second(&self, field: impl Fn(&buck2_data::Snapshot) -> u64) -> Option<u64> {
        self.per_micro_second(|s| field(s) * 1_000_000)
    }

    pub fn re_download_bytes_per_second(&self) -> Option<u64> {
        self.bytes_per_second(|snapshot| snapshot.re_download_bytes)
    }

    pub fn re_upload_bytes_per_second(&self) -> Option<u64> {
        self.bytes_per_second(|snapshot| snapshot.re_upload_bytes)
    }

    pub fn http_download_bytes_per_second(&self) -> Option<u64> {
        self.bytes_per_second(|snapshot| snapshot.http_download_bytes)
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Add;
    use std::time::Duration;
    use std::time::SystemTime;

    use super::TwoSnapshots;

    #[test]
    fn test_cpu_percents() {
        let t0 = SystemTime::UNIX_EPOCH.add(Duration::from_secs(100000));

        let mut two_snapshots = TwoSnapshots::default();
        assert_eq!(None, two_snapshots.cpu_percents());
        assert_eq!(None, two_snapshots.user_cpu_percents());
        assert_eq!(None, two_snapshots.system_cpu_percents());
        two_snapshots.update(
            t0,
            &buck2_data::Snapshot {
                buck2_user_cpu_us: 100,
                buck2_system_cpu_us: 200,
                ..Default::default()
            },
        );
        assert_eq!(None, two_snapshots.cpu_percents());
        assert_eq!(None, two_snapshots.user_cpu_percents());
        assert_eq!(None, two_snapshots.system_cpu_percents());
        two_snapshots.update(
            t0.add(Duration::from_secs(2)),
            &buck2_data::Snapshot {
                buck2_user_cpu_us: 6_000_100,
                buck2_system_cpu_us: 8_000_200,
                ..Default::default()
            },
        );
        // 2 seconds real time, 14 seconds user + system time, so 700% CPU.
        assert_eq!(Some(700), two_snapshots.cpu_percents());
        assert_eq!(Some(300), two_snapshots.user_cpu_percents());
        assert_eq!(Some(400), two_snapshots.system_cpu_percents());
    }

    #[test]
    fn test_bytes_per_second() {
        let t0 = SystemTime::UNIX_EPOCH.add(Duration::from_secs(100000));

        let mut two_snapshots = TwoSnapshots::default();
        assert_eq!(None, two_snapshots.re_download_bytes_per_second());
        two_snapshots.update(
            t0,
            &buck2_data::Snapshot {
                re_download_bytes: 100,
                ..Default::default()
            },
        );
        assert_eq!(None, two_snapshots.re_download_bytes_per_second());
        two_snapshots.update(
            t0.add(Duration::from_secs(2)),
            &buck2_data::Snapshot {
                re_download_bytes: 6100,
                ..Default::default()
            },
        );
        assert_eq!(Some(3000), two_snapshots.re_download_bytes_per_second());
    }
}
