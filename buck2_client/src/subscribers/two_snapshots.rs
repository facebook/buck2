/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;
use std::time::SystemTime;

#[derive(Default)]
pub(crate) struct TwoSnapshots {
    pub(crate) penultimate: Option<(SystemTime, buck2_data::Snapshot)>,
    pub(crate) last: Option<(SystemTime, buck2_data::Snapshot)>,
}

impl TwoSnapshots {
    pub(crate) fn update(&mut self, timestamp: SystemTime, snapshot: &buck2_data::Snapshot) {
        self.penultimate = mem::replace(&mut self.last, Some((timestamp, snapshot.clone())));
    }

    /// User + system CPU time between two snapshots in percents.
    pub(crate) fn cpu_percents(&self) -> Option<u32> {
        let (penultimate_time, penultimate_snapshot) = self.penultimate.as_ref()?;
        let (last_time, last_snapshot) = self.last.as_ref()?;
        let duration = last_time.duration_since(*penultimate_time).ok()?;
        if duration.is_zero() {
            return None;
        }
        let last_cpu_us = last_snapshot.buck2_user_cpu_us + last_snapshot.buck2_system_cpu_us;
        let penultimate_cpu_us =
            penultimate_snapshot.buck2_user_cpu_us + penultimate_snapshot.buck2_system_cpu_us;
        if penultimate_cpu_us > 0 && last_cpu_us >= penultimate_cpu_us {
            let cpu_us = last_cpu_us - penultimate_cpu_us;
            Some((cpu_us * 100 / (duration.as_micros() as u64)) as u32)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Add;
    use std::time::Duration;
    use std::time::SystemTime;

    use crate::subscribers::two_snapshots::TwoSnapshots;

    #[test]
    fn test_cpu_percents() {
        let t0 = SystemTime::UNIX_EPOCH.add(Duration::from_secs(100000));

        let mut two_snapshots = TwoSnapshots::default();
        assert_eq!(None, two_snapshots.cpu_percents());
        two_snapshots.update(
            t0,
            &buck2_data::Snapshot {
                buck2_user_cpu_us: 100,
                buck2_system_cpu_us: 200,
                ..Default::default()
            },
        );
        assert_eq!(None, two_snapshots.cpu_percents());
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
    }
}
