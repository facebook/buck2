/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;
use std::time::Instant;

use buck2_common::process_stats::process_cpu_time_us;

pub(crate) struct ClientCpuTracker {
    last_instant: Instant,
    last_cpu_us: Option<u64>,
}

impl ClientCpuTracker {
    pub(crate) fn new() -> ClientCpuTracker {
        ClientCpuTracker {
            last_instant: Instant::now(),
            last_cpu_us: process_cpu_time_us(),
        }
    }

    fn compute(
        prev_instant: Instant,
        prev_cpu_time_us: Option<u64>,
        new_instant: Instant,
        new_cpu_time_us: Option<u64>,
    ) -> Option<u32> {
        let prev_cpu_time_us = prev_cpu_time_us?;
        let new_cpu_time_us = new_cpu_time_us?;
        let duration = new_instant.checked_duration_since(prev_instant)?;
        let duration_us = u64::try_from(duration.as_micros()).ok()?;
        if duration_us == 0 {
            return None;
        }
        let cpu_time_since_prev_us = new_cpu_time_us.checked_sub(prev_cpu_time_us)?;
        u32::try_from(
            cpu_time_since_prev_us
                .checked_mul(100)?
                .checked_div(duration_us)?,
        )
        .ok()
    }

    // CPU time percents since last tick (can be more than 100% if there are multiple cores).
    pub(crate) fn tick_cpu_time_percents(&mut self) -> Option<u32> {
        let prev_cpu_us = mem::replace(&mut self.last_cpu_us, process_cpu_time_us());
        let prev_instant = mem::replace(&mut self.last_instant, Instant::now());
        Self::compute(
            prev_instant,
            prev_cpu_us,
            self.last_instant,
            self.last_cpu_us,
        )
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;
    use std::time::Instant;

    use crate::client_cpu_tracker::ClientCpuTracker;

    #[test]
    fn test_compute() {
        let start = Instant::now();
        let now = start + Duration::from_secs(3);
        assert_eq!(
            Some(20),
            ClientCpuTracker::compute(start, Some(1_000_000_000), now, Some(1_000_600_000))
        );
    }
}
