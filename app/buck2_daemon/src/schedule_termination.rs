/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::thread;
use std::time::Duration;

use buck2_core::buck2_env;
use buck2_util::process_stats::process_cpu_time_us;
use buck2_util::threads::thread_spawn;

fn elapsed_cpu_time_as_percents(
    cpu_time_before_us: Option<u64>,
    cpu_time_after_us: Option<u64>,
    duration_seconds: u64,
) -> Option<u64> {
    let cpu_time_before = cpu_time_before_us?;
    let cpu_time_after = cpu_time_after_us?;
    let elapsed_cpu_time_us = cpu_time_after.checked_sub(cpu_time_before)?;
    let elapsed_cpu_time_us_avg_per_second = elapsed_cpu_time_us.checked_div(duration_seconds)?;
    elapsed_cpu_time_us_avg_per_second.checked_div(1_000_000 / 100)
}

/// Our tests sometimes don't exit Buck 2 cleanly, and they might not get an oppportunity to do so
/// if they are terminated. This allows the daemon to self-destruct.
pub(crate) fn maybe_schedule_termination() -> buck2_error::Result<()> {
    if let Some(duration) = buck2_env!("BUCK2_TERMINATE_AFTER", type=u64, applicability=testing)? {
        thread_spawn("buck2-terminate-after", move || {
            const MEASURE_CPU_TIME_FOR: u64 = 10;
            let (sleep_before, sleep_after) = match duration.checked_sub(MEASURE_CPU_TIME_FOR) {
                Some(sleep_before) => (sleep_before, MEASURE_CPU_TIME_FOR),
                None => (0, duration),
            };

            thread::sleep(Duration::from_secs(sleep_before));
            let process_cpu_time_us_before = process_cpu_time_us();
            thread::sleep(Duration::from_secs(sleep_after));
            let process_cpu_time_us_after = process_cpu_time_us();

            let elapsed_cpu_time_avg_in_percents = elapsed_cpu_time_as_percents(
                process_cpu_time_us_before,
                process_cpu_time_us_after,
                sleep_after,
            );
            if let Some(elapsed_cpu_time_avg_in_percents) = elapsed_cpu_time_avg_in_percents {
                panic!(
                    "Buck is exiting after {}s elapsed; avg process CPU in the last {}s is {}%",
                    duration, sleep_after, elapsed_cpu_time_avg_in_percents
                );
            } else {
                panic!("Buck is exiting after {}s elapsed", duration);
            }
        })?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::schedule_termination::elapsed_cpu_time_as_percents;

    #[test]
    fn test_elapsed_cpu_time_as_percents() {
        // 12 seconds wall time
        // 6 seconds of CPU time
        // equivalent to 50% CPU usage
        assert_eq!(
            Some(50),
            elapsed_cpu_time_as_percents(Some(1_000_123), Some(7_000_123), 12)
        );
    }
}
