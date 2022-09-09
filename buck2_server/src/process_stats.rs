/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub(crate) struct ProcessStats {
    pub(crate) max_rss_bytes: u64,
    pub(crate) user_cpu_us: u64,
    pub(crate) system_cpu_us: u64,
}

#[cfg(unix)]
pub(crate) fn process_stats() -> Option<ProcessStats> {
    let usage = unsafe {
        let mut usage: libc::rusage = std::mem::zeroed();
        match libc::getrusage(libc::RUSAGE_SELF, &mut usage as *mut _) {
            0 => usage,
            _ => return None,
        }
    };
    // POSIX didn't specify unit of ru_maxrss. Linux uses KB while BSD and
    // OSX use bytes (despite their manpages might say differently).
    let rss_scale = if cfg!(target_os = "linux") {
        1024
    } else {
        // Assume BSD-ish
        1
    };

    fn tv_to_micros(tv: &libc::timeval) -> u64 {
        (1_000_000 * tv.tv_sec as u64) + (tv.tv_usec as u64)
    }

    Some(ProcessStats {
        max_rss_bytes: (usage.ru_maxrss as u64) * rss_scale,
        user_cpu_us: tv_to_micros(&usage.ru_utime),
        system_cpu_us: tv_to_micros(&usage.ru_stime),
    })
}

#[cfg(not(unix))]
pub(crate) fn process_stats() -> Option<ProcessStats> {
    None
}

#[cfg(test)]
mod tests {
    use crate::process_stats::process_stats;

    #[test]
    fn test_process_stats() {
        let process_stats = process_stats();
        if cfg!(unix) {
            let process_stats = process_stats.expect("process_stats() should return Some");
            // Sometimes tests start too quickly and CPU counters are zero.
            if false {
                assert!(process_stats.user_cpu_us > 0);
                assert!(process_stats.system_cpu_us > 0);
            }
            assert!(process_stats.max_rss_bytes > 0);
        }
    }
}
