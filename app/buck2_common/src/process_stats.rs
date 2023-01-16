/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub struct ProcessStats {
    pub rss_bytes: Option<u64>,
    pub max_rss_bytes: u64,
    pub user_cpu_us: u64,
    pub system_cpu_us: u64,
}

#[cfg(unix)]
pub fn process_stats() -> Option<ProcessStats> {
    use crate::process_stats::proc_self_stat::ProcSelfStat;

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

    let rss_bytes = if cfg!(target_os = "linux") {
        // Buck2 snapshot is made once per second, so this shouldn't be too expensive.
        ProcSelfStat::read().map(|stat| {
            // `getconf PAGESIZE`, but practically it's always 4096.
            stat.rss * 4096
        })
    } else {
        None
    };

    Some(ProcessStats {
        rss_bytes,
        max_rss_bytes: (usage.ru_maxrss as u64) * rss_scale,
        user_cpu_us: tv_to_micros(&usage.ru_utime),
        system_cpu_us: tv_to_micros(&usage.ru_stime),
    })
}

#[cfg(not(unix))]
pub fn process_stats() -> Option<ProcessStats> {
    None
}

pub fn process_cpu_time_us() -> Option<u64> {
    process_stats().map(|s| s.user_cpu_us + s.system_cpu_us)
}

#[cfg_attr(not(unix), allow(dead_code))]
mod proc_self_stat {
    use std::fs;

    /// Parsed `/proc/self/stat` file.
    pub struct ProcSelfStat {
        /// Resident Set Size: number of pages the process has in real memory.
        /// Raw value.
        pub rss: u64,
    }

    impl ProcSelfStat {
        pub fn parse(stat: &str) -> Option<ProcSelfStat> {
            let rss = stat.split(' ').nth(23)?.parse().ok()?;
            Some(ProcSelfStat { rss })
        }

        pub fn read() -> Option<ProcSelfStat> {
            fs::read_to_string("/proc/self/stat")
                .ok()
                .and_then(|s| ProcSelfStat::parse(&s))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::process_stats::proc_self_stat::ProcSelfStat;
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
            if cfg!(target_os = "linux") {
                let rss_bytes = process_stats.rss_bytes.unwrap();
                assert!(rss_bytes > 0);
            }
        }
    }

    #[test]
    fn test_proc_self_stat_parse() {
        let stat = "1736324 (cat) R 53088 1736324 53088 34816 1736324 4194304 113 \
            0 0 0 0 0 0 0 20 0 1 0 \
            2018135 222441472 215 \
            18446744073709551615 94565082071040 94565082102344 140727456826704 \
            0 0 0 0 0 0 0 0 0 17 11 0 0 0 0 0 \
            94565084199504 94565084201152 94565084205056 140727456831309 \
            140727456831329 140727456831329 140727456833519 0";
        assert_eq!(215, ProcSelfStat::parse(stat).unwrap().rss);
    }

    #[test]
    fn test_proc_self_stat_read() {
        if cfg!(target_os = "linux") {
            let stat = ProcSelfStat::read().unwrap();
            assert!(stat.rss > 0);
        }
    }
}
