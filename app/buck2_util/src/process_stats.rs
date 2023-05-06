/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::default::Default;

#[derive(Default)]
pub struct ProcessStats {
    pub rss_bytes: Option<u64>,
    pub max_rss_bytes: Option<u64>,
    pub user_cpu_us: Option<u64>,
    pub system_cpu_us: Option<u64>,
}

#[cfg(unix)]
pub fn process_stats() -> ProcessStats {
    use crate::process_stats::proc_self_stat::ProcSelfStat;

    let usage = unsafe {
        let mut usage: libc::rusage = std::mem::zeroed();
        match libc::getrusage(libc::RUSAGE_SELF, &mut usage as *mut _) {
            0 => usage,
            _ => return ProcessStats::default(),
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

    ProcessStats {
        rss_bytes,
        max_rss_bytes: Some((usage.ru_maxrss as u64) * rss_scale),
        user_cpu_us: Some(tv_to_micros(&usage.ru_utime)),
        system_cpu_us: Some(tv_to_micros(&usage.ru_stime)),
    }
}

#[cfg(windows)]
pub fn process_stats() -> ProcessStats {
    use winapi::shared::minwindef::DWORD;
    use winapi::um::processthreadsapi::GetCurrentProcess;
    use winapi::um::psapi::K32GetProcessMemoryInfo;
    use winapi::um::psapi::PROCESS_MEMORY_COUNTERS;

    let mut pmc: PROCESS_MEMORY_COUNTERS = unsafe { std::mem::zeroed() };
    pmc.cb = std::mem::size_of_val(&pmc) as DWORD;
    // Code is referenced from eden/scm/lib/procinfo/src/lib.rs
    // API reference: https://learn.microsoft.com/en-us/windows/win32/api/psapi/ns-psapi-process_memory_counters
    let (wss_bytes, max_wss_bytes) =
        match unsafe { K32GetProcessMemoryInfo(GetCurrentProcess(), &mut pmc, pmc.cb) } {
            0 => (None, None),
            _ => (
                Some(pmc.WorkingSetSize as u64),
                Some(pmc.PeakWorkingSetSize as u64),
            ),
        };

    // Technically, `GetProcessMemoryInfo` returns working set size, not resident set size. However,
    // we log it as rss for two reasons:
    // (1) Reading https://learn.microsoft.com/en-us/windows/win32/memory/working-set, the definition of
    // working set size is "set of pages in the virtual address space of the process that are currently resident
    // in physical memory", which sound just like RSS in unix.
    // (2) We can reuse our existing logging for rss without having duplicate column and logic for wss.
    ProcessStats {
        rss_bytes: wss_bytes,
        max_rss_bytes: max_wss_bytes,
        user_cpu_us: None,
        system_cpu_us: None,
    }
}

#[cfg(not(any(unix, windows)))]
pub fn process_stats() -> ProcessStats {
    ProcessStats::default()
}

pub fn process_cpu_time_us() -> Option<u64> {
    let stats = process_stats();
    if let (Some(user_cpu_us), Some(system_cpu_us)) = (stats.user_cpu_us, stats.system_cpu_us) {
        Some(user_cpu_us + system_cpu_us)
    } else {
        None
    }
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
            // Sometimes tests start too quickly and CPU counters are zero.
            if false {
                assert!(process_stats.user_cpu_us.unwrap() > 0);
                assert!(process_stats.system_cpu_us.unwrap() > 0);
            }
        }
        assert!(process_stats.max_rss_bytes.unwrap() > 0);
        if cfg!(target_os = "linux") || cfg!(target_os = "windows") {
            let rss_bytes = process_stats.rss_bytes.unwrap();
            assert!(rss_bytes > 0);
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
