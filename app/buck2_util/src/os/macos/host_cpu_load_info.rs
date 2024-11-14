/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(target_os = "macos")]

use buck2_error::buck2_error;

use crate::os::macos::mach_error_string::mach_error_string;

/// Query `HOST_CPU_LOAD_INFO`. This is low-level API.
pub fn host_cpu_load_info() -> buck2_error::Result<buck2_data::HostCpuLoadInfo> {
    unsafe {
        let mut count: libc::mach_msg_type_number_t = libc::HOST_CPU_LOAD_INFO_COUNT;

        // https://github.com/rust-lang/libc/pull/3916
        #[repr(C)]
        struct host_cpu_load_info {
            cpu_ticks: [libc::natural_t; libc::CPU_STATE_MAX as usize],
        }

        let mut host_info = host_cpu_load_info {
            cpu_ticks: [0; libc::CPU_STATE_MAX as usize],
        };

        let res = libc::host_statistics64(
            libc::mach_host_self(),
            libc::HOST_CPU_LOAD_INFO,
            &mut host_info as *mut _ as *mut libc::integer_t,
            &mut count,
        );
        if res != libc::KERN_SUCCESS {
            return Err(buck2_error!(
                [],
                "host_statistics64 failed: {}",
                mach_error_string(res)
            ));
        }

        Ok(buck2_data::HostCpuLoadInfo {
            user: host_info.cpu_ticks[libc::CPU_STATE_USER as usize],
            system: host_info.cpu_ticks[libc::CPU_STATE_SYSTEM as usize],
            idle: host_info.cpu_ticks[libc::CPU_STATE_IDLE as usize],
            nice: host_info.cpu_ticks[libc::CPU_STATE_NICE as usize],
        })
    }
}

#[cfg(test)]
mod tests {
    use std::thread;
    use std::time::Duration;

    use crate::os::macos::host_cpu_load_info::host_cpu_load_info;

    #[test]
    fn test_host_cpu_load_info() {
        let x = host_cpu_load_info().unwrap();
        thread::sleep(Duration::from_millis(10));
        let y = host_cpu_load_info().unwrap();
        assert!(x.user <= y.user);
        assert!(x.system <= y.system);
        assert!(x.idle <= y.idle);
        assert!(x.nice <= y.nice);
        let sum_x = x.user as u64 + x.system as u64 + x.idle as u64 + x.nice as u64;
        let sum_y = y.user as u64 + y.system as u64 + y.idle as u64 + y.nice as u64;

        let delta = sum_y.wrapping_sub(sum_x) as i64;

        // 10 CPUs for 100 seconds at 100 ticks per second.
        assert!(delta < 100_000, "{:?} <=> {:?}", x, y);

        // This test fails if comparison is `>` instead of `>=`
        // unless sleep time is 1s or more.
        // `host_cpu_load_info` seems to be updated every 1s.
        assert!(delta >= 0, "{:?} <=> {:?}", x, y);
    }
}
