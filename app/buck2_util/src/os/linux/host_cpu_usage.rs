/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(target_os = "linux")]

use std::fs;

use buck2_error::buck2_error;
use buck2_error::BuckErrorContext;

use crate::os::host_cpu_usage::HostCpuUsage;

/// Reads CPU ticks from /proc/stat.
pub fn host_cpu_usage() -> buck2_error::Result<HostCpuUsage> {
    // https://www.linuxhowtos.org/manpages/5/proc.htm describes the format of /proc/stat.
    // The first line is "cpu ..." which is the aggregated view for all cores.
    let contents = fs::read_to_string("/proc/stat")?;
    let line = contents
        .lines()
        .next()
        .buck_error_context("Failed to read /proc/stat")?;

    let mut line = line.split_whitespace();
    // Expected values at indices:
    // [0]: String literal "cpu", [1]: user time, [2]: nice time, [3]: system time
    if line.next() == Some("cpu") {
        let user_millis_str = line
            .next()
            .buck_error_context("Failed to read user CPU usage")?;
        let system_millis_str = line
            .nth(1)
            .buck_error_context("Failed to read system CPU usage")?;

        return Ok(HostCpuUsage {
            user_millis: user_millis_str.parse::<u64>()?,
            system_millis: system_millis_str.parse::<u64>()?,
        });
    }
    Err(buck2_error!(
        buck2_error::ErrorTag::Tier0,
        "Failed to get CPU stats from /proc/stat"
    ))
}
