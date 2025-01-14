/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use dupe::Dupe;

/// CPU usage sum for all cores from the system start. Field values are in milliseconds.
#[derive(Debug, Clone, Dupe)]
pub struct HostCpuUsage {
    pub user_millis: u64,
    pub system_millis: u64,
}

impl HostCpuUsage {
    pub fn get() -> buck2_error::Result<Self> {
        #[cfg(target_os = "macos")]
        {
            let sc_clk_tck = crate::os::unix_like::sc_clk_tck::sc_clk_tck()?;
            let load_info = crate::os::macos::host_cpu_load_info::host_cpu_load_info()?;
            if let (Some(user_millis), Some(system_millis)) = (
                Self::ticks_to_ms(load_info.user.into(), sc_clk_tck),
                Self::ticks_to_ms(load_info.system.into(), sc_clk_tck),
            ) {
                Ok(HostCpuUsage {
                    user_millis,
                    system_millis,
                })
            } else {
                Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Error getting host CPU usage"
                ))
            }
        }
        #[cfg(target_os = "linux")]
        {
            let sc_clk_tck = crate::os::unix_like::sc_clk_tck::sc_clk_tck()?;
            let load_info = crate::os::linux::host_cpu_usage::host_cpu_usage()?;
            if let (Some(user_millis), Some(system_millis)) = (
                Self::ticks_to_ms(load_info.user_millis, sc_clk_tck),
                Self::ticks_to_ms(load_info.system_millis, sc_clk_tck),
            ) {
                Ok(HostCpuUsage {
                    user_millis,
                    system_millis,
                })
            } else {
                Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Error getting host CPU usage"
                ))
            }
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Unimplemented,
                "HostCpuUsage is not implemented for this platform"
            ))
        }
    }

    #[cfg(any(target_os = "macos", target_os = "linux"))]
    fn ticks_to_ms(ticks: u64, sc_clk_tck: u32) -> Option<u64> {
        // There are sc_clk_tck ticks in a second.
        ticks.checked_mul(1000)?.checked_div(sc_clk_tck as u64)
    }
}
