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
    pub user: Option<u64>,
    pub system: Option<u64>,
}

impl HostCpuUsage {
    pub fn get() -> buck2_error::Result<Self> {
        #[cfg(target_os = "macos")]
        {
            fn ticks_to_ms(ticks: u32, sc_clk_tck: u32) -> Option<u64> {
                // There are sc_clk_tck ticks in a second.
                (ticks as u64).checked_mul(1000 / sc_clk_tck as u64)
            }

            let sc_clk_tck = crate::os::macos::sc_clk_tck::sc_clk_tck()?;
            let load_info = crate::os::macos::host_cpu_load_info::host_cpu_load_info()?;
            Ok(HostCpuUsage {
                user: ticks_to_ms(load_info.user, sc_clk_tck),
                system: ticks_to_ms(load_info.system, sc_clk_tck),
            })
        }
        #[cfg(not(target_os = "macos"))]
        {
            use buck2_error::buck2_error;
            Err(buck2_error::buck2_error!(
                [],
                "HostCpuUsage is not implemented for this platform"
            ))
        }
    }
}
