/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_util::os::host_cpu_usage::HostCpuUsage;
use dupe::Dupe;

/// Host CPU usage since a buck2 command was started.
pub(crate) struct HostCpuUsageSinceCmdStart {
    pub(crate) user_millis: u64,
    pub(crate) system_millis: u64,
}

/// Collects and emits the delta of CPU usage since the command start (i.e. collector creation).
#[derive(Clone, Dupe)]
pub(crate) struct CpuUsageCollector {
    start: HostCpuUsage,
}

impl CpuUsageCollector {
    pub(crate) fn new() -> anyhow::Result<Self> {
        // Collect the reference point for the CPU usage at the start of the command.
        let start = HostCpuUsage::get()?;
        Ok(Self { start })
    }

    /// Returns the CPU usage since the collector was created.
    pub(crate) fn get_usage_since_command_start(
        &self,
    ) -> anyhow::Result<HostCpuUsageSinceCmdStart> {
        if let Ok(current_usage) = HostCpuUsage::get() {
            // TODO(rajneeshl): The MacOS low-level API returns u32 for the tick count which overflows
            // in a couple of weeks. This could lead to situations where the start > current.
            if let (Some(user_millis), Some(system_millis)) = (
                current_usage
                    .user_millis
                    .checked_sub(self.start.user_millis),
                current_usage
                    .system_millis
                    .checked_sub(self.start.system_millis),
            ) {
                return Ok(HostCpuUsageSinceCmdStart {
                    user_millis,
                    system_millis,
                });
            }
        }
        Err(anyhow::anyhow!("Failed to get CPU usage"))
    }
}
