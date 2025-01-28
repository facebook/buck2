/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::is_open_source;
use buck2_event_observer::action_stats::ActionStats;
use buck2_event_observer::humanized::HumanizedBytes;

use crate::subscribers::recorder::process_memory;

const BYTES_PER_GIGABYTE: u64 = 1000000000;

pub(crate) struct MemoryPressureHigh {
    pub(crate) system_total_memory: u64,
    pub(crate) process_memory: u64,
}

pub(crate) struct LowDiskSpace {
    pub(crate) total_disk_space: u64,
    pub(crate) used_disk_space: u64,
}

pub const SYSTEM_MEMORY_REMEDIATION_LINK: &str = ": https://fburl.com/buck2_mem_remediation";
pub const DISK_REMEDIATION_LINK: &str = ": https://fburl.com/buck2_disk_remediation";
pub const CACHE_MISS_LINK: &str = "https://fburl.com/buck2_cache_miss";

pub(crate) fn system_memory_exceeded_msg(memory_pressure: &MemoryPressureHigh) -> String {
    format!(
        "High memory pressure: buck2 is using {} out of {}{}",
        HumanizedBytes::new(memory_pressure.process_memory),
        HumanizedBytes::new(memory_pressure.system_total_memory),
        if is_open_source() {
            ""
        } else {
            SYSTEM_MEMORY_REMEDIATION_LINK
        }
    )
}

pub(crate) fn low_disk_space_msg(low_disk_space: &LowDiskSpace) -> String {
    format!(
        "Low disk space: only {} remaining out of {}{}",
        HumanizedBytes::new(low_disk_space.total_disk_space - low_disk_space.used_disk_space),
        HumanizedBytes::new(low_disk_space.total_disk_space),
        if is_open_source() {
            ""
        } else {
            DISK_REMEDIATION_LINK
        }
    )
}

pub(crate) fn cache_misses_msg(action_stats: &ActionStats) -> String {
    let cache_hit_percent = action_stats.total_cache_hit_percentage();
    let msg = format!(
        "Low cache hits detected: {}%. This may significantly impact build speed",
        cache_hit_percent
    );
    if !is_open_source() {
        format!("{msg}: {CACHE_MISS_LINK}.")
    } else {
        format!("{msg}. Try rebasing to a stable revision with warmed caches.")
    }
}

pub(crate) fn check_memory_pressure(
    process_memory: u64,
    system_info: &buck2_data::SystemInfo,
) -> Option<MemoryPressureHigh> {
    let system_total_memory = system_info.system_total_memory_bytes?;
    let memory_pressure_threshold_percent = system_info.memory_pressure_threshold_percent?;
    // TODO (ezgi): one-shot commands don't record this. Prevent panick (division-by-zero) until it is fixed.
    if (process_memory * 100)
        .checked_div(system_total_memory)
        .is_some_and(|res| res >= memory_pressure_threshold_percent)
    {
        Some(MemoryPressureHigh {
            system_total_memory,
            process_memory,
        })
    } else {
        None
    }
}

pub(crate) fn check_memory_pressure_snapshot(
    last_snapshot: Option<&buck2_data::Snapshot>,
    system_info: &buck2_data::SystemInfo,
) -> Option<MemoryPressureHigh> {
    let process_memory = process_memory(last_snapshot?)?;
    check_memory_pressure(process_memory, system_info)
}

pub(crate) fn check_remaining_disk_space(
    used_disk_space: u64,
    system_info: &buck2_data::SystemInfo,
) -> Option<LowDiskSpace> {
    let total_disk_space = system_info.total_disk_space_bytes?;
    let remaining_disk_space_threshold =
        system_info.remaining_disk_space_threshold_gb? * BYTES_PER_GIGABYTE;

    if total_disk_space - used_disk_space <= remaining_disk_space_threshold {
        Some(LowDiskSpace {
            total_disk_space,
            used_disk_space,
        })
    } else {
        None
    }
}

pub(crate) fn check_remaining_disk_space_snapshot(
    last_snapshot: Option<&buck2_data::Snapshot>,
    system_info: &buck2_data::SystemInfo,
) -> Option<LowDiskSpace> {
    let used_disk_space = last_snapshot?.used_disk_space_bytes?;
    check_remaining_disk_space(used_disk_space, system_info)
}

// This check uses average RE download speed calculated as a number of bytes downloaded divided on time between two snapshots.
// This speed calculation is not precisely correct as we don't know how much time we've been downloading between two snapshots.
// TODO(yurysamkevich): compute average download speed in RE/HTTP client
pub(crate) fn check_download_speed(
    first_snapshot: &Option<buck2_data::Snapshot>,
    last_snapshot: Option<&buck2_data::Snapshot>,
    system_info: &buck2_data::SystemInfo,
    avg_re_download_speed: Option<u64>,
    concurrent_commands: bool,
) -> bool {
    // RE download/upload stats is collected per daemon.
    // If there are concurrent commands we get stats for both.
    // It's incorrect to display the warning in this case.
    if concurrent_commands {
        return false;
    }
    inner_check_download_speed(
        first_snapshot,
        last_snapshot,
        system_info,
        avg_re_download_speed,
    )
    .is_some()
}

fn inner_check_download_speed(
    first_snapshot: &Option<buck2_data::Snapshot>,
    last_snapshot: Option<&buck2_data::Snapshot>,
    system_info: &buck2_data::SystemInfo,
    avg_re_download_speed: Option<u64>,
) -> Option<()> {
    let re_download_bytes =
        last_snapshot?.re_download_bytes - first_snapshot.as_ref()?.re_download_bytes;
    let avg_re_download_speed = avg_re_download_speed?;

    if re_download_bytes >= system_info.min_re_download_bytes_threshold?
        && avg_re_download_speed < system_info.avg_re_download_bytes_per_sec_threshold?
    {
        Some(())
    } else {
        None
    }
}

pub(crate) fn is_vpn_enabled() -> bool {
    if !cfg!(target_os = "macos") {
        // TODO(rajneeshl): Add support for Windows
        return false;
    }

    // Brittle check based on Cisco client's current behaviour.
    // Small section copied from https://fburl.com/code/g7ttsdz3
    std::path::Path::new("/opt/cisco/secureclient/vpn/ac_pf.token").exists()
}

pub(crate) fn check_cache_misses(
    action_stats: &ActionStats,
    system_info: &buck2_data::SystemInfo,
    first_build_since_rebase: bool,
    estimated_completion_percent: Option<u8>,
) -> bool {
    if !cache_warning_completion_threshold_crossed(
        action_stats,
        estimated_completion_percent,
        system_info,
    ) {
        return false;
    }
    let cache_hit_percent = action_stats.total_cache_hit_percentage();
    let threshold = system_info.min_cache_hit_threshold_percent.unwrap_or(0) as u8;
    first_build_since_rebase && cache_hit_percent < threshold
}

fn cache_warning_completion_threshold_crossed(
    action_stats: &ActionStats,
    estimated_completion_percent: Option<u8>,
    system_info: &buck2_data::SystemInfo,
) -> bool {
    if let Some(estimated_completion_percent) = estimated_completion_percent {
        let percent_completion_threshold = system_info
            .cache_warning_min_completion_threshold_percent
            .unwrap_or(0) as u8;
        if estimated_completion_percent > percent_completion_threshold {
            return true;
        }
    }

    // The completion_treshold is set to 10% typically.
    // For large builds, 10% completion may be too late to warn about cache misses.
    // Additionally check if we have crossed an action count threshold.
    action_stats.total_executed_and_cached_actions()
        > system_info.cache_warning_min_actions_count.unwrap_or(0)
}
