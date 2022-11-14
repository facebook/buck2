/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::memory::allocator_stats;
use cli_proto::StatusResponse;

// TODO(raulgarcia4): this fn is used for `buck2 status` but will be redundant
// and thus removed once we capture jemalloc stats in the Snapshot.
#[cfg(all(unix, not(fbcode_build), not(buck_oss_build)))]
pub fn jemalloc_stats(response: &mut StatusResponse) {
    use jemalloc_ctl::epoch;
    use jemalloc_ctl::stats;

    fn set<T>(to: &mut Option<u64>, from: Result<usize, T>) {
        if let Ok(from) = from {
            *to = Some(from as u64);
        }
    }

    // Many statistics are cached and only updated when the epoch is advanced.
    if epoch::advance().is_err() {
        return;
    }
    set(&mut response.bytes_allocated, stats::allocated::read());
    set(&mut response.bytes_resident, stats::resident::read());
    set(&mut response.bytes_retained, stats::retained::read());
}

#[cfg(not(all(unix, not(fbcode_build), not(buck_oss_build))))]
pub fn jemalloc_stats(_response: &mut StatusResponse) {}

// TODO(raulgarcia4): Consider moving out this file to buck2_common,
// similary to what was done with buck2_common::process_stats.
pub struct AllocatorStats {
    pub bytes_active: Option<u64>,
    pub bytes_allocated: Option<u64>,
}

pub fn get_allocator_stats() -> anyhow::Result<AllocatorStats> {
    fn set(stats: &serde_json::Value, val: &str, to: &mut Option<u64>) -> anyhow::Result<()> {
        if let serde_json::Value::Number(stat) = &stats["jemalloc"]["stats"][val] {
            *to = stat.as_u64();
            Ok(())
        } else {
            Err(anyhow::anyhow!("Allocator stat '{}' not found.", val))
        }
    }

    // This options configuration flag string is passed to `malloc_stats_print()`.
    // Explanation: (<https://jemalloc.net/jemalloc.3.html>)
    // The default configuration prints minimal output, formatted as JSON.
    let alloc_stats_options = "Jmdablxg";
    let alloc_stats: serde_json::Value =
        serde_json::from_str(&allocator_stats(alloc_stats_options)?)?;

    let mut bytes_active = None;
    let mut bytes_allocated = None;
    set(&alloc_stats, "active", &mut bytes_active)?;
    set(&alloc_stats, "allocated", &mut bytes_allocated)?;

    Ok(AllocatorStats {
        bytes_active,
        bytes_allocated,
    })
}

#[cfg(test)]
mod tests {
    use buck2_common::memory::has_jemalloc_stats;

    use crate::jemalloc_stats::get_allocator_stats;
    use crate::jemalloc_stats::jemalloc_stats;

    #[test]
    fn test_jemalloc_stats() {
        if cfg!(unix) && !cfg!(fbcode_build) {
            let mut response = cli_proto::StatusResponse::default();
            jemalloc_stats(&mut response);
            assert!(response.bytes_allocated.is_some());
            assert!(response.bytes_resident.is_some());
            assert!(response.bytes_retained.is_some());
        }
    }

    #[test]
    fn test_get_allocator_stats() -> anyhow::Result<()> {
        if has_jemalloc_stats() {
            if let Ok(alloc_stats) = get_allocator_stats() {
                assert!(alloc_stats.bytes_active.is_some());
                assert!(alloc_stats.bytes_allocated.is_some());
                return Ok(());
            }
            return Err(anyhow::anyhow!("Allocator stats not found"));
        }
        Ok(())
    }
}
