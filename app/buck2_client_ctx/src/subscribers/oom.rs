/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::process::Stdio;
use std::time::Duration;
use std::time::SystemTime;

use buck2_core::soft_error;
use once_cell::sync::Lazy;
use regex::Regex;

static KERNEL_OOM_KILL_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"oom-kill:.*task_memcg=/([^,]+)").unwrap());

static OOMD_KILL_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"oomd kill:\s+\S+\s+\S+\s+\S+\s+(\S+)").unwrap());

static SYSTEMD_OOMD_KILL_RE: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"Killed (\S+) due to memory pressure for ").unwrap());

pub(super) async fn check_daemon_oom_killed(
    buck2_cgroup_path: &str,
    since: SystemTime,
) -> buck2_error::Result<bool> {
    let buck2_cgroup_path = match buck2_cgroup_path.strip_prefix("/sys/fs/cgroup/") {
        Some(rel) => rel,
        None => {
            let _unused = soft_error!(
                "oom_cgroup_path_unexpected_prefix",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Environment,
                    "cgroup path does not start with /sys/fs/cgroup/: {}",
                    buck2_cgroup_path
                ),
                quiet: true
            );
            return Ok(false);
        }
    };

    // Check kernel OOM killer messages in dmesg.
    // The kernel writes these synchronously when OOM killing, so they are
    // immediately available
    let since_str = format_timestamp_for_dmesg(since);
    let child = match buck2_util::process::async_background_command("dmesg")
        .args(["--since", &since_str])
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .kill_on_drop(true)
        .spawn()
    {
        Ok(c) => c,
        Err(e) => {
            let _unused = soft_error!(
                "dmesg_oom_detection_failed",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Environment,
                    "Failed to run dmesg for OOM detection: {}",
                    e
                ),
                quiet: true
            );
            return Ok(false);
        }
    };

    let output = match tokio::time::timeout(Duration::from_secs(5), child.wait_with_output()).await
    {
        Ok(Ok(output)) => output,
        Ok(Err(e)) => {
            let _unused = soft_error!(
                "dmesg_timeout",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Environment,
                    "Failed to wait on dmesg for OOM detection: {}",
                    e
                ),
                quiet: true
            );
            return Ok(false);
        }
        Err(_elapsed) => {
            let _unused = soft_error!(
                "dmesg_oom_detection_failed",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Environment,
                    "dmesg timed out after 5 seconds for OOM detection"
                ),
                quiet: true
            );
            return Ok(false);
        }
    };

    let stdout_str = String::from_utf8_lossy(&output.stdout);
    let reversed_lines: Vec<&str> = stdout_str.lines().rev().collect();
    if let Some(last_line) = reversed_lines.first() {
        tracing::debug!("dmesg last entry: {}", last_line);
    }
    for line in &reversed_lines {
        if dmesg_line_matches_oom_kill_cgroup(line, buck2_cgroup_path) {
            return Ok(true);
        }
    }
    Ok(false)
}

/// Format a `SystemTime` as `"YYYY-MM-DD HH:MM:SS"` in local time,
/// matching Eden's `timestampToDateTimeString` for use with `dmesg --since`.
fn format_timestamp_for_dmesg(time: SystemTime) -> String {
    let dt: chrono::DateTime<chrono::Local> = time.into();
    dt.format("%Y-%m-%d %H:%M:%S").to_string()
}

/// Check whether a dmesg line reports an OOM kill whose cgroup matches
/// `relative_cgroup` (either exactly, as a parent, or as a child).
fn dmesg_line_matches_oom_kill_cgroup(line: &str, buck2_cgroup: &str) -> bool {
    let matched_cgroup = parse_kernel_oom_kill_cgroup(line)
        .or_else(|| parse_oomd_kill_cgroup(line))
        .or_else(|| parse_systemd_oomd_kill_cgroup(line));
    if let Some(matched_cgroup) = matched_cgroup {
        matched_cgroup == buck2_cgroup
            // Add / in check to avoid matching a sibling cgroup
            || matched_cgroup.starts_with(&format!("{}/", buck2_cgroup))
            || buck2_cgroup.starts_with(&format!("{}/", matched_cgroup))
    } else {
        false
    }
}

/// Parse a kernel OOM kill line to extract the cgroup path.
///
/// Input format: `[timestamp] oom-kill:constraint=...,task_memcg=/<cgroup_path>,task=...,pid=...,uid=...`
/// Returns the cgroup path without leading `/`, e.g. `user.slice/.../allprocs/daemon`.
fn parse_kernel_oom_kill_cgroup(line: &str) -> Option<&str> {
    let caps = KERNEL_OOM_KILL_RE.captures(line)?;
    Some(caps.get(1)?.as_str())
}

/// Parse an oomd kill line to extract the cgroup path.
///
/// Input format: `[timestamp] oomd kill: <pressure1> <pressure2> <pressure3> <cgroup_path> <size> ruleset:[...] ...`
/// Returns the cgroup path, e.g. `user.slice/.../buck2_daemon....scope`.
fn parse_oomd_kill_cgroup(line: &str) -> Option<&str> {
    let caps = OOMD_KILL_RE.captures(line)?;
    Some(caps.get(1)?.as_str())
}

/// Parse a systemd-oomd kill line to extract the cgroup path.
///
/// Input format: `[timestamp] Killed /<cgroup_path> due to memory pressure for /<monitored_cgroup> being ...`
/// Returns the killed cgroup path without leading `/`, e.g. `system.slice/buck2_daemon....scope`.
fn parse_systemd_oomd_kill_cgroup(line: &str) -> Option<&str> {
    let caps = SYSTEMD_OOMD_KILL_RE.captures(line)?;
    let path = caps.get(1)?.as_str();
    // systemd-oomd paths have a leading `/`, strip it for consistency
    Some(path.strip_prefix('/').unwrap_or(path))
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_dmesg_line_matches_oom_kill_cgroup() {
        use super::dmesg_line_matches_oom_kill_cgroup;

        let line = "[Thu Jan  1 00:00:00 2025] oom-kill:constraint=CONSTRAINT_MEMCG,task_memcg=/user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope,task=buck2,pid=1234,uid=1000";

        assert!(dmesg_line_matches_oom_kill_cgroup(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope"
        ));
        assert!(dmesg_line_matches_oom_kill_cgroup(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice"
        ));
        assert!(dmesg_line_matches_oom_kill_cgroup(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope/child"
        ));

        // Sibling cgroup that shares a prefix should not match
        assert!(!dmesg_line_matches_oom_kill_cgroup(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope_sibling"
        ));

        // oomd kill line format
        let oomd_line = "[1813654.116643] oomd kill: 81.28 80.05 42.78 user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope 137490391040 ruleset:[protection against low swap] detectorgroup:[free swap goes below 5%] killCommand: build";

        assert!(dmesg_line_matches_oom_kill_cgroup(
            oomd_line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope"
        ));
        assert!(dmesg_line_matches_oom_kill_cgroup(
            oomd_line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice"
        ));
        assert!(dmesg_line_matches_oom_kill_cgroup(
            oomd_line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope/daemon"
        ));

        // Sibling cgroup that shares a prefix should not match
        assert!(!dmesg_line_matches_oom_kill_cgroup(
            oomd_line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope_sibling"
        ));
    }

    #[test]
    fn test_dmesg_line_matches_systemd_oomd_kill_cgroup() {
        use super::dmesg_line_matches_oom_kill_cgroup;

        let line = "[858172.042162] Killed /user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope due to memory pressure for /user.slice being 76.55% > 70.00% for > 3min with reclaim activity";

        // Exact cgroup match
        assert!(dmesg_line_matches_oom_kill_cgroup(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope"
        ));

        // Parent cgroup match
        assert!(dmesg_line_matches_oom_kill_cgroup(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice"
        ));

        // Child cgroup match
        assert!(dmesg_line_matches_oom_kill_cgroup(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope/child"
        ));

        // Sibling cgroup that shares a prefix should not match
        assert!(!dmesg_line_matches_oom_kill_cgroup(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope_sibling"
        ));
    }
}
