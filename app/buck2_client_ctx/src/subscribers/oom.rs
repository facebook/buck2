/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::process::Stdio;
use std::sync::LazyLock;
use std::time::Duration;
use std::time::Instant;

use buck2_core::soft_error;
use regex::Regex;

static KERNEL_OOM_KILL_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"oom-kill:.*task_memcg=/([^,]+)").unwrap());

static OOMD_KILL_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"oomd kill:\s+\S+\s+\S+\s+\S+\s+(\S+)").unwrap());

static SYSTEMD_OOMD_KILL_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"Killed (\S+) due to memory pressure for ").unwrap());

/// Monotonic lookback used when the daemon start time is unavailable.
const DMESG_LOOKBACK: Duration = Duration::from_secs(5 * 60);

pub(crate) async fn check_daemon_oom_killed(
    cgroup_path_of_buck2_daemon: &str,
    daemon_start_instant: Option<Instant>,
) -> buck2_error::Result<bool> {
    let cgroup_path_of_buck2_daemon =
        match cgroup_path_of_buck2_daemon.strip_prefix("/sys/fs/cgroup/") {
            Some(rel) => rel,
            None => {
                let _unused = soft_error!(
                    "oom_cgroup_path_unexpected_prefix",
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Environment,
                        "cgroup path does not start with /sys/fs/cgroup/: {}",
                        cgroup_path_of_buck2_daemon
                    ),
                    quiet: true
                );
                return Ok(false);
            }
        };

    // Check kernel OOM killer messages in dmesg.
    // The kernel writes these synchronously when OOM killing, so they are
    // immediately available
    let lower_bound = match dmesg_lower_bound(daemon_start_instant) {
        Ok(lower_bound) => lower_bound,
        Err(e) => {
            let _unused = soft_error!("dmesg_monotonic_clock_failed", e, quiet: true);
            return Ok(false);
        }
    };
    // Raw output fixes timestamps in boot-time seconds and prefixes records with `<priority>`.
    let child = match buck2_util::process::async_background_command("dmesg")
        .arg("--raw")
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
    tracing::debug!(
        "OOM detection: dmesg returned {} lines, last: {:?}",
        stdout_str.lines().count(),
        stdout_str.lines().next_back(),
    );
    if let Some(line) =
        find_matching_oom_kill(&stdout_str, cgroup_path_of_buck2_daemon, lower_bound)
    {
        tracing::debug!("OOM detection matched dmesg line: {:?}", line);
        return Ok(true);
    }
    Ok(false)
}

fn dmesg_lower_bound(daemon_start_instant: Option<Instant>) -> buck2_error::Result<Duration> {
    // Sample the raw clock first so call latency moves the estimated start earlier.
    let now_monotonic = monotonic_now()?;
    let daemon_start_monotonic = daemon_start_instant.and_then(|start| {
        let daemon_age = Instant::now().checked_duration_since(start)?;
        now_monotonic.checked_sub(daemon_age)
    });
    Ok(dmesg_lower_bound_at(now_monotonic, daemon_start_monotonic))
}

fn dmesg_lower_bound_at(
    now_monotonic: Duration,
    daemon_start_monotonic: Option<Duration>,
) -> Duration {
    let buffered_since = now_monotonic.saturating_sub(DMESG_LOOKBACK);
    let lower_bound = daemon_start_monotonic.unwrap_or(buffered_since);
    // Raw dmesg timestamps have microsecond precision, so round the lower bound down rather
    // than exclude an event from the daemon's first displayed microsecond.
    Duration::new(lower_bound.as_secs(), lower_bound.subsec_micros() * 1_000)
}

fn monotonic_now() -> buck2_error::Result<Duration> {
    let mut timestamp = libc::timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    // SAFETY: `timestamp` is valid, aligned, writable storage for a `timespec` throughout
    // the call, and POSIX `clock_gettime` does not retain the pointer.
    if unsafe { libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut timestamp) } != 0 {
        return Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Environment,
            "clock_gettime(CLOCK_MONOTONIC) failed: {}",
            std::io::Error::last_os_error()
        ));
    }
    if timestamp.tv_sec < 0 || !(0..1_000_000_000).contains(&timestamp.tv_nsec) {
        return Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Environment,
            "clock_gettime(CLOCK_MONOTONIC) returned invalid timespec: {}s {}ns",
            timestamp.tv_sec,
            timestamp.tv_nsec
        ));
    }
    Ok(Duration::new(
        timestamp.tv_sec as u64,
        timestamp.tv_nsec as u32,
    ))
}

fn find_matching_oom_kill<'a>(
    dmesg: &'a str,
    cgroup_path_of_buck2_daemon: &str,
    lower_bound: Duration,
) -> Option<&'a str> {
    let matcher = Buck2CgroupMatcher::new(cgroup_path_of_buck2_daemon);
    for line in dmesg.lines().rev() {
        let Some(event_time) = parse_dmesg_timestamp(line) else {
            continue;
        };
        if event_time < lower_bound {
            // `dmesg` records are chronological, so every preceding record is older too.
            break;
        }
        if !matcher.dmesg_line_matches_oom_kill(line) {
            continue;
        }
        return Some(line);
    }
    None
}

fn parse_dmesg_timestamp(line: &str) -> Option<Duration> {
    // Linux `dmesg --raw` prefixes each record with priority and seconds since boot, for example
    // `<6>[1813654.116643] oomd kill: ...`.
    let line = line.trim_start();
    let line = if let Some(line) = line.strip_prefix('<') {
        line.split_once('>')?.1
    } else {
        line
    };
    let raw = line
        .trim_start()
        .strip_prefix('[')?
        .split_once(']')?
        .0
        .trim();
    let (seconds, fraction) = raw.split_once('.').unwrap_or((raw, ""));
    let seconds = seconds.parse::<u64>().ok()?;
    if fraction.len() > 9 || !fraction.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let nanoseconds = if fraction.is_empty() {
        0
    } else {
        fraction.parse::<u32>().ok()? * 10u32.pow(9 - fraction.len() as u32)
    };
    Some(Duration::new(seconds, nanoseconds))
}

/// FNV prime, used as the polynomial base in `Buck2CgroupMatcher`.
const NAMESPACE_HASH_BASE: u64 = 0x100000001b3;

fn hash_component(s: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}

/// Pre-computed view of a buck2 daemon cgroup path used to test dmesg lines
/// against many candidate killed cgroups without re-splitting or re-hashing
/// `buck2_cgroup` each time.
struct Buck2CgroupMatcher<'a> {
    components: Vec<&'a str>,
    /// `prefix_hashes[k]` is the polynomial rolling hash of `components[..k]`,
    /// i.e. `sum_{j<k} h(components[j]) * BASE^j`, under the recurrence
    /// `prefix_hashes[k+1] = prefix_hashes[k] + h(components[k]) * BASE^k`.
    /// Length is `components.len() + 1`; `prefix_hashes[0] == 0`.
    prefix_hashes: Box<[u64]>,
}

impl<'a> Buck2CgroupMatcher<'a> {
    fn new(buck2_cgroup: &'a str) -> Self {
        let components: Vec<&'a str> = buck2_cgroup.split('/').collect();
        let prefix_hashes: Box<[u64]> = std::iter::once(0u64)
            .chain(components.iter().scan((0u64, 1u64), |(h, power), c| {
                *h = h.wrapping_add(hash_component(c).wrapping_mul(*power));
                *power = power.wrapping_mul(NAMESPACE_HASH_BASE);
                Some(*h)
            }))
            .collect();
        Self {
            components,
            prefix_hashes,
        }
    }

    /// Check whether a dmesg line reports an OOM kill whose cgroup matches
    /// this buck2 cgroup (either exactly, as a parent, as a child, or as an
    /// ancestor under a cgroup-namespace prefix).
    fn dmesg_line_matches_oom_kill(&self, line: &str) -> bool {
        let Some(matched_cgroup) = parse_kernel_oom_kill_cgroup(line)
            .or_else(|| parse_oomd_kill_cgroup(line))
            .or_else(|| parse_systemd_oomd_kill_cgroup(line))
        else {
            return false;
        };
        let killed: Vec<&str> = matched_cgroup.split('/').collect();
        killed.starts_with(&self.components)
            || self.components.starts_with(&killed)
            || self.prefix_matches_suffix(&killed)
    }

    /// Check whether some non-empty proper suffix of `killed`'s path
    /// components equals a prefix of this buck2 cgroup's path components.
    /// This is the case when oomd from the physical host kills `killed`
    /// while the buck2 daemon (or one of its ancestors) is observed inside a
    /// (possibly nested) cgroup namespace mounted under `killed`.
    ///
    /// Uses pre-computed prefix hashes of `self.components` to skip the
    /// slice-equality check on candidates whose hash does not match.
    fn prefix_matches_suffix(&self, killed: &[&str]) -> bool {
        // i must satisfy 1 <= i < n (non-empty proper suffix) and n - i <= m
        // (suffix length cannot exceed the buck2 prefix it is compared to).
        let start = killed.len().saturating_sub(self.components.len()).max(1);
        // Iterate i from largest to smallest so we can extend `killed[i..]` by
        // one component each step via `f(i) = h(killed[i]) + BASE * f(i+1)`,
        // making the whole scan O(n) instead of O(n^2).
        (start..killed.len())
            .rev()
            .scan(0u64, |suffix_hash, i| {
                *suffix_hash = hash_component(killed[i])
                    .wrapping_add(NAMESPACE_HASH_BASE.wrapping_mul(*suffix_hash));
                Some((i, *suffix_hash))
            })
            .any(|(i, suffix_hash)| {
                let suffix_len = killed.len() - i;
                suffix_hash == self.prefix_hashes[suffix_len]
                    && self.components[..suffix_len] == killed[i..]
            })
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
    use std::time::Duration;

    use super::Buck2CgroupMatcher;
    use super::dmesg_lower_bound_at;
    use super::find_matching_oom_kill;
    use super::parse_dmesg_timestamp;

    #[test]
    fn test_dmesg_line_matches_oom_kill_cgroup() {
        let matches = |line: &str, buck2_cgroup: &str| {
            Buck2CgroupMatcher::new(buck2_cgroup).dmesg_line_matches_oom_kill(line)
        };

        let line = "[Thu Jan  1 00:00:00 2025] oom-kill:constraint=CONSTRAINT_MEMCG,task_memcg=/user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope,task=buck2,pid=1234,uid=1000";

        assert!(matches(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope"
        ));
        assert!(matches(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice"
        ));
        assert!(matches(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope/child"
        ));

        // Sibling cgroup that shares a prefix should not match
        assert!(!matches(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope_sibling"
        ));

        // oomd kill line format
        let oomd_line = "[1813654.116643] oomd kill: 81.28 80.05 42.78 user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope 137490391040 ruleset:[protection against low swap] detectorgroup:[free swap goes below 5%] killCommand: build";

        assert!(matches(
            oomd_line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope"
        ));
        assert!(matches(
            oomd_line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice"
        ));
        assert!(matches(
            oomd_line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope/daemon"
        ));

        // Sibling cgroup that shares a prefix should not match
        assert!(!matches(
            oomd_line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope_sibling"
        ));
    }

    #[test]
    fn test_dmesg_line_matches_oom_kill_cgroup_namespace_prefix() {
        let matches = |line: &str, buck2_cgroup: &str| {
            Buck2CgroupMatcher::new(buck2_cgroup).dmesg_line_matches_oom_kill(line)
        };

        // oomd on the physical host logs the absolute
        // host-prefixed cgroup path (`workload.slice/...`),
        // while the buck2 client reads the daemon's cgroup path from
        // inside a container's cgroup namespace
        // (starts at `task/...`). A parent of the daemon's cgroup is killed.
        let od_oomd_line = "[1813654.116643] oomd kill: 81.28 80.05 42.78 workload.slice/workload-tw.slice/workload-tw-twcli_fake_uuid.a36daf.allotment.slice/fbcode_154651.0_od0473.vll6_65021946483df_252.task.xx._650219bcdd5a4_258.tw.task.service/task/user.slice/user-29230.slice/user@29230.service/buck2.slice 137490391040 ruleset:[protection against low swap] detectorgroup:[free swap goes below 5%] killCommand: build";

        assert!(matches(
            od_oomd_line,
            "task/user.slice/user-29230.slice/user@29230.service/buck2.slice/buck2_daemon.fbsource.v2.f73409b0_6cb5_4f25_8d82_55a1c4a2d4ba.scope/daemon"
        ));

        // Daemon's own cgroup is killed
        let od_oomd_line_daemon_killed = "[1813654.116643] oomd kill: 81.28 80.05 42.78 workload.slice/workload-tw.slice/workload-tw-twcli_fake_uuid.a36daf.allotment.slice/fbcode_154651.0_od0473.vll6_65021946483df_252.task.xx._650219bcdd5a4_258.tw.task.service/task/user.slice/user-29230.slice/user@29230.service/buck2.slice/buck2_daemon.fbsource.v2.f73409b0_6cb5_4f25_8d82_55a1c4a2d4ba.scope/daemon 137490391040 ruleset:[protection against low swap] detectorgroup:[free swap goes below 5%] killCommand: build";

        assert!(matches(
            od_oomd_line_daemon_killed,
            "task/user.slice/user-29230.slice/user@29230.service/buck2.slice/buck2_daemon.fbsource.v2.f73409b0_6cb5_4f25_8d82_55a1c4a2d4ba.scope/daemon"
        ));

        // Negative: matched_cgroup tail looks like `buck2_cgroup` but not at a
        // `/` boundary — for instance, host path ends with `xtask` while
        // buck2_cgroup starts with `task`. Must not match.
        let bad_boundary_line = "[1813654.116643] oomd kill: 81.28 80.05 42.78 workload.slice/workload-tw.slice/sometask/user.slice/user-29230.slice/user@29230.service/buck2.slice 137490391040 ruleset:[protection against low swap] detectorgroup:[free swap goes below 5%] killCommand: build";

        assert!(!matches(
            bad_boundary_line,
            "task/user.slice/user-29230.slice/user@29230.service/buck2.slice/buck2_daemon.fbsource.v2.f73409b0_6cb5_4f25_8d82_55a1c4a2d4ba.scope/daemon"
        ));

        // Negative: totally unrelated host cgroup (no overlap at all).
        let unrelated_line = "[1813654.116643] oomd kill: 81.28 80.05 42.78 system.slice/some-other.service 137490391040 ruleset:[protection against low swap] detectorgroup:[free swap goes below 5%] killCommand: build";

        assert!(!matches(
            unrelated_line,
            "task/user.slice/user-29230.slice/user@29230.service/buck2.slice/buck2_daemon.fbsource.v2.f73409b0_6cb5_4f25_8d82_55a1c4a2d4ba.scope/daemon"
        ));
    }

    #[test]
    fn test_dmesg_line_matches_systemd_oomd_kill_cgroup() {
        let matches = |line: &str, buck2_cgroup: &str| {
            Buck2CgroupMatcher::new(buck2_cgroup).dmesg_line_matches_oom_kill(line)
        };

        let line = "[858172.042162] Killed /user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope due to memory pressure for /user.slice being 76.55% > 70.00% for > 3min with reclaim activity";

        // Exact cgroup match
        assert!(matches(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope"
        ));

        // Parent cgroup match
        assert!(matches(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice"
        ));

        // Child cgroup match
        assert!(matches(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope/child"
        ));

        // Sibling cgroup that shares a prefix should not match
        assert!(!matches(
            line,
            "user.slice/user-190155.slice/user@190155.service/buck2.slice/buck2_daemon.fbsource.v2.1ac02af6_d3c8_4eb1_bb72_c351ba823628.scope_sibling"
        ));
    }

    #[test]
    fn test_dmesg_lower_bound_uses_daemon_start_or_lookback() {
        let now = Duration::from_secs(1_000);

        assert_eq!(Duration::from_secs(700), dmesg_lower_bound_at(now, None));
        assert_eq!(
            Duration::from_secs(970),
            dmesg_lower_bound_at(now, Some(Duration::from_secs(970)))
        );
        assert_eq!(
            Duration::from_secs(100),
            dmesg_lower_bound_at(now, Some(Duration::from_secs(100)))
        );
        assert_eq!(
            Duration::ZERO,
            dmesg_lower_bound_at(Duration::from_secs(100), None)
        );
        assert_eq!(
            Duration::new(970, 123_456_000),
            dmesg_lower_bound_at(
                Duration::new(1_000, 123_456_789),
                Some(Duration::new(970, 123_456_789))
            )
        );
    }

    #[test]
    fn test_parse_dmesg_timestamp() {
        assert_eq!(
            Some(Duration::new(1_813_654, 116_643_000)),
            parse_dmesg_timestamp("<6>[1813654.116643] oomd kill")
        );
        assert_eq!(
            Some(Duration::new(42, 100_000_000)),
            parse_dmesg_timestamp("  [ 42.1 ] oom-kill")
        );
        assert_eq!(
            Some(Duration::from_secs(42)),
            parse_dmesg_timestamp("[42] oom-kill")
        );
        assert_eq!(None, parse_dmesg_timestamp("[Thu Jan 1] oom-kill"));
    }

    #[test]
    fn test_stale_matching_oom_does_not_misclassify_daemon_crash() {
        let cgroup = "user.slice/buck2.slice/buck2_daemon.scope";
        let stale_oom = "<6>[99.999999] oom-kill:constraint=CONSTRAINT_MEMCG,task_memcg=/user.slice/buck2.slice/buck2_daemon.scope,task=buck2,pid=1234,uid=1000";
        let unparseable_oom = "<6>[Thu Jan 1 00:00:00 2025] oom-kill:constraint=CONSTRAINT_MEMCG,task_memcg=/user.slice/buck2.slice/buck2_daemon.scope,task=buck2,pid=1234,uid=1000";
        let unrelated_current_oom = "<6>[101.000000] oom-kill:constraint=CONSTRAINT_MEMCG,task_memcg=/system.slice/unrelated.scope,task=other,pid=5678,uid=1000";
        let dmesg = format!("{stale_oom}\n{unparseable_oom}\n{unrelated_current_oom}");

        assert_eq!(
            None,
            find_matching_oom_kill(&dmesg, cgroup, Duration::from_secs(100))
        );

        let current_oom = "<6>[100.000000] oom-kill:constraint=CONSTRAINT_MEMCG,task_memcg=/user.slice/buck2.slice/buck2_daemon.scope,task=buck2,pid=1234,uid=1000";
        let dmesg = format!("{dmesg}\n{current_oom}");
        assert_eq!(
            Some(current_oom),
            find_matching_oom_kill(&dmesg, cgroup, Duration::from_secs(100))
        );
    }
}
