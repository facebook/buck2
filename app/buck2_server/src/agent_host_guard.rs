/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::buck2_env;
use buck2_events::metadata::hostname;
use globset::Glob;
use globset::GlobSet;
use globset::GlobSetBuilder;

use crate::daemon::state::DaemonStateData;

/// Substring that marks an originating cgroup as a coding-agent cgroup.
const CGROUP_AGENT_MARKER: &str = "3pai";

/// Comma-separated hostname globs matched against the local hostname.
/// Unset/empty disables the feature. Example:
/// ```ini
/// [buck2]
/// agent_hostname_fail_glob = bad-host-*.example.com,other-*
/// agent_hostname_fail_message = This host is temporarily blocked for agent builds, see S123456.
/// ```
const FAIL_GLOB_KEY: BuckconfigKeyRef = BuckconfigKeyRef {
    section: "buck2",
    property: "agent_hostname_fail_glob",
};
/// Shown to the user when a build is rejected. Required whenever
/// `agent_hostname_fail_glob` is set.
const FAIL_MESSAGE_KEY: BuckconfigKeyRef = BuckconfigKeyRef {
    section: "buck2",
    property: "agent_hostname_fail_message",
};

#[derive(Debug, buck2_error::Error)]
enum AgentHostGuardError {
    #[error("{message} (rejected on host `{hostname}`, originating cgroup `{cgroup}`)")]
    #[buck2(tag = Environment)]
    Denied {
        message: String,
        hostname: String,
        cgroup: String,
    },
    #[error(
        "`buck2.agent_hostname_fail_glob` is set but `buck2.agent_hostname_fail_message` is empty; set a message so rejected builds explain why"
    )]
    #[buck2(tag = Input)]
    MissingMessage,
    #[error("Invalid hostname glob `{glob}`: {error}")]
    #[buck2(tag = Input)]
    InvalidGlob { glob: String, error: String },
    #[error("Invalid hostname glob set: {error}")]
    #[buck2(tag = Input)]
    InvalidGlobSet { error: String },
}

/// Reject the build if it originates from a 3pai cgroup and runs on a denylisted host.
pub(crate) fn check_agent_host_guard(
    config: &LegacyBuckConfig,
    daemon: &DaemonStateData,
) -> buck2_error::Result<()> {
    let glob = config.get(FAIL_GLOB_KEY).unwrap_or("").trim();
    if glob.is_empty() {
        // Feature disabled.
        return Ok(());
    }

    let message = config.get(FAIL_MESSAGE_KEY).unwrap_or("").trim();
    if message.is_empty() {
        return Err(AgentHostGuardError::MissingMessage.into());
    }

    // The cgroup spawner is disabled in integration tests, so `daemon_originating_cgroup`
    // is always `None` there. This test-only override lets tests exercise the gate.
    let cgroup_override = buck2_env!(
        "BUCK2_TEST_DAEMON_ORIGINATING_CGROUP",
        applicability = testing,
    )?;
    let cgroup = cgroup_override.or(daemon.daemon_originating_cgroup.as_deref());

    let Some(cgroup) = cgroup.filter(|c| c.contains(CGROUP_AGENT_MARKER)) else {
        // Not an agent-originated daemon.
        return Ok(());
    };

    let Some(hostname) = hostname() else {
        // Can't determine the hostname; don't block.
        return Ok(());
    };

    if build_hostname_globset(glob)?.is_match(&hostname) {
        return Err(AgentHostGuardError::Denied {
            message: message.to_owned(),
            hostname,
            cgroup: cgroup.to_owned(),
        }
        .into());
    }

    Ok(())
}

/// Build a `GlobSet` from a comma-separated list of hostname glob patterns.
fn build_hostname_globset(spec: &str) -> buck2_error::Result<GlobSet> {
    let mut builder = GlobSetBuilder::new();
    for pattern in spec.split(',').map(str::trim).filter(|s| !s.is_empty()) {
        let glob = Glob::new(pattern).map_err(|e| AgentHostGuardError::InvalidGlob {
            glob: pattern.to_owned(),
            error: e.to_string(),
        })?;
        builder.add(glob);
    }
    builder.build().map_err(|e| {
        AgentHostGuardError::InvalidGlobSet {
            error: e.to_string(),
        }
        .into()
    })
}
