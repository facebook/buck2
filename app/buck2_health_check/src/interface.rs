/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::health_check_context::HealthCheckContext;
use crate::report::Report;

#[derive(Eq, PartialEq, Hash)]
pub enum HealthCheckType {
    MemoryPressure,
    LowDiskSpace,
    SlowDownloadSpeed,
    VpnEnabled,
    StableRevision,
}

/// Trait to generalize a buck2 health check.
/// Refer https://fburl.com/buck_health_checks for details on adding a new health check.
#[async_trait::async_trait]
pub(crate) trait HealthCheck: Send + Sync {
    /// Returns an optional report when invoked at every `snapshot` event.
    /// Return value is interpreted as follows:
    /// `None`: Health check cannot run. e.g. not applicable for this command/target
    /// `tag: None and health_issue: None`: Health check ran but nothing to report (all healthy)
    /// `tag: Some/None and health_issue: Some/None`: The issue could either be reported to user on console, logged to scuba or both.
    fn run_check(&self) -> buck2_error::Result<Option<Report>>;

    /// Trigger when the health check context updates.
    /// The `run_check` method is executed repeatedly at every snapshot and should be optimized.
    /// This trigger can be used to precompute/cache relevant data.
    async fn handle_context_update(&mut self, context: &HealthCheckContext);
}
