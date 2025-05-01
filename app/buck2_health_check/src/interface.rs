/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::report::Report;

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
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

/// Trait to generalize a health check service e.g. in-process, out-of-process over gRPC.
#[async_trait::async_trait]
pub(crate) trait HealthCheckService: Sync + Send {
    /// Update the context for the health check service.
    async fn update_context(&mut self, event: HealthCheckContextEvent) -> buck2_error::Result<()>;

    /// Run all registered health checks.
    async fn run_checks(&mut self) -> buck2_error::Result<Vec<Report>>;
}

/// A subset of the client data that is relevant for health checks.
/// This is intentionally kept as a small set to avoid serialization costs.
#[derive(Default)]
pub(crate) struct HealthCheckContext {
    /// Data from the command start.
    /// Example use: Run a check only on a subset of commands.
    pub command_data: Option<buck2_data::command_start::Data>,

    /// Target patterns.
    /// Example use: Project/target specific checks, target specific configs e.g. warm revision.
    pub parsed_target_patterns: Option<buck2_data::ParsedTargetPatterns>,

    /// Revision hash of the mergebase.
    /// Example use: Warm revision check.
    pub branched_from_revision: Option<String>,

    /// Denotes if this command is seeing cache miss without any file changes.
    pub has_excess_cache_misses: bool,

    /// Configurations for health check experiments.
    pub experiment_configurations: Option<buck2_data::SystemInfo>,
}

/// An event from the daemon event subscriber to the health check client.
pub enum HealthCheckEvent {
    HealthCheckContextEvent(HealthCheckContextEvent),
    // This snapshot can be used to pass a subset of the buck2_data::Snapshot data to health checks.
    // Presently, unused since the existing health checks do not need this data.
    Snapshot(),
}

/// An event to trigger update of context in the health check server.
/// This may result in side effects like precomputing data, etc. in health checks.
pub enum HealthCheckContextEvent {
    CommandStart(buck2_data::CommandStart),
    ParsedTargetPatterns(buck2_data::ParsedTargetPatterns),
    BranchedFromRevision(String),
    /// Sent only once and communicates if buck2 is experiencing excess cache misses.
    HasExcessCacheMisses(),
    /// Configuration about the health checks.
    ExperimentConfigurations(buck2_data::SystemInfo),
}
