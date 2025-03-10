/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(dead_code)]

/// A subset of the client data that is relevant for health checks.
/// This is intentionally kept as a small set to avoid serialization costs.
#[derive(Default)]
pub(crate) struct HealthCheckContext {
    /// The unique trace id for the command.
    pub trace_id: String,

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
