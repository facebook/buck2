/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;
use std::time::SystemTime;

use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_miniperf_proto::MiniperfCounter;
use remote_execution::ActionResultResponse;
use remote_execution::ExecuteResponse;
use remote_execution::TDirectory2;
use remote_execution::TExecutedActionMetadata;
use remote_execution::TFile;
use remote_execution::TSubsysPerfCount;
use remote_execution::TSymlink;
use remote_execution::TTimestamp;

use crate::digest_config::DigestConfig;
use crate::execute::kind::CommandExecutionKind;
use crate::execute::kind::RemoteCommandExecutionDetails;
use crate::execute::result::CommandExecutionMetadata;
use crate::re::manager::ManagedRemoteExecutionClient;
use crate::re::streams::RemoteCommandStdStreams;

pub struct ActionCacheResult(pub ActionResultResponse, pub buck2_data::CacheType);

pub trait RemoteActionResult: Send + Sync {
    fn output_files(&self) -> &[TFile];
    fn output_directories(&self) -> &[TDirectory2];
    fn output_symlinks(&self) -> &[TSymlink];

    fn execution_kind(&self, details: RemoteCommandExecutionDetails) -> CommandExecutionKind;

    /// This is only called after we inspect the action result, and the exit code is not 0
    fn execution_kind_for_failed_actions(
        &self,
        details: RemoteCommandExecutionDetails,
        materialized_inputs_for_failed: Option<Vec<ProjectRelativePathBuf>>,
        materialized_outputs_for_failed_actions: Option<Vec<ProjectRelativePathBuf>>,
    ) -> CommandExecutionKind;

    fn timing(&self) -> CommandExecutionMetadata;

    fn std_streams(
        &self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
        digest_config: DigestConfig,
    ) -> RemoteCommandStdStreams;

    /// The TTL given by RE for the outputs for this action.
    fn ttl(&self) -> i64;
}

impl RemoteActionResult for ExecuteResponse {
    fn output_files(&self) -> &[TFile] {
        &self.action_result.output_files
    }

    fn output_directories(&self) -> &[TDirectory2] {
        &self.action_result.output_directories
    }

    fn output_symlinks(&self) -> &[TSymlink] {
        &self.action_result.output_symlinks
    }

    fn execution_kind(&self, details: RemoteCommandExecutionDetails) -> CommandExecutionKind {
        self.execution_kind_for_failed_actions(details, None, None)
    }

    fn execution_kind_for_failed_actions(
        &self,
        details: RemoteCommandExecutionDetails,
        materialized_inputs_for_failed: Option<Vec<ProjectRelativePathBuf>>,
        materialized_outputs_for_failed_actions: Option<Vec<ProjectRelativePathBuf>>,
    ) -> CommandExecutionKind {
        let meta = &self.action_result.execution_metadata;
        let queue_time = meta
            .last_queued_timestamp
            .saturating_duration_since(&meta.queued_timestamp);

        CommandExecutionKind::Remote {
            details,
            queue_time,
            materialized_inputs_for_failed,
            materialized_outputs_for_failed_actions,
        }
    }

    fn timing(&self) -> CommandExecutionMetadata {
        timing_from_re_metadata(&self.action_result.execution_metadata)
    }

    fn std_streams(
        &self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
        digest_config: DigestConfig,
    ) -> RemoteCommandStdStreams {
        RemoteCommandStdStreams::new(&self.action_result, client, use_case, digest_config)
    }

    fn ttl(&self) -> i64 {
        self.action_result_ttl
    }
}

impl RemoteActionResult for ActionCacheResult {
    fn output_files(&self) -> &[TFile] {
        &self.0.action_result.output_files
    }

    fn output_directories(&self) -> &[TDirectory2] {
        &self.0.action_result.output_directories
    }

    fn output_symlinks(&self) -> &[TSymlink] {
        &self.0.action_result.output_symlinks
    }

    fn execution_kind(&self, details: RemoteCommandExecutionDetails) -> CommandExecutionKind {
        match self.1 {
            buck2_data::CacheType::ActionCache => CommandExecutionKind::ActionCache { details },
            buck2_data::CacheType::RemoteDepFileCache => {
                CommandExecutionKind::RemoteDepFileCache { details }
            }
        }
    }

    fn execution_kind_for_failed_actions(
        &self,
        details: RemoteCommandExecutionDetails,
        _materialized_inputs_for_failed: Option<Vec<ProjectRelativePathBuf>>,
        _materialized_outputs_for_failed_actions: Option<Vec<ProjectRelativePathBuf>>,
    ) -> CommandExecutionKind {
        self.execution_kind(details)
    }

    fn timing(&self) -> CommandExecutionMetadata {
        let mut timing = timing_from_re_metadata(&self.0.action_result.execution_metadata);
        // This was a cache hit so we didn't wait at all
        timing.wall_time = Duration::ZERO;
        timing.input_materialization_duration = Duration::ZERO;
        timing.queue_duration = None;
        timing
    }

    fn std_streams(
        &self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
        digest_config: DigestConfig,
    ) -> RemoteCommandStdStreams {
        RemoteCommandStdStreams::new(&self.0.action_result, client, use_case, digest_config)
    }

    fn ttl(&self) -> i64 {
        self.0.ttl
    }
}

fn timing_from_re_metadata(meta: &TExecutedActionMetadata) -> CommandExecutionMetadata {
    let execution_time = meta
        .execution_completed_timestamp
        .saturating_duration_since(&meta.execution_start_timestamp);

    let start_time = SystemTime::UNIX_EPOCH
        + meta
            .execution_start_timestamp
            .saturating_duration_since(&TTimestamp::unix_epoch());

    let execution_stats = match convert_perf_counts(meta) {
        Ok(v) => Some(v),
        Err(e) => {
            tracing::warn!("Invalid instruction counts received from RE: {:#}", e);
            None
        }
    };

    let fetch_input_time = meta
        .input_fetch_completed_timestamp
        .saturating_duration_since(&meta.input_fetch_start_timestamp);

    let queue_duration = meta
        .worker_start_timestamp
        .saturating_duration_since(&meta.queued_timestamp);

    CommandExecutionMetadata {
        wall_time: execution_time,
        execution_time,
        start_time,
        execution_stats,
        input_materialization_duration: fetch_input_time,
        hashing_duration: Duration::ZERO,
        hashed_artifacts_count: 0,
        queue_duration: Some(queue_duration),
    }
}

fn convert_perf_counts(
    meta: &TExecutedActionMetadata,
) -> buck2_error::Result<buck2_data::CommandExecutionStats> {
    Ok({
        let userspace_counter = convert_perf_count(&meta.instruction_counts.userspace_events)?;
        let kernel_counter = convert_perf_count(&meta.instruction_counts.kernel_events)?;
        buck2_data::CommandExecutionStats {
            cpu_instructions_user: userspace_counter.map(|p| p.adjusted_count()),
            cpu_instructions_kernel: kernel_counter.map(|p| p.adjusted_count()),
            userspace_events: userspace_counter.map(|p| p.to_proto()),
            kernel_events: kernel_counter.map(|p| p.to_proto()),
            memory_peak: Some(meta.max_used_mem as u64),
        }
    })
}

fn convert_perf_count(
    perf_count: &TSubsysPerfCount,
) -> buck2_error::Result<Option<MiniperfCounter>> {
    if perf_count.time_running == 0 {
        return Ok(None);
    }

    Ok(Some(MiniperfCounter {
        count: perf_count
            .count
            .try_into()
            .buck_error_context("Invalid count")?,
        time_enabled: perf_count
            .time_enabled
            .try_into()
            .buck_error_context("Invalid time_enabled")?,
        time_running: perf_count
            .time_running
            .try_into()
            .buck_error_context("Invalid time_running")?,
    }))
}
