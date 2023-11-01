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

use anyhow::Context as _;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_miniperf_proto::MiniperfCounter;
use remote_execution::ActionResultResponse;
use remote_execution::ExecuteResponse;
use remote_execution::TDirectory2;
use remote_execution::TExecutedActionMetadata;
use remote_execution::TFile;
use remote_execution::TPerfCount;
use remote_execution::TSubsysPerfCount;
use remote_execution::TTimestamp;

use crate::digest_config::DigestConfig;
use crate::execute::kind::CommandExecutionKind;
use crate::execute::kind::RemoteCommandExecutionDetails;
use crate::execute::result::CommandExecutionMetadata;
use crate::re::manager::ManagedRemoteExecutionClient;
use crate::re::streams::RemoteCommandStdStreams;

pub struct RemoteDepFileResult(pub ActionResultResponse);

pub trait RemoteActionResult: Send + Sync {
    fn output_files(&self) -> &[TFile];
    fn output_directories(&self) -> &[TDirectory2];

    fn execution_kind(&self, details: RemoteCommandExecutionDetails) -> CommandExecutionKind;

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

    fn execution_kind(&self, details: RemoteCommandExecutionDetails) -> CommandExecutionKind {
        let meta = &self.action_result.execution_metadata;
        let queue_time = meta
            .last_queued_timestamp
            .saturating_duration_since(&meta.queued_timestamp);

        CommandExecutionKind::Remote {
            details,
            queue_time,
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

impl RemoteActionResult for Box<dyn RemoteActionResult> {
    fn output_files(&self) -> &[TFile] {
        self.as_ref().output_files()
    }

    fn output_directories(&self) -> &[TDirectory2] {
        self.as_ref().output_directories()
    }

    fn execution_kind(&self, details: RemoteCommandExecutionDetails) -> CommandExecutionKind {
        self.as_ref().execution_kind(details)
    }

    fn timing(&self) -> CommandExecutionMetadata {
        self.as_ref().timing()
    }

    fn std_streams(
        &self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
        digest_config: DigestConfig,
    ) -> RemoteCommandStdStreams {
        self.as_ref().std_streams(client, use_case, digest_config)
    }

    fn ttl(&self) -> i64 {
        self.as_ref().ttl()
    }
}

impl RemoteActionResult for ActionResultResponse {
    fn output_files(&self) -> &[TFile] {
        &self.action_result.output_files
    }

    fn output_directories(&self) -> &[TDirectory2] {
        &self.action_result.output_directories
    }

    fn execution_kind(&self, details: RemoteCommandExecutionDetails) -> CommandExecutionKind {
        CommandExecutionKind::ActionCache { details }
    }

    fn timing(&self) -> CommandExecutionMetadata {
        let mut timing = timing_from_re_metadata(&self.action_result.execution_metadata);
        timing.wall_time = Duration::ZERO; // This was a cache hit so we didn't wait.
        timing.input_materialization_duration = Duration::ZERO; // This was a cache hit so we didn't wait.
        timing
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
        self.ttl
    }
}

impl RemoteActionResult for RemoteDepFileResult {
    fn output_files(&self) -> &[TFile] {
        self.0.output_files()
    }

    fn output_directories(&self) -> &[TDirectory2] {
        self.0.output_directories()
    }

    fn execution_kind(&self, details: RemoteCommandExecutionDetails) -> CommandExecutionKind {
        CommandExecutionKind::RemoteDepFileCache { details }
    }

    fn timing(&self) -> CommandExecutionMetadata {
        self.0.timing()
    }

    fn std_streams(
        &self,
        client: &ManagedRemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
        digest_config: DigestConfig,
    ) -> RemoteCommandStdStreams {
        self.0.std_streams(client, use_case, digest_config)
    }

    fn ttl(&self) -> i64 {
        self.0.ttl()
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

    let execution_stats = match convert_perf_counts(&meta.instruction_counts) {
        Ok(v) => Some(v),
        Err(e) => {
            tracing::warn!("Invalid instruction counts received from RE: {:#}", e);
            None
        }
    };

    let fetch_input_time = meta
        .input_fetch_completed_timestamp
        .saturating_duration_since(&meta.input_fetch_start_timestamp);

    CommandExecutionMetadata {
        wall_time: execution_time,
        execution_time,
        start_time,
        execution_stats,
        input_materialization_duration: fetch_input_time,
        hashing_duration: Duration::ZERO,
    }
}

fn convert_perf_counts(
    perf_counts: &TPerfCount,
) -> anyhow::Result<buck2_data::CommandExecutionStats> {
    Ok({
        let userspace_counter = convert_perf_count(&perf_counts.userspace_events)?;
        let kernel_counter = convert_perf_count(&perf_counts.kernel_events)?;
        buck2_data::CommandExecutionStats {
            cpu_instructions_user: userspace_counter.map(|p| p.adjusted_count()),
            cpu_instructions_kernel: kernel_counter.map(|p| p.adjusted_count()),
            userspace_events: userspace_counter.map(|p| p.to_proto()),
            kernel_events: kernel_counter.map(|p| p.to_proto()),
        }
    })
}

fn convert_perf_count(perf_count: &TSubsysPerfCount) -> anyhow::Result<Option<MiniperfCounter>> {
    if perf_count.time_running == 0 {
        return Ok(None);
    }

    Ok(Some(MiniperfCounter {
        count: perf_count.count.try_into().context("Invalid count")?,
        time_enabled: perf_count
            .time_enabled
            .try_into()
            .context("Invalid time_enabled")?,
        time_running: perf_count
            .time_running
            .try_into()
            .context("Invalid time_running")?,
    }))
}
