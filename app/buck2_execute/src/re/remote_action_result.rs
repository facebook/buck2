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
use buck2_common::executor_config::RemoteExecutorUseCase;
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
use crate::execute::action_digest::ActionDigest;
use crate::execute::kind::CommandExecutionKind;
use crate::execute::result::CommandExecutionMetadata;
use crate::re::manager::ManagedRemoteExecutionClient;
use crate::re::streams::RemoteCommandStdStreams;

pub trait RemoteActionResult: Send + Sync {
    fn output_files(&self) -> &[TFile];
    fn output_directories(&self) -> &[TDirectory2];

    fn execution_kind(&self, digest: ActionDigest) -> CommandExecutionKind;

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

    fn execution_kind(&self, digest: ActionDigest) -> CommandExecutionKind {
        CommandExecutionKind::Remote { digest }
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

impl RemoteActionResult for ActionResultResponse {
    fn output_files(&self) -> &[TFile] {
        &self.action_result.output_files
    }

    fn output_directories(&self) -> &[TDirectory2] {
        &self.action_result.output_directories
    }

    fn execution_kind(&self, digest: ActionDigest) -> CommandExecutionKind {
        CommandExecutionKind::ActionCache { digest }
    }

    fn timing(&self) -> CommandExecutionMetadata {
        let mut timing = timing_from_re_metadata(&self.action_result.execution_metadata);
        timing.wall_time = Duration::ZERO; // This was a cache hit so we didn't wait.
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

fn timing_from_re_metadata(meta: &TExecutedActionMetadata) -> CommandExecutionMetadata {
    let execution_time = meta
        .execution_completed_timestamp
        .saturating_duration_since(&meta.execution_start_timestamp);

    let re_queue_time = meta
        .last_queued_timestamp
        .saturating_duration_since(&meta.queued_timestamp);

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

    CommandExecutionMetadata {
        wall_time: execution_time,
        re_queue_time: Some(re_queue_time),
        execution_time,
        start_time,
        execution_stats,
    }
}

fn convert_perf_counts(
    perf_counts: &TPerfCount,
) -> anyhow::Result<buck2_data::CommandExecutionStats> {
    Ok(buck2_data::CommandExecutionStats {
        cpu_instructions_user: convert_perf_count(&perf_counts.userspace_events)?
            .map(|p| p.adjusted_count()),
        cpu_instructions_kernel: convert_perf_count(&perf_counts.kernel_events)?
            .map(|p| p.adjusted_count()),
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
