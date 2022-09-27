/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::sync::Arc;
use std::time::Duration;

use buck2_common::liveliness_manager::LivelinessManager;
use buck2_events::dispatch::span;
use buck2_events::dispatch::span_async;
use buck2_events::dispatch::EventDispatcher;
use indexmap::IndexMap;

use crate::artifact_value::ArtifactValue;
use crate::execute::claim::ClaimManager;
use crate::execute::claim::ClaimedRequest;
use crate::execute::kind::CommandExecutionKind;
use crate::execute::name::ExecutorName;
use crate::execute::output::CommandStdStreams;
use crate::execute::request::CommandExecutionOutput;
use crate::execute::result::CommandExecutionReport;
use crate::execute::result::CommandExecutionResult;
use crate::execute::result::CommandExecutionStatus;
use crate::execute::result::CommandExecutionTimingData;

/// This tracker helps track the information that will go into the BuckCommandExecutionMetadata
pub struct CommandExecutionManager {
    executor_name: ExecutorName,
    claimed: bool,

    pub claim_manager: Arc<dyn ClaimManager>,
    pub events: EventDispatcher,
    pub liveliness_manager: Arc<dyn LivelinessManager>,
}

impl CommandExecutionManager {
    pub fn new(
        executor_name: ExecutorName,
        claim_manager: Arc<dyn ClaimManager>,
        events: EventDispatcher,
        liveliness_manager: Arc<dyn LivelinessManager>,
    ) -> Self {
        Self {
            executor_name,
            claim_manager,
            claimed: false,
            events,
            liveliness_manager,
        }
    }

    pub fn stage<T, F: FnOnce() -> T>(
        &mut self,
        stage: impl Into<buck2_data::executor_stage_start::Stage>,
        f: F,
    ) -> T {
        let event = buck2_data::ExecutorStageStart {
            stage: Some(stage.into()),
        };

        span(event, || (f(), buck2_data::ExecutorStageEnd {}))
    }

    pub async fn stage_async<F: Future>(
        &mut self,
        stage: impl Into<buck2_data::executor_stage_start::Stage>,
        f: F,
    ) -> <F as Future>::Output {
        let event = buck2_data::ExecutorStageStart {
            stage: Some(stage.into()),
        };
        span_async(
            event,
            async move { (f.await, buck2_data::ExecutorStageEnd {}) },
        )
        .await
    }

    /// An exec_cmd request might go to multiple executors. try_claim() indicates that an executor wants to claim the work item.
    ///
    /// An executor must claim the request before making any local changes.
    /// An executor can claim the request at an earlier point (for example, to indicate it has started working on it and others shouldn't bother).
    ///
    /// This is primarily used by the hybrid executor to support sending work to both a local and a remote executor but only allowing one to actually produce a result.
    // TODO(cjhopman): Ideally there'd be some object that all local modifications of a command execution would flow through
    // and we could require executors get that object via making a claim, but that's tough right now.
    pub fn try_claim(&mut self) -> Option<ClaimedRequest> {
        if self.claim_manager.try_claim() {
            self.claimed = true;
            Some(ClaimedRequest {})
        } else {
            None
        }
    }

    pub fn claim_rejected(self) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::ClaimRejected,
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionTimingData::default(),
        )
    }

    /// Explicitly takes a ClaimedRequest here (even though it's unused) to help implementors remember to claim things
    /// since a command can't be successful without making local changes.
    pub fn success(
        self,
        claim: ClaimedRequest,
        execution_kind: CommandExecutionKind,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult {
        // just make the claim look used in the function signature.
        let _ = claim;
        self.result(
            CommandExecutionStatus::Success { execution_kind },
            outputs,
            std_streams,
            Some(0),
            timing,
        )
    }

    pub fn failure(
        self,
        execution_kind: CommandExecutionKind,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
    ) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Failure { execution_kind },
            outputs,
            std_streams,
            exit_code,
            CommandExecutionTimingData::default(),
        )
    }

    pub fn timeout(
        self,
        execution_kind: CommandExecutionKind,
        duration: Duration,
        std_streams: CommandStdStreams,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::TimedOut {
                duration,
                execution_kind,
            },
            IndexMap::new(),
            std_streams,
            None,
            timing,
        )
    }

    pub fn error(self, stage: String, error: anyhow::Error) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Error { stage, error },
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionTimingData::default(),
        )
    }

    fn result(
        self,
        status: CommandExecutionStatus,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult {
        CommandExecutionResult {
            outputs,
            report: CommandExecutionReport {
                claim: if self.claimed {
                    Some(ClaimedRequest {})
                } else {
                    None
                },
                status,
                executor: self.executor_name,
                timing,
                std_streams,
                exit_code,
            },
            rejected_execution: None,
            did_cache_upload: false,
        }
    }
}
