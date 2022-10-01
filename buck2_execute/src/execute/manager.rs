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
use crate::execute::claim::Claim;
use crate::execute::claim::ClaimManager;
use crate::execute::kind::CommandExecutionKind;
use crate::execute::output::CommandStdStreams;
use crate::execute::request::CommandExecutionOutput;
use crate::execute::result::CommandExecutionReport;
use crate::execute::result::CommandExecutionResult;
use crate::execute::result::CommandExecutionStatus;
use crate::execute::result::CommandExecutionTimingData;

trait CommandExecutionManagerLike: Sized {
    /// Create a new Command execution result.
    fn result(
        self,
        status: CommandExecutionStatus,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult;
}

/// This tracker helps track the information that will go into the BuckCommandExecutionMetadata
pub struct CommandExecutionManager {
    pub claim_manager: Box<dyn ClaimManager>,
    pub events: EventDispatcher,
    pub liveliness_manager: Arc<dyn LivelinessManager>,
}

impl CommandExecutionManager {
    pub fn new(
        claim_manager: Box<dyn ClaimManager>,
        events: EventDispatcher,
        liveliness_manager: Arc<dyn LivelinessManager>,
    ) -> Self {
        Self {
            claim_manager,
            events,
            liveliness_manager,
        }
    }

    /// Acquire a claim. This might never return if the claim has been taken.
    pub async fn claim(self) -> CommandExecutionManagerWithClaim {
        let claim = self.claim_manager.claim().await;

        CommandExecutionManagerWithClaim {
            claim,
            events: self.events,
            liveliness_manager: self.liveliness_manager,
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
}

impl CommandExecutionManagerLike for CommandExecutionManager {
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
                claim: None,
                status,
                timing,
                std_streams,
                exit_code,
            },
            rejected_execution: None,
            did_cache_upload: false,
        }
    }
}

pub struct CommandExecutionManagerWithClaim {
    pub events: EventDispatcher,
    pub liveliness_manager: Arc<dyn LivelinessManager>,
    claim: Box<dyn Claim>,
}

/// Like CommandExecutionManager but provides access to things that are only allowed with a Claim;
impl CommandExecutionManagerWithClaim {
    /// Explicitly requires a Claim here to help implementors remember to claim things since a
    /// command can't be successful without making local changes.
    pub fn success(
        self,
        execution_kind: CommandExecutionKind,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Success { execution_kind },
            outputs,
            std_streams,
            Some(0),
            timing,
        )
    }

    pub fn cancel_claim(self) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::ClaimCancelled,
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionTimingData::default(),
        )
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
}

impl CommandExecutionManagerLike for CommandExecutionManagerWithClaim {
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
                claim: Some(self.claim),
                status,
                timing,
                std_streams,
                exit_code,
            },
            rejected_execution: None,
            did_cache_upload: false,
        }
    }
}

pub trait CommandExecutionManagerExt: Sized {
    fn failure(
        self,
        execution_kind: CommandExecutionKind,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
    ) -> CommandExecutionResult;

    fn timeout(
        self,
        execution_kind: CommandExecutionKind,
        duration: Duration,
        std_streams: CommandStdStreams,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult;

    fn error(self, stage: String, error: anyhow::Error) -> CommandExecutionResult;
}

impl<T> CommandExecutionManagerExt for T
where
    T: CommandExecutionManagerLike,
{
    fn failure(
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

    fn timeout(
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

    fn error(self, stage: String, error: anyhow::Error) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Error { stage, error },
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionTimingData::default(),
        )
    }
}
