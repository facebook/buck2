/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;

use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_events::dispatch::EventDispatcher;
use indexmap::IndexMap;

use crate::artifact_value::ArtifactValue;
use crate::execute::claim::Claim;
use crate::execute::claim::ClaimManager;
use crate::execute::kind::CommandExecutionKind;
use crate::execute::output::CommandStdStreams;
use crate::execute::request::CommandExecutionOutput;
use crate::execute::result::CommandExecutionMetadata;
use crate::execute::result::CommandExecutionReport;
use crate::execute::result::CommandExecutionResult;
use crate::execute::result::CommandExecutionStatus;

trait CommandExecutionManagerLike: Sized {
    /// Create a new Command execution result.
    fn result(
        self,
        status: CommandExecutionStatus,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
        timing: CommandExecutionMetadata,
    ) -> CommandExecutionResult;
}

/// This tracker helps track the information that will go into the BuckCommandExecutionMetadata
pub struct CommandExecutionManager {
    pub claim_manager: Box<dyn ClaimManager>,
    pub events: EventDispatcher,
    pub liveliness_observer: Arc<dyn LivelinessObserver>,
}

impl CommandExecutionManager {
    pub fn new(
        claim_manager: Box<dyn ClaimManager>,
        events: EventDispatcher,
        liveliness_observer: Arc<dyn LivelinessObserver>,
    ) -> Self {
        Self {
            claim_manager,
            events,
            liveliness_observer,
        }
    }

    /// Acquire a claim. This might never return if the claim has been taken.
    pub async fn claim(self) -> CommandExecutionManagerWithClaim {
        let claim = self.claim_manager.claim().await;

        CommandExecutionManagerWithClaim {
            claim,
            events: self.events,
            liveliness_observer: self.liveliness_observer,
        }
    }

    pub fn on_result_delayed(&mut self) {
        self.claim_manager.on_result_delayed();
    }

    pub fn cancel(self) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::ClaimCancelled,
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionMetadata::default(),
        )
    }
}

impl CommandExecutionManagerLike for CommandExecutionManager {
    fn result(
        self,
        status: CommandExecutionStatus,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
        timing: CommandExecutionMetadata,
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
            eligible_for_full_hybrid: false,
        }
    }
}

pub struct CommandExecutionManagerWithClaim {
    pub events: EventDispatcher,
    pub liveliness_observer: Arc<dyn LivelinessObserver>,
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
        timing: CommandExecutionMetadata,
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
            CommandExecutionMetadata::default(),
        )
    }
}

impl CommandExecutionManagerLike for CommandExecutionManagerWithClaim {
    fn result(
        self,
        status: CommandExecutionStatus,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
        timing: CommandExecutionMetadata,
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
            eligible_for_full_hybrid: false,
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
        timing: CommandExecutionMetadata,
    ) -> CommandExecutionResult;

    fn timeout(
        self,
        execution_kind: CommandExecutionKind,
        duration: Duration,
        std_streams: CommandStdStreams,
        timing: CommandExecutionMetadata,
    ) -> CommandExecutionResult;

    fn error(self, stage: &'static str, error: impl Into<anyhow::Error>) -> CommandExecutionResult;
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
        timing: CommandExecutionMetadata,
    ) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Failure { execution_kind },
            outputs,
            std_streams,
            exit_code,
            timing,
        )
    }

    fn timeout(
        self,
        execution_kind: CommandExecutionKind,
        duration: Duration,
        std_streams: CommandStdStreams,
        timing: CommandExecutionMetadata,
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

    fn error(self, stage: &'static str, error: impl Into<anyhow::Error>) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Error {
                stage,
                error: error.into(),
            },
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionMetadata::default(),
        )
    }
}
