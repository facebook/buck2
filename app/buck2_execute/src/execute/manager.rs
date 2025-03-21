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
use futures::future::Future;
use futures::future::FutureExt;
use indexmap::IndexMap;

use crate::artifact_value::ArtifactValue;
use crate::execute::claim::Claim;
use crate::execute::claim::ClaimManager;
use crate::execute::kind::CommandExecutionKind;
use crate::execute::output::CommandStdStreams;
use crate::execute::request::CommandExecutionOutput;
use crate::execute::result::CommandCancellationReason;
use crate::execute::result::CommandExecutionErrorType;
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
        additional_message: Option<String>,
    ) -> CommandExecutionResult;

    fn execution_kind(&self) -> Option<CommandExecutionKind>;
}

pub struct CommandExecutionManagerInner {
    pub claim_manager: Box<dyn ClaimManager>,
    pub events: EventDispatcher,
    pub liveliness_observer: Arc<dyn LivelinessObserver>,
    pub intend_to_fallback_on_failure: bool,
    pub execution_kind: Option<CommandExecutionKind>,
}

/// This tracker helps track the information that will go into the BuckCommandExecutionMetadata
pub struct CommandExecutionManager {
    pub inner: Box<CommandExecutionManagerInner>,
}

impl CommandExecutionManager {
    pub fn new(
        claim_manager: Box<dyn ClaimManager>,
        events: EventDispatcher,
        liveliness_observer: Arc<dyn LivelinessObserver>,
    ) -> Self {
        Self {
            inner: Box::new(CommandExecutionManagerInner {
                claim_manager,
                events,
                liveliness_observer,
                intend_to_fallback_on_failure: false,
                execution_kind: None,
            }),
        }
    }

    /// Acquire a claim. This might never return if the claim has been taken.
    pub fn claim(self) -> impl Future<Output = CommandExecutionManagerWithClaim> {
        let events = self.inner.events;
        let liveliness_observer = self.inner.liveliness_observer;
        let execution_kind = self.inner.execution_kind;
        self.inner
            .claim_manager
            .claim()
            .map(|claim| CommandExecutionManagerWithClaim {
                inner: Box::new(CommandExecutionManagerWithClaimInner {
                    claim,
                    events,
                    liveliness_observer,
                    execution_kind,
                }),
            })
    }

    pub fn on_result_delayed(&mut self) {
        self.inner.claim_manager.on_result_delayed();
    }

    pub fn cancel(self, reason: Option<CommandCancellationReason>) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Cancelled { reason },
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionMetadata::default(),
            None,
        )
    }

    pub fn with_intend_to_fallback_on_failure(
        mut self,
        intend_to_fallback_on_failure: bool,
    ) -> Self {
        self.inner.intend_to_fallback_on_failure = intend_to_fallback_on_failure;
        self
    }

    pub fn with_execution_kind(mut self, execution_kind: CommandExecutionKind) -> Self {
        self.inner.execution_kind = Some(execution_kind);
        self
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
        additional_message: Option<String>,
    ) -> CommandExecutionResult {
        CommandExecutionResult {
            outputs,
            report: CommandExecutionReport {
                claim: None,
                status,
                timing,
                std_streams,
                exit_code,
                additional_message,
            },
            rejected_execution: None,
            did_cache_upload: false,
            did_dep_file_cache_upload: false,
            dep_file_key: None,
            eligible_for_full_hybrid: false,
            dep_file_metadata: None,
            action_result: None,
        }
    }

    fn execution_kind(&self) -> Option<CommandExecutionKind> {
        self.inner.execution_kind.clone()
    }
}

pub struct CommandExecutionManagerWithClaimInner {
    pub events: EventDispatcher,
    pub liveliness_observer: Arc<dyn LivelinessObserver>,
    pub execution_kind: Option<CommandExecutionKind>,
    claim: Box<dyn Claim>,
}

pub struct CommandExecutionManagerWithClaim {
    pub inner: Box<CommandExecutionManagerWithClaimInner>,
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
            None,
        )
    }

    pub fn cancel_claim(self) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Cancelled { reason: None },
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionMetadata::default(),
            None,
        )
    }

    pub fn with_execution_kind(mut self, execution_kind: CommandExecutionKind) -> Self {
        self.inner.execution_kind = Some(execution_kind);
        self
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
        additional_message: Option<String>,
    ) -> CommandExecutionResult {
        CommandExecutionResult {
            outputs,
            report: CommandExecutionReport {
                claim: Some(self.inner.claim),
                status,
                timing,
                std_streams,
                exit_code,
                additional_message,
            },
            rejected_execution: None,
            did_cache_upload: false,
            did_dep_file_cache_upload: false,
            dep_file_key: None,
            eligible_for_full_hybrid: false,
            dep_file_metadata: None,
            action_result: None,
        }
    }

    fn execution_kind(&self) -> Option<CommandExecutionKind> {
        self.inner.execution_kind.clone()
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
        additional_message: Option<String>,
    ) -> CommandExecutionResult;

    fn worker_failure(
        self,
        execution_kind: CommandExecutionKind,
        stderr: String,
        timing: CommandExecutionMetadata,
    ) -> CommandExecutionResult;

    fn timeout(
        self,
        execution_kind: CommandExecutionKind,
        duration: Duration,
        std_streams: CommandStdStreams,
        timing: CommandExecutionMetadata,
        additional_message: Option<String>,
    ) -> CommandExecutionResult;

    fn error(
        self,
        stage: &'static str,
        error: impl Into<buck2_error::Error>,
    ) -> CommandExecutionResult {
        self.error_classified(stage, error, CommandExecutionErrorType::Other)
    }

    fn error_classified(
        self,
        stage: &'static str,
        error: impl Into<buck2_error::Error>,
        error_type: CommandExecutionErrorType,
    ) -> CommandExecutionResult;
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
        additional_message: Option<String>,
    ) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Failure { execution_kind },
            outputs,
            std_streams,
            exit_code,
            timing,
            additional_message,
        )
    }

    fn worker_failure(
        self,
        execution_kind: CommandExecutionKind,
        stderr: String,
        timing: CommandExecutionMetadata,
    ) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::WorkerFailure { execution_kind },
            Default::default(),
            CommandStdStreams::Local {
                stdout: Default::default(),
                stderr: stderr.into_bytes(),
            },
            None,
            timing,
            None,
        )
    }

    fn timeout(
        self,
        execution_kind: CommandExecutionKind,
        duration: Duration,
        std_streams: CommandStdStreams,
        timing: CommandExecutionMetadata,
        additional_message: Option<String>,
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
            additional_message,
        )
    }

    fn error_classified(
        self,
        stage: &'static str,
        error: impl Into<buck2_error::Error>,
        error_type: CommandExecutionErrorType,
    ) -> CommandExecutionResult {
        let execution_kind = self.execution_kind();
        self.result(
            CommandExecutionStatus::Error {
                stage,
                error: error.into(),
                execution_kind,
                typ: error_type,
            },
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionMetadata::default(),
            None,
        )
    }
}
