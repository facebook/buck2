/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::ControlFlow;
use std::sync::Arc;

use async_trait::async_trait;
use more_futures::cancellation::CancellationContext;
use remote_execution as RE;

use crate::digest_config::DigestConfig;
use crate::execute::action_digest::ActionDigest;
use crate::execute::blobs::ActionBlobs;
use crate::execute::manager::CommandExecutionManager;
use crate::execute::request::CommandExecutionRequest;
use crate::execute::request::ExecutorPreference;
use crate::execute::result::CommandExecutionResult;
use crate::execute::target::CommandExecutionTarget;

pub struct PreparedAction {
    pub action: ActionDigest,
    // The encoded action and other messages referenced from it by digest (e.g. RE::Command).
    // Does not include the files referenced in inputs.
    pub blobs: ActionBlobs,
    pub platform: RE::Platform,
}

pub struct PreparedCommand<'a, 'b> {
    pub request: &'a CommandExecutionRequest,
    pub target: &'b dyn CommandExecutionTarget,
    pub prepared_action: &'a PreparedAction,
    pub digest_config: DigestConfig,
}

#[async_trait]
pub trait PreparedCommandExecutor: Send + Sync {
    /// Execute a command.
    ///
    /// This intentionally does not return a Result since we want to capture information about the
    /// execution even if there are errors. Any errors can be propagated by converting them
    /// to a result with CommandExecutionManager::error.
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
        cancellations: &CancellationContext,
    ) -> CommandExecutionResult;

    /// Checks if there is any possibility for a command with a given executor preference to
    /// be executed locally.
    fn is_local_execution_possible(&self, executor_preference: ExecutorPreference) -> bool;
}

#[async_trait]
pub trait PreparedCommandOptionalExecutor: Send + Sync {
    /// Take a command and evaluate whether it needs to be actually executed (locally or remotely) or can be skipped.
    /// In the skip case, this should handle all the things that would happen in an actual execution (materialization, output declaration, etc.).
    ///
    /// Given a command, evaluate whether the execution can be skipped. (for example because it is already cached)
    /// If it can be skipped, return a CommandExecutionResult that can be used as if the action was just executed.
    /// Otherwise, return a CommandExecutionManager that can be used to execute the action.
    async fn maybe_execute(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
        cancellations: &CancellationContext,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager>;
}

#[async_trait]
impl PreparedCommandOptionalExecutor for Arc<dyn PreparedCommandOptionalExecutor> {
    async fn maybe_execute(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
        cancellations: &CancellationContext,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        (**self)
            .maybe_execute(command, manager, cancellations)
            .await
    }
}

// When we don't want to check a command can be skipped, just use the NoOpCommandExecutor that always returns the continue case.
pub struct NoOpCommandExecutor {}

#[async_trait]
impl PreparedCommandOptionalExecutor for NoOpCommandExecutor {
    async fn maybe_execute(
        &self,
        _command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
        _cancellations: &CancellationContext,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        ControlFlow::Continue(manager)
    }
}
