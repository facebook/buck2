/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use events::dispatch::EventDispatcher;
use futures::{pin_mut, Future, FutureExt};
use gazebo::prelude::*;
use remote_execution as RE;

use crate::execute::{
    commands::{
        local::LocalExecutor, re::ReExecutor, ActionResultStatus, ClaimManager,
        CommandExecutionManager, CommandExecutionResult, ExecutorName, PreparedCommand,
        PreparedCommandExecutor,
    },
    HybridExecutionLevel,
};

/// The [HybridExecutor] will accept requests and dispatch them to both a local and remote delegate
/// executor, unless the CommandExecutionRequest expresses a preference. That will allow them to
/// race and whichever claims the request first will get to execute it.
///
/// If the remote executor claims the request and fails, we will enqueue the request again to the
/// local executor.
///
/// TODO(cjhopman): local fallback should probably be configurable. It should be as simple as not
/// wrapping up the initial remote_result with the fallback stuff below.
pub struct HybridExecutor {
    pub local: LocalExecutor,
    pub remote: ReExecutor,
    pub level: HybridExecutionLevel,
    pub prefer_local: bool,
}

impl HybridExecutor {
    async fn local_exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        claim_manager: Arc<dyn ClaimManager>,
        events: EventDispatcher,
    ) -> CommandExecutionResult {
        let local_manager = CommandExecutionManager::new(self.local.name(), claim_manager, events);
        self.local.exec_cmd(command, local_manager).await
    }

    async fn remote_exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        claim_manager: Arc<dyn ClaimManager>,
        events: EventDispatcher,
    ) -> CommandExecutionResult {
        let remote_manager =
            CommandExecutionManager::new(self.remote.name(), claim_manager, events);
        self.remote.exec_cmd(command, remote_manager).await
    }
}

#[async_trait]
impl PreparedCommandExecutor for HybridExecutor {
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        // inspect that and construct our own result. Especially in the case of secondary fallback,
        // the current approach effectively loses the data from the primary result (for example, we
        // should have stage events the reflect the full duration not just the fallback part).

        // Note that this only sets up these futures, nothing will happen until they are awaited (this is important in the
        // case where we shouldn't be sending one of them).
        let local_result =
            self.local_exec_cmd(command, manager.claim_manager.dupe(), manager.events.dupe());
        let remote_result =
            self.remote_exec_cmd(command, manager.claim_manager.dupe(), manager.events.dupe());

        if command.request.local_only() || self.remote.is_action_too_large(&command.action_paths) {
            return local_result.await;
        };

        let local_result = local_result.left_future();
        let remote_result = remote_result.right_future();

        let primary_result;
        let secondary_result;
        let fallback_executor;
        let fallback_executor_name;

        if self.prefer_local {
            primary_result = local_result;
            secondary_result = remote_result;
            fallback_executor = &self.remote as &dyn PreparedCommandExecutor;
            fallback_executor_name = ExecutorName("remote_fallback");
        } else {
            primary_result = remote_result;
            secondary_result = local_result;
            fallback_executor = &self.local as &dyn PreparedCommandExecutor;
            fallback_executor_name = ExecutorName("local_fallback");
        }

        let fallback_only = match self.level {
            HybridExecutionLevel::Limited => return primary_result.await,
            HybridExecutionLevel::Fallback => true,
            HybridExecutionLevel::Full => false,
        };

        let primary_result_with_fallback = with_fallback(
            command,
            manager,
            primary_result,
            fallback_executor,
            fallback_executor_name,
        );

        if fallback_only {
            return primary_result_with_fallback.await;
        }

        // fuse and pin these so we can select over them.
        let secondary_result = secondary_result.fuse();
        let primary_result_with_fallback = primary_result_with_fallback.fuse();
        pin_mut!(secondary_result);
        pin_mut!(primary_result_with_fallback);

        // The loop+complete allows us a simple way to ignore a result if it has a ClaimRejected status. It should be
        // impossible for both of them to finish with that status.
        #[allow(clippy::mut_mut)] // The select! uses a &mut &mut
        loop {
            futures::select! {
                secondary = secondary_result => {
                    match secondary.metadata().status {
                        ActionResultStatus::ClaimRejected => {},
                        _ => {
                            // We don't wait for the primary execution result to finish. We depend on the ClaimManager
                            // (and the primary executor to properly use it) to ensure that the primary executor doesn't
                            // have any side effects.
                            return secondary;
                        }
                    }
                }
                primary = primary_result_with_fallback => {
                    match primary.metadata().status {
                        ActionResultStatus::ClaimRejected => {},
                        _ => {
                            // We don't wait for the secondary execution result to finish as it may
                            // be sitting in a queue for a long time. We depend on the ClaimManager
                            // (and the secondary executor to properly use it) to ensure that the
                            // secondary executor doesn't have any side effects.
                            return primary;
                        }
                    }
                }
                complete => {
                    panic!("should've returned results from one of the executors.")
                }
            };
        }
    }

    fn re_platform(&self) -> Option<&RE::Platform> {
        self.remote.re_platform()
    }

    fn name(&self) -> ExecutorName {
        match self.level {
            HybridExecutionLevel::Limited => ExecutorName("limited-hybrid"),
            HybridExecutionLevel::Fallback => ExecutorName("fallback-hybrid"),
            HybridExecutionLevel::Full => ExecutorName("hybrid"),
        }
    }
}

/// Wraps the remote_result with fallback-handling.
async fn with_fallback<F: Future<Output = CommandExecutionResult>>(
    command: &PreparedCommand<'_, '_>,
    manager: CommandExecutionManager,
    inner_result: F,
    fallback: &dyn PreparedCommandExecutor,
    fallback_name: ExecutorName,
) -> CommandExecutionResult {
    let mut result = inner_result.await;

    match result.metadata().status {
        // If the claim is rejected, fallback has already started on it. If the execution is successful, use the result.
        super::ActionResultStatus::ClaimRejected | super::ActionResultStatus::Success { .. } => {
            result
        }
        // For either failure type:
        // if the request has been claimed, log something and fall back to a new fallback execution (since the
        // earlier one is either already or will be cancelled when it can't get the claim)
        // if the request was not claimed by the primary execution, return ClaimRejected so that the hybrid
        // select below will ignore this result.
        super::ActionResultStatus::Failure { .. }
        | super::ActionResultStatus::Error(_, _)
        | super::ActionResultStatus::TimedOut { .. } => {
            if result.metadata().claim.is_some() {
                let fallback_manager = CommandExecutionManager::new(
                    // TODO(cjhopman): This should probably use the fallback executor's name, but
                    // ExecutorName wants a &'static str
                    fallback_name,
                    // The primary executor has already successfully made a claim, so pass a new ClaimManager
                    <dyn ClaimManager>::new_simple(),
                    manager.events.dupe(),
                );
                fallback.exec_cmd(command, fallback_manager).await
            } else {
                // TODO(cjhopman): This isn't a great approach. Maybe the remote_result_with_fallback should
                // return a different type so that we can explicitly signal this case.
                result.metadata.status = ActionResultStatus::ClaimRejected;
                result
            }
        }
    }
}
