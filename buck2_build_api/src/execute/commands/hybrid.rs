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
use futures::future;
use futures::future::Either;
use futures::future::Future;
use futures::FutureExt;
use gazebo::prelude::*;
use remote_execution as RE;

use crate::execute::commands::local::LocalExecutor;
use crate::execute::commands::re::ReExecutor;
use crate::execute::commands::ClaimManager;
use crate::execute::commands::CommandExecutionManager;
use crate::execute::commands::CommandExecutionResult;
use crate::execute::commands::CommandExecutionStatus;
use crate::execute::commands::ExecutorName;
use crate::execute::commands::PreparedCommand;
use crate::execute::commands::PreparedCommandExecutor;
use crate::execute::HybridExecutionLevel;

/// The [HybridExecutor] will accept requests and dispatch them to both a local and remote delegate
/// executor, unless the CommandExecutionRequest expresses a preference. That will allow them to
/// race and whichever claims the request first will get to execute it.
///
/// If the remote executor claims the request but does not produce a successful response, we will
/// enqueue the request again to the local executor.
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

        let remote = Executable {
            job: remote_result,
            fallback: FallbackExecutor {
                executor: &self.local as _,
                name: ExecutorName("local_fallback"),
            },
        };

        let local = Executable {
            job: local_result,
            fallback: FallbackExecutor {
                executor: &self.remote as _,
                name: ExecutorName("remote_fallback"),
            },
        };

        let (primary, secondary) = if self.prefer_local {
            (local, remote)
        } else {
            (remote, local)
        };

        let (fallback_only, fallback_on_failure) = match self.level {
            HybridExecutionLevel::Limited => return primary.job.await,
            HybridExecutionLevel::Fallback {
                fallback_on_failure,
            } => (true, fallback_on_failure),
            HybridExecutionLevel::Full {
                fallback_on_failure,
            } => (false, fallback_on_failure),
        };

        let is_retryable_status = move |r: &CommandExecutionResult| {
            match r.metadata().status {
                // This doesn't really matter sicne we only ever pass this with statuses known /
                // expected to not be ClaimRejected.
                super::CommandExecutionStatus::ClaimRejected => false,
                // If the execution is successful, use the result.
                super::CommandExecutionStatus::Success { .. } => false,
                // Retry commands that failed (i.e. exit 1) only if we're instructed to do so.
                super::CommandExecutionStatus::Failure { .. } => fallback_on_failure,
                // Errors are infra errors and are always retried because that is the point of
                // falling back.
                super::CommandExecutionStatus::Error { .. }
                | super::CommandExecutionStatus::TimedOut { .. } => true,
            }
        };

        if fallback_only {
            return exec_with_restart(
                command,
                manager,
                primary.into_future(),
                &is_retryable_status,
            )
            .await;
        }

        let primary = primary.into_future();
        let secondary = secondary.into_future();
        futures::pin_mut!(primary);
        futures::pin_mut!(secondary);

        let (first, second) =
            Either::factor_first(futures::future::select(primary, secondary).await);

        // Note: We discard the fallback this yields here. That's because we already kicked off
        // that executor in `second` so we'll just reuse that future.
        let (first_res, first_fallback) = first;

        if is_claim_rejected(&first_res) {
            // If the first result was rejected then wait for the second one and then if that
            // provides a status we must retry, we'll use its fallback executor (which is the one
            // that just produced the rejected claim).
            exec_with_restart(command, manager, second, &is_retryable_status).await
        } else if is_retryable_status(&first_res) {
            // If the first result is retryable, then we have to watch out for the case where the
            // second executor returns a rejected claim (meaning it didn't execute). When that
            // happens, we should retry it on that executor itself, which is the first fallback we
            // received.
            let (second_res, _unused_fallback) = second.await;
            exec_with_restart(
                command,
                manager,
                future::ready((second_res, first_fallback)),
                is_claim_rejected,
            )
            .await
        } else {
            // Everyone is happy, we got our result.
            first_res
        }
    }

    fn re_platform(&self) -> Option<&RE::Platform> {
        self.remote.re_platform()
    }

    fn name(&self) -> ExecutorName {
        match self.level {
            HybridExecutionLevel::Limited => ExecutorName("limited-hybrid"),
            HybridExecutionLevel::Fallback { .. } => ExecutorName("fallback-hybrid"),
            HybridExecutionLevel::Full { .. } => ExecutorName("hybrid"),
        }
    }
}

fn is_claim_rejected(res: &CommandExecutionResult) -> bool {
    match res.metadata().status {
        CommandExecutionStatus::ClaimRejected => true,
        _ => false,
    }
}

/// Execute `exec`, and restart on the FallbackExecutor it yields if `should_restart` passes.
async fn exec_with_restart<S>(
    command: &PreparedCommand<'_, '_>,
    manager: CommandExecutionManager,
    exec: impl Future<Output = (CommandExecutionResult, FallbackExecutor<'_>)>,
    should_restart: S,
) -> CommandExecutionResult
where
    S: FnOnce(&CommandExecutionResult) -> bool,
{
    let (result, fallback) = exec.await;

    if !should_restart(&result) {
        return result;
    }

    let fallback_manager = CommandExecutionManager::new(
        fallback.name,
        // We already obtained a result here so we need a new claim to allow it to proceed.
        <dyn ClaimManager>::new_simple(),
        manager.events.dupe(),
    );

    fallback.executor.exec_cmd(command, fallback_manager).await
}

struct Executable<'a, F> {
    job: F,
    fallback: FallbackExecutor<'a>,
}

struct FallbackExecutor<'a> {
    executor: &'a dyn PreparedCommandExecutor,
    name: ExecutorName,
}

impl<'a, F> Executable<'a, F>
where
    F: Future,
{
    async fn into_future(self) -> (<F as Future>::Output, FallbackExecutor<'a>) {
        (self.job.await, self.fallback)
    }
}
