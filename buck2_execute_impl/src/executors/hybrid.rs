/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_common::executor_config::HybridExecutionLevel;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::liveliness_manager::CancelledLivelinessGuard;
use buck2_common::liveliness_manager::LivelinessGuard;
use buck2_common::liveliness_manager::LivelinessManager;
use buck2_common::liveliness_manager::LivelinessManagerExt;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::execute::claim::Claim;
use buck2_execute::execute::claim::ClaimManager;
use buck2_execute::execute::claim::MutexClaimManager;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::manager::CommandExecutionManagerExt;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionStatus;
use derivative::Derivative;
use futures::future::BoxFuture;
use futures::future::Either;
use futures::future::Future;
use futures::FutureExt;
use gazebo::prelude::*;
use host_sharing::HostSharingRequirements;
use host_sharing::WeightClass;
use remote_execution as RE;

use crate::executors::local::LocalExecutor;
use crate::executors::re::ReExecutor;
use crate::low_pass_filter::LowPassFilter;

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
    pub executor_preference: ExecutorPreference,
    pub low_pass_filter: Arc<LowPassFilter>,
}

impl HybridExecutor {
    async fn local_exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        claim_manager: Box<dyn ClaimManager>,
        events: EventDispatcher,
        liveliness_manager: Arc<dyn LivelinessManager>,
    ) -> CommandExecutionResult {
        let local_manager = CommandExecutionManager::new(claim_manager, events, liveliness_manager);
        self.local.exec_cmd(command, local_manager).await
    }

    async fn remote_exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        claim_manager: Box<dyn ClaimManager>,
        events: EventDispatcher,
        liveliness_manager: Arc<dyn LivelinessManager>,
    ) -> CommandExecutionResult {
        let remote_manager =
            CommandExecutionManager::new(claim_manager, events, liveliness_manager);
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
        let executor_preference = self
            .executor_preference
            .and(&command.request.executor_preference());

        let executor_preference = match executor_preference {
            Ok(executor_preference) => executor_preference,
            Err(e) => return manager.error("prepare_hybrid", e),
        };

        // inspect that and construct our own result. Especially in the case of secondary fallback,
        // the current approach effectively loses the data from the primary result (for example, we
        // should have stage events the reflect the full duration not just the fallback part).

        // Construct our claim manager and a liveniless guard for local commands. The way this
        // works is as follows: when RE takes a claim, we cancel local commands. This means that we
        // can truly race RE and local: if RE finishes even after local started, it'll cancel the
        // local execution, we'll get back here with a ClaimCancelled from local execution, cancel
        // local's claim, and then resume RE.
        let (local_execution_liveliness_manager, local_execution_liveliness_guard) =
            LivelinessGuard::create();

        // Used to monitor that the RE execution is alive.
        let (remote_execution_liveliness_manager, remote_execution_liveliness_guard) =
            LivelinessGuard::create();

        let claim_manager = MutexClaimManager::new();

        // Note that this only sets up these futures, nothing will happen until they are awaited
        // (this is important in the case where we shouldn't be sending one of them).
        let local_result = self.local_exec_cmd(
            command,
            box claim_manager.dupe(),
            manager.events.dupe(),
            Arc::new(
                manager
                    .liveliness_manager
                    .dupe()
                    .and(local_execution_liveliness_manager.dupe()),
            ),
        );

        let remote_result = self.remote_exec_cmd(
            command,
            box ReClaimManager::new(
                local_execution_liveliness_guard,
                box claim_manager,
                remote_execution_liveliness_guard,
            ),
            manager.events.dupe(),
            manager.liveliness_manager.dupe(),
        );

        if executor_preference.requires_local()
            || self.remote.is_action_too_large(&command.action_paths)
        {
            return local_result.await;
        };

        if executor_preference.requires_remote() {
            return remote_result.await;
        }

        let jobs = HybridExecutorJobs {
            local: local_result.map(|r| (r, JobPriority(1))),
            remote: remote_result.map(|r| (r, JobPriority(0))),
            executor_preference,
        };

        let (is_limited, fallback_only, fallback_on_failure, low_pass_filter) = match self.level {
            HybridExecutionLevel::Limited => (true, false, false, false),
            HybridExecutionLevel::Fallback {
                fallback_on_failure,
            } => (false, true, fallback_on_failure, false),
            HybridExecutionLevel::Full {
                fallback_on_failure,
                low_pass_filter,
            } => (false, false, fallback_on_failure, low_pass_filter),
        };

        if is_limited {
            return jobs.into_primary().await.0;
        }

        let weight = match command.request.host_sharing_requirements() {
            HostSharingRequirements::ExclusiveAccess => self.low_pass_filter.capacity(),
            HostSharingRequirements::OnePerToken(.., WeightClass::Permits(permits)) => *permits,
            HostSharingRequirements::Shared(WeightClass::Permits(permits)) => *permits,
        };

        let is_retryable_status = move |r: &CommandExecutionResult| {
            match &r.report.status {
                // This does need be retried since if we get a cancelled claim that would typically
                // mean the other result asked for cancellation and we're about to receive the
                // result here.
                CommandExecutionStatus::ClaimCancelled => true,
                // If the execution is successful, use the result.
                CommandExecutionStatus::Success { .. } => false,
                // Retry commands that failed (i.e. exit 1) only if we're instructed to do so.
                CommandExecutionStatus::Failure { .. } => fallback_on_failure,
                // Errors are infra errors and are always retried because that is the point of
                // falling back.
                CommandExecutionStatus::Error { .. } | CommandExecutionStatus::TimedOut { .. } => {
                    true
                }
            }
        };

        let fallback_only = fallback_only && !command.request.force_full_hybrid_if_capable();

        let ((mut first_res, first_priority), second) = if fallback_only {
            // In the fallback-only case, the primary always "wins" the race, since we don't start
            // the secondary.
            jobs.execute_sequential().await
        } else {
            // In the full-hybrid case, we do race both executors. If the low-pass filter is in
            // use, then we wrap the local execution with that.

            let jobs = jobs.map_local(move |local| {
                if low_pass_filter {
                    // If the command prefers local execution, then it'll count in the low-pass
                    // filter but it will not wait for access. This ensures that commands that are
                    // flagged as prefer_local for performance reasons get to run locally even when
                    // full hybrid is enabled.
                    //
                    // NOTE (@torozco): that we only use the command's executor preference here.
                    // Ideally we'd probably just use `executor_preference` (which folds in this
                    // executor's preference), but anecdotally I've seen a bunch of users pass
                    // `--prefer-local` without necessarily knowing why so I'd like them to not get
                    // this behavor by default.
                    let bypass_low_pass_filter =
                        if command.request.executor_preference().prefers_local() {
                            futures::future::ready(()).left_future()
                        } else {
                            futures::future::pending().right_future()
                        };

                    async move {
                        // Block local until either conditon is met:
                        // - we only have a few actions (that's low_pass_filter)
                        // - the remote executor aborts (that's remote_execution_liveliness_guard)
                        let access = self.low_pass_filter.access(weight);
                        let alive = remote_execution_liveliness_manager.while_alive();
                        futures::pin_mut!(access);
                        futures::pin_mut!(alive);
                        futures::pin_mut!(bypass_low_pass_filter);
                        let _guard = futures::future::select(
                            access,
                            futures::future::select(alive, bypass_low_pass_filter),
                        )
                        .await;
                        local.await
                    }
                    .left_future()
                } else {
                    local.right_future()
                }
            });

            jobs.execute_concurrent().await
        };

        let mut res = if is_retryable_status(&first_res) {
            // If the first result had made a claim, then cancel it now to let the other result
            // proceed.
            if let Some(claim) = first_res.report.claim.take() {
                if let Err(e) = claim.release() {
                    return manager.error(
                        "hybrid",
                        e.context("Local execution started executing without a Claim"),
                    );
                }
            }

            let (second_res, second_priority) = second.await;

            // For the purposes of giving users a good UX, if both things failed, give them the
            // local executor's error, which is likely to not have failed because of e.g.
            // sandboxing.
            let (mut primary_res, secondary_res) = if is_retryable_status(&second_res) {
                if first_priority > second_priority {
                    (first_res, second_res)
                } else {
                    (second_res, first_res)
                }
            } else {
                (second_res, first_res)
            };

            primary_res.rejected_execution = Some(secondary_res.report);
            primary_res
        } else {
            // Everyone is happy, we got our result.
            first_res
        };

        res.eligible_for_full_hybrid = !fallback_only;
        res
    }

    fn re_platform(&self) -> Option<&RE::Platform> {
        self.remote.re_platform()
    }

    fn re_use_case(&self) -> RemoteExecutorUseCase {
        self.remote.re_use_case()
    }
}

struct ReClaimManager {
    inner: Option<ReClaimManagerInner>,
}

impl ReClaimManager {
    fn new(
        local_execution_liveliness_guard: LivelinessGuard,
        claim_manager: Box<dyn ClaimManager>,
        remote_execution_liveliness_guard: LivelinessGuard,
    ) -> Self {
        Self {
            inner: Some(ReClaimManagerInner {
                local_execution_liveliness_guard,
                claim_manager,
                remote_execution_liveliness_guard,
            }),
        }
    }
}

impl Drop for ReClaimManager {
    /// If RE decides to not execute, then allow local execution to proceed.
    fn drop(&mut self) {
        if let Some(inner) = self.inner.take() {
            inner.local_execution_liveliness_guard.forget();
        }
    }
}

struct ReClaimManagerInner {
    /// Used to control and potentially cancel the execution of local comands.
    local_execution_liveliness_guard: LivelinessGuard,
    claim_manager: Box<dyn ClaimManager>,

    /// Only kept aive while the ReClaimManager (or its Claim) is alive.
    remote_execution_liveliness_guard: LivelinessGuard,
}

#[async_trait::async_trait]
impl ClaimManager for ReClaimManager {
    async fn claim(mut self: Box<Self>) -> Box<dyn Claim> {
        let inner = self.inner.take().expect("This is only taken once");

        // Kill in-flight local commands.
        let released_liveliness_guard = inner.local_execution_liveliness_guard.cancel();

        // Ask for the lock. If we never get it then we just exit as that would mean local
        // execution finished.
        let claim = inner.claim_manager.claim().await;

        box ReClaim {
            released_liveliness_guard,
            claim,
            _remote_execution_liveliness_guard: inner.remote_execution_liveliness_guard,
        }
    }
}

#[derive(Derivative)]
#[derivative(Debug)] // None of the fields have meaningful debug here.
struct ReClaim {
    #[derivative(Debug = "ignore")]
    released_liveliness_guard: CancelledLivelinessGuard,
    #[derivative(Debug = "ignore")]
    claim: Box<dyn Claim>,

    /// Only used for its lifetime.
    #[derivative(Debug = "ignore")]
    _remote_execution_liveliness_guard: LivelinessGuard,
}

impl Claim for ReClaim {
    /// Releasing the ReClaim. When we do this, we need to let local execution proceed again. This
    /// can happen if we do a fallback without full hybrid: RE goes, and fails, and then we want to
    /// proceed with local.
    ///
    /// NOTE: this does NOT work in the following scenario:
    ///
    /// - Local claims
    /// - RE has a result, asks local to cancel (successfully)
    /// - RE claims
    /// - RE fails
    ///
    /// In that case, RE will have failed. To fix this, we need local to release its claim instead
    /// of returning ClaimCancelled.
    fn release(self: Box<Self>) -> anyhow::Result<()> {
        // An error here should only occur if local execution had started without the claim.
        self.released_liveliness_guard
            .restore()
            .context("Unable to restore CancelledLivelinessGuard!")?
            .forget();

        self.claim.release()?;

        Ok(())
    }
}

/// The local and remote side of the executor, and a preference to select which future is the
/// primary and which is the secondary.
struct HybridExecutorJobs<L, R> {
    local: L,
    remote: R,
    executor_preference: ExecutorPreference,
}

impl<L, R, O> HybridExecutorJobs<L, R>
where
    L: Future<Output = O>,
    R: Future<Output = O>,
{
    /// Modify the local side future.
    fn map_local<L2>(self, f: impl FnOnce(L) -> L2) -> HybridExecutorJobs<L2, R> {
        HybridExecutorJobs {
            local: f(self.local),
            remote: self.remote,
            executor_preference: self.executor_preference,
        }
    }

    /// Return only the primary future.
    fn into_primary(self) -> Either<L, R> {
        self.into_futures().0
    }

    /// Return both futures, but box them, which makes them Upin and also erases their type.
    fn into_boxfutures<'a>(self) -> (BoxFuture<'a, O>, BoxFuture<'a, O>)
    where
        L: Send + 'a,
        R: Send + 'a,
    {
        let (primary, secondary) = self.into_futures();
        (primary.boxed(), secondary.boxed())
    }

    /// Return both futures.
    fn into_futures(self) -> (Either<L, R>, Either<L, R>) {
        let local = self.local.left_future();
        let remote = self.remote.right_future();

        if self.executor_preference.prefers_local() {
            (local, remote)
        } else {
            (remote, local)
        }
    }

    /// Race both futures, return the first result and the second future.
    async fn execute_concurrent<'a>(self) -> (O, BoxFuture<'a, O>)
    where
        L: Send + 'a,
        R: Send + 'a,
    {
        let (primary, secondary) = self.into_boxfutures();
        let (out, futs) = Either::factor_first(futures::future::select(primary, secondary).await);
        (out, futs.into_inner())
    }

    /// Run primary then return secondary.
    async fn execute_sequential<'a>(self) -> (O, BoxFuture<'a, O>)
    where
        L: Send + 'a,
        R: Send + 'a,
    {
        let (primary, secondary) = self.into_boxfutures();
        (primary.await, secondary)
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq)]
struct JobPriority(u8);
