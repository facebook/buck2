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
use buck2_common::liveliness_observer::CancelledLivelinessGuard;
use buck2_common::liveliness_observer::LivelinessGuard;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_common::liveliness_observer::LivelinessObserverExt;
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
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::Either;
use futures::future::Future;
use futures::FutureExt;
use host_sharing::HostSharingRequirements;

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
        liveliness_observer: Arc<dyn LivelinessObserver>,
    ) -> CommandExecutionResult {
        let local_manager =
            CommandExecutionManager::new(claim_manager, events, liveliness_observer);
        self.local.exec_cmd(command, local_manager).await
    }

    async fn remote_exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        claim_manager: Box<dyn ClaimManager>,
        events: EventDispatcher,
        liveliness_observer: Arc<dyn LivelinessObserver>,
    ) -> CommandExecutionResult {
        let remote_manager =
            CommandExecutionManager::new(claim_manager, events, liveliness_observer);
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
            .and(command.request.executor_preference());

        let executor_preference = match executor_preference {
            Ok(executor_preference) => executor_preference,
            Err(e) => return manager.error("prepare_hybrid", e),
        };

        // inspect that and construct our own result. Especially in the case of secondary fallback,
        // the current approach effectively loses the data from the primary result (for example, we
        // should have stage events the reflect the full duration not just the fallback part).

        // Construct our claim manager and a liveliness guard for local commands. The way this
        // works is as follows: when RE takes a claim, we cancel local commands. This means that we
        // can truly race RE and local: if RE finishes even after local started, it'll cancel the
        // local execution, we'll get back here with a ClaimCancelled from local execution, cancel
        // local's claim, and then resume RE.
        let (local_execution_liveliness_observer, local_execution_liveliness_guard) =
            LivelinessGuard::create();

        // Used to monitor that the RE execution is alive.
        let (remote_execution_liveliness_observer, remote_execution_liveliness_guard) =
            LivelinessGuard::create();

        let claim_manager = MutexClaimManager::new();

        // Note that this only sets up these futures, nothing will happen until they are awaited
        // (this is important in the case where we shouldn't be sending one of them).
        let local_result = self.local_exec_cmd(
            command,
            Box::new(claim_manager.dupe()),
            manager.events.dupe(),
            Arc::new(
                manager
                    .liveliness_observer
                    .dupe()
                    .and(local_execution_liveliness_observer.dupe()),
            ),
        );

        let remote_result = self.remote_exec_cmd(
            command,
            Box::new(ReClaimManager::new(
                local_execution_liveliness_guard,
                Box::new(claim_manager),
                remote_execution_liveliness_guard,
            )),
            manager.events.dupe(),
            manager.liveliness_observer.dupe(),
        );

        if executor_preference.requires_local()
            || self.remote.is_action_too_large(&command.request.paths())
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
            HostSharingRequirements::OnePerToken(.., class) => {
                self.local.host_sharing_broker.requested_permits(class)
            }
            HostSharingRequirements::Shared(class) => {
                self.local.host_sharing_broker.requested_permits(class)
            }
        };

        let is_retryable_status = move |r: &CommandExecutionResult| {
            match &r.report.status {
                // This does need be retried since if we get a cancelled result that would
                // typically mean the other result asked for cancellation and we're about to
                // receive the result here, or it could mean we're being asked to cancel by our
                // caller.
                CommandExecutionStatus::Cancelled => true,
                // If the execution is successful, use the result.
                CommandExecutionStatus::Success { .. } => false,
                // Retry commands that failed (i.e. exit 1) only if we're instructed to do so.
                CommandExecutionStatus::Failure { .. } => fallback_on_failure,
                // Don't retry timeouts. They are used for tests and falling back on a timeout is
                // sort of the opposite of what's been requested.
                CommandExecutionStatus::TimedOut { .. } => false,
                // Errors are infra errors and are always retried because that is the point of
                // falling back.
                CommandExecutionStatus::Error { .. } => true,
            }
        };

        let fallback_only = fallback_only && !command.request.force_full_hybrid_if_capable();

        let ((mut first_res, first_priority), second) =
            if executor_preference.prefers_local() || executor_preference.prefers_remote() {
                // Don't race in this scenario, since this is typically used for
                // actions that are too expensive to run on RE.
                jobs.execute_sequential().await
            } else {
                // In the full-hybrid case, we do race both executors. If the low-pass filter is in
                // use, then we wrap the local execution with that.
                let jobs = if fallback_only {
                    jobs.map_local(move |local| {
                        async move {
                            // Block local until the remote executor aborts (that's remote_execution_liveliness_guard)
                            // The claim actually comes back to us via the execution report so there's no race condition
                            // where local unblocks just when RE finishes
                            remote_execution_liveliness_observer.while_alive().await;
                            local.await
                        }
                        .boxed()
                    })
                } else if low_pass_filter {
                    jobs.map_local(move |local| {
                        async move {
                            // Block local until either condition is met:
                            // - we only have a few actions (that's low_pass_filter)
                            // - the remote executor aborts (that's remote_execution_liveliness_guard)
                            let access = self.low_pass_filter.access(weight);
                            let alive = remote_execution_liveliness_observer.while_alive();
                            futures::pin_mut!(access);
                            futures::pin_mut!(alive);
                            let _guard = futures::future::select(access, alive).await;
                            local.await
                        }
                        .boxed()
                    })
                } else {
                    jobs.map_local(|local| local.boxed())
                };
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
                remote_execution_liveliness_guard: Some(remote_execution_liveliness_guard),
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
    /// Used to control and potentially cancel the execution of local commands.
    local_execution_liveliness_guard: LivelinessGuard,
    claim_manager: Box<dyn ClaimManager>,

    /// Only kept alive while the ReClaimManager (or its Claim) is alive.
    remote_execution_liveliness_guard: Option<LivelinessGuard>,
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

        Box::new(ReClaim {
            released_liveliness_guard,
            claim,
            _remote_execution_liveliness_guard: inner.remote_execution_liveliness_guard,
        })
    }

    fn on_result_delayed(&mut self) {
        if let Some(inner) = &mut self.inner {
            inner.remote_execution_liveliness_guard.take();
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
    _remote_execution_liveliness_guard: Option<LivelinessGuard>,
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
