/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Handles command concurrency.
//!
//! `buck2` supports limited concurrency for commands.
//! If there are no buckconfig changes, nor file changes, then commands can be allowed to execute
//! concurrently. Otherwise, `buck2` will block waiting for other commands to finish.

use std::collections::VecDeque;
use std::fmt::Debug;
use std::ops::Deref;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use async_condvar_fair::Condvar;
use async_trait::async_trait;
use buck2_core::soft_error;
use buck2_data::DiceBlockConcurrentCommandEnd;
use buck2_data::DiceBlockConcurrentCommandStart;
use buck2_data::DiceEqualityCheck;
use buck2_data::DiceSynchronizeSectionEnd;
use buck2_data::DiceSynchronizeSectionStart;
use buck2_data::ExclusiveCommandWaitEnd;
use buck2_data::ExclusiveCommandWaitStart;
use buck2_data::ExitWhenDifferentState;
use buck2_data::NoActiveDiceState;
use buck2_events::dispatch::EventDispatcher;
use buck2_util::truncate::truncate;
use buck2_wrapper_common::invocation_id::TraceId;
use derive_more::Display;
use dice::Dice;
use dice::DiceComputations;
use dice::DiceEquality;
use dice::DiceTransaction;
use dice::DiceTransactionUpdater;
use dice::UserComputationData;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::future::FutureExt;
use futures::future::Shared;
use itertools::Itertools;
use more_futures::cancellation::ExplicitCancellationContext;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use thiserror::Error;
use tokio::sync::Mutex;
use tokio::sync::MutexGuard;

#[derive(Error, Debug)]
enum ConcurrencyHandlerError {
    #[error(
        "Recursive invocation of Buck, which is discouraged, but will probably work (using the same state). Trace Ids: {0}. Recursive invocation command: `{1}`"
    )]
    NestedInvocationWithSameStates(String, String),
    #[error(
        "Recursive invocation of Buck, with a different state. Use `--isolation-dir` on the inner invocation to fix this. Trace Ids: {0}. Recursive invocation command: `{1}`"
    )]
    NestedInvocationWithDifferentStates(String, String),
    #[error("`--exit-when-different-state` was set")]
    ExitWhenDifferentState,
}

#[derive(Clone, Dupe, Copy, Debug)]
pub enum RunState {
    NestedSameState,
    ParallelSameState,
}

#[derive(Clone, Dupe, Copy, Debug)]
pub enum BypassSemaphore {
    Run(RunState),
    Block,
    Error,
}

/// Manages concurrent commands, blocking when appropriate.
///
/// Currently, we allow concurrency if two `DiceTransactions` are deemed equivalent, such that
/// any computation result that occurs in one is directly reusable by another.
#[derive(Clone, Dupe, Allocative)]
pub struct ConcurrencyHandler {
    data: Arc<Mutex<ConcurrencyHandlerData>>,
    // use an async condvar because the `wait` to `notify` spans across an async function (namely
    // the entire command execution).
    #[allocative(skip)]
    cond: Arc<Condvar>,
    dice: Arc<Dice>,
    /// Used to prevent commands (clean --stale) from running in parallel with dice commands
    exclusive_command_lock: Arc<ExclusiveCommandLock>,
}

#[derive(Allocative)]
struct ConcurrencyHandlerData {
    /// the currently active `Dice` being used. Commands can only run concurrently if these are
    /// "equivalent".
    dice_status: DiceStatus,
    /// A list of the currently running commands.
    active_commands: SmallMap<CommandId, CommandData>,
    /// When a command enters
    next_command_id: CommandId,
    /// The epoch of the last ActiveDice we assigned.
    cleanup_epoch: usize,
    /// Whether this has been tainted previously.
    previously_tainted: bool,
}

#[derive(Allocative, Display, Copy, Clone, Dupe, PartialEq, Eq, Hash)]
struct CommandId(usize);

impl CommandId {
    /// Increment this counter and return the next command.
    fn increment(&mut self) -> CommandId {
        let res = CommandId(self.0);
        self.0 += 1;
        res
    }
}

#[derive(Allocative)]
struct CommandData {
    trace_id: TraceId,
    argv: Vec<String>,
    dispatcher: EventDispatcher,
}

impl CommandData {
    fn format_argv(&self) -> String {
        let mut iter = self.argv.iter();
        // Skip the "/path/to/buck2" part so we can just emit "buck2" for the start of the cmd
        iter.next();

        let cmd = format!("buck2 {}", iter.join(" "));
        truncate(&cmd, 500)
    }

    fn notify_tainted(&self) {
        self.dispatcher.instant_event(buck2_data::TagEvent {
            tags: vec!["concurrency-tainted".to_owned()],
        });
    }

    fn notify_previously_tainted(&self) {
        self.dispatcher.instant_event(buck2_data::TagEvent {
            tags: vec!["concurrency-previously-tainted".to_owned()],
        });
    }
}

#[derive(Allocative)]
enum DiceStatus {
    Available {
        active: Option<ActiveDice>,
    },
    Cleanup {
        future: Shared<BoxFuture<'static, ()>>,
        epoch: usize,
    },
}

#[derive(Allocative)]
struct ActiveDice {
    version: DiceEquality,

    /// Whether this DICE version had concurrent commands that executed on it.
    tainted: bool,
}

impl DiceStatus {
    fn idle() -> Self {
        Self::Available { active: None }
    }

    fn active(version: DiceEquality) -> Self {
        Self::Available {
            active: Some(ActiveDice {
                version,
                tainted: false,
            }),
        }
    }
}

impl ConcurrencyHandlerData {
    fn has_no_active_commands(&self) -> bool {
        self.active_commands.is_empty()
    }

    /// Attempt a transition to cleanup, or straight to idle if cleanup can be skipped. Returns
    /// whether the transition was done.
    fn transition_to_cleanup(&mut self, dice: &Dice) -> bool {
        if !self.has_no_active_commands() {
            return false;
        }

        tracing::info!("Transitioning ActiveDice to cleanup");

        // When releasing the active DICE, if any work is ongoing, place it in a clean up
        // state. Callers will wait until it goes idle.
        self.cleanup_epoch += 1;
        self.dice_status = DiceStatus::Cleanup {
            future: dice.wait_for_idle().boxed().shared(),
            epoch: self.cleanup_epoch,
        };

        true
    }

    /// Attempt a transition to available assuming the cleanup future at `cleanup_epoch` has been
    /// awaited already.
    fn transition_to_idle(&mut self, cleanup_epoch: usize) {
        if !matches!(self.dice_status, DiceStatus::Cleanup { .. }) {
            // Noop: we already transitioned to available.
            return;
        }

        if self.cleanup_epoch != cleanup_epoch {
            // Noop: we already transitioned to available then back to cleanup.
            return;
        }

        self.dice_status = DiceStatus::idle();
    }

    fn notify_tainted(&self) {
        for command in self.active_commands.values() {
            command.notify_tainted()
        }
    }
}

#[async_trait]
pub trait DiceUpdater: Send + Sync {
    async fn update(
        &self,
        mut ctx: DiceTransactionUpdater,
        _user_data: &mut UserComputationData,
    ) -> anyhow::Result<DiceTransactionUpdater>;
}

#[async_trait]
pub trait DiceDataProvider: Send + Sync + 'static {
    async fn provide(&self, ctx: &DiceComputations) -> anyhow::Result<UserComputationData>;
}

#[derive(Allocative)]
struct ExclusiveCommandLock {
    lock: tokio::sync::RwLock<()>,
    owning_command: Arc<parking_lot::Mutex<VecDeque<String>>>,
}

enum ExclusiveCommandLockGuard<'a> {
    Shared(tokio::sync::RwLockReadGuard<'a, ()>),
    Exclusive(
        tokio::sync::RwLockWriteGuard<'a, ()>,
        Arc<parking_lot::Mutex<VecDeque<String>>>,
    ),
}

impl<'a> Drop for ExclusiveCommandLockGuard<'a> {
    fn drop(&mut self) {
        if let ExclusiveCommandLockGuard::Exclusive(_, owner) = self {
            let mut own = owner.lock();
            own.pop_front();
        }
    }
}

impl ExclusiveCommandLock {
    pub fn new() -> Self {
        ExclusiveCommandLock {
            lock: tokio::sync::RwLock::new(()),
            owning_command: Arc::new(parking_lot::Mutex::new(VecDeque::new())),
        }
    }

    pub async fn exclusive_lock<'a>(&'a self, cmd_name: String) -> ExclusiveCommandLockGuard<'a> {
        {
            let mut owning_command = self.owning_command.lock();
            owning_command.push_back(cmd_name);
            drop(owning_command);
        }
        ExclusiveCommandLockGuard::Exclusive(self.lock.write().await, self.owning_command.dupe())
    }

    pub async fn shared_lock<'a>(&'a self) -> ExclusiveCommandLockGuard<'a> {
        ExclusiveCommandLockGuard::Shared(self.lock.read().await)
    }

    pub fn owning_command(&self) -> Option<String> {
        // owning command is not unset when exclusive lock is dropped, just ignored
        if self.lock.try_read().is_ok() {
            None
        } else {
            self.owning_command.lock().front().cloned()
        }
    }
}

impl ConcurrencyHandler {
    pub fn new(dice: Arc<Dice>) -> Self {
        ConcurrencyHandler {
            data: Arc::new(Mutex::new(ConcurrencyHandlerData {
                dice_status: DiceStatus::idle(),
                active_commands: SmallMap::new(),
                next_command_id: CommandId(0),
                cleanup_epoch: 0,
                previously_tainted: false,
            })),
            cond: Default::default(),
            dice,
            exclusive_command_lock: Arc::new(ExclusiveCommandLock::new()),
        }
    }

    /// Enters a critical section that requires concurrent command synchronization,
    /// and runs the given `exec` function in the critical section.
    pub async fn enter<F, Fut, R>(
        &self,
        event_dispatcher: EventDispatcher,
        data: &dyn DiceDataProvider,
        updates: &dyn DiceUpdater,
        exec: F,
        is_nested_invocation: bool,
        sanitized_argv: Vec<String>,
        exclusive_cmd: Option<String>,
        exit_when_different_state: bool,
        cancellations: &ExplicitCancellationContext,
    ) -> anyhow::Result<R>
    where
        F: FnOnce(DiceTransaction) -> Fut,
        Fut: Future<Output = R> + Send,
    {
        let _exclusive_command_guard = event_dispatcher
            .span_async(
                ExclusiveCommandWaitStart {
                    command_name: self.exclusive_command_lock.owning_command(),
                },
                async move {
                    let guard = if let Some(cmd_name) = exclusive_cmd {
                        let guard = self.exclusive_command_lock.exclusive_lock(cmd_name).await;
                        self.dice.wait_for_idle().await;
                        guard
                    } else {
                        self.exclusive_command_lock.shared_lock().await
                    };
                    (guard, ExclusiveCommandWaitEnd {})
                },
            )
            .await;

        let events = event_dispatcher.dupe();
        let (_guard, transaction) = event_dispatcher
            .span_async(DiceSynchronizeSectionStart {}, async move {
                (
                    cancellations
                        .critical_section(|| {
                            self.wait_for_others(
                                data,
                                updates,
                                events,
                                is_nested_invocation,
                                sanitized_argv,
                                exit_when_different_state,
                            )
                        })
                        .await,
                    DiceSynchronizeSectionEnd {},
                )
            })
            .await?;

        Ok(exec(transaction).await)
    }

    // this is normally super unsafe, but because we are using an async condvar that takes care
    // of unlocking this mutex, this mutex is actually essentially never held across awaits.
    // The async condvar will handle properly allowing under threads to proceed, avoiding
    // starvation.
    async fn wait_for_others(
        &self,
        user_data: &dyn DiceDataProvider,
        updates: &dyn DiceUpdater,
        event_dispatcher: EventDispatcher,
        is_nested_invocation: bool,
        sanitized_argv: Vec<String>,
        exit_when_different_state: bool,
    ) -> anyhow::Result<(OnExecExit, DiceTransaction)> {
        let trace = event_dispatcher.trace_id().dupe();

        let span = tracing::span!(tracing::Level::DEBUG, "wait_for_others", trace = %trace);
        let _enter = span.enter();

        let mut data = self.data.lock().await;

        let command_id = data.next_command_id.increment();

        let command_data = CommandData {
            trace_id: trace.dupe(),
            argv: sanitized_argv,
            dispatcher: event_dispatcher.dupe(),
        };

        let (transaction, tainted) = loop {
            match &data.dice_status {
                DiceStatus::Cleanup { future, epoch } => {
                    tracing::debug!("ActiveDice is in cleanup");
                    let future = future.clone();
                    let epoch = *epoch;

                    // block while dice cleans up
                    drop(data);
                    event_dispatcher
                        .span_async(
                            buck2_data::DiceCleanupStart { epoch: epoch as _ },
                            async move { (future.await, buck2_data::DiceCleanupEnd {}) },
                        )
                        .await;
                    data = self.data.lock().await;

                    data.transition_to_idle(epoch);
                }
                DiceStatus::Available { active } => {
                    tracing::debug!("ActiveDice is available");

                    let dice_was_idle = self.dice.is_idle().await;

                    // we rerun the updates in case that files on disk have changed between commands.
                    // this might cause some churn, but concurrent commands don't happen much and
                    // isn't a big perf bottleneck. Dice should be able to resurrect nodes properly.

                    let transaction = async {
                        let updater = self.dice.updater();
                        let mut user_data = user_data
                            .provide(updater.existing_state().await.deref())
                            .await?;

                        let transaction = updates.update(updater, &mut user_data).await?;

                        event_dispatcher
                            .span_async(buck2_data::DiceStateUpdateStart {}, async {
                                (
                                    async {
                                        let transaction =
                                            transaction.commit_with_data(user_data).await;
                                        anyhow::Ok(transaction)
                                    }
                                    .await,
                                    buck2_data::DiceStateUpdateEnd {},
                                )
                            })
                            .await
                    }
                    .await?;

                    if let Some(active) = active {
                        let is_same_state = transaction.equivalent(&active.version);

                        // If we have a different state, attempt to transition to cleanup. This will
                        // succeed only if the current state is not in use.
                        if !is_same_state {
                            if data.transition_to_cleanup(&self.dice) {
                                continue;
                            }
                        }

                        tracing::debug!("ActiveDice has an active_transaction");

                        event_dispatcher.instant_event(DiceEqualityCheck {
                            is_equal: is_same_state,
                        });

                        let bypass_semaphore =
                            self.determine_bypass_semaphore(is_same_state, is_nested_invocation);

                        match bypass_semaphore {
                            BypassSemaphore::Error => {
                                return Err(anyhow::Error::new(
                                    ConcurrencyHandlerError::NestedInvocationWithDifferentStates(
                                        format_traces(&data.active_commands, &command_data),
                                        command_data.format_argv(),
                                    ),
                                ));
                            }
                            BypassSemaphore::Run(state) => {
                                self.emit_logs(state, &data.active_commands, &command_data)?;
                                break (transaction, false);
                            }
                            BypassSemaphore::Block => {
                                if exit_when_different_state {
                                    event_dispatcher.instant_event(ExitWhenDifferentState {});

                                    return Err(anyhow::Error::new(
                                        ConcurrencyHandlerError::ExitWhenDifferentState,
                                    ))
                                    .context(buck2_data::ErrorCause::DaemonIsBusy);
                                }
                                // We should probably show more than the first here, but for now
                                // this is what we have.
                                //
                                // Note: unwrap here relies on the fact that transition_to_cleanup
                                // would have transitioned if we had no active commands.

                                let active_command = data.active_commands.first().unwrap().1;
                                let trace_id = active_command.trace_id.dupe();
                                let argv = active_command.format_argv();

                                data = event_dispatcher
                                    .span_async(
                                        DiceBlockConcurrentCommandStart {
                                            current_active_trace_id: trace_id.to_string(),
                                            cmd_args: argv,
                                        },
                                        async {
                                            (
                                                self.cond.wait((data, &*self.data)).await,
                                                DiceBlockConcurrentCommandEnd {
                                                    ending_active_trace_id: trace_id.to_string(),
                                                },
                                            )
                                        },
                                    )
                                    .await;
                            }
                        }
                    } else {
                        tracing::debug!("ActiveDice has no active_transaction");
                        event_dispatcher.instant_event(NoActiveDiceState {});
                        data.dice_status = DiceStatus::active(transaction.equality_token());
                        break (transaction, !dice_was_idle);
                    }
                }
            }
        };

        tracing::info!("Acquired access to DICE");

        if data.previously_tainted {
            command_data.notify_previously_tainted();
        }

        if tainted {
            command_data.notify_tainted();
            data.notify_tainted();
            data.previously_tainted = true;
        }

        // create the on exit drop handler, which will take care of notifying tasks.
        let drop_guard = OnExecExit::new(self.dupe(), command_id, command_data, data);

        Ok((drop_guard, transaction))
    }

    /// Access dice without locking for dumps.
    pub fn unsafe_dice(&self) -> &Arc<Dice> {
        &self.dice
    }

    fn determine_bypass_semaphore(
        &self,
        is_same_state: bool,
        is_nested_invocation: bool,
    ) -> BypassSemaphore {
        if is_same_state {
            if is_nested_invocation {
                BypassSemaphore::Run(RunState::NestedSameState)
            } else {
                BypassSemaphore::Run(RunState::ParallelSameState)
            }
        } else if is_nested_invocation {
            BypassSemaphore::Error
        } else {
            BypassSemaphore::Block
        }
    }

    fn emit_logs(
        &self,
        state: RunState,
        active_commands: &SmallMap<CommandId, CommandData>,
        current_command: &CommandData,
    ) -> anyhow::Result<()> {
        let active_commands = format_traces(active_commands, current_command);

        match state {
            RunState::NestedSameState => {
                soft_error!(
                    "nested_invocation_same_dice_state",
                    anyhow::anyhow!(ConcurrencyHandlerError::NestedInvocationWithSameStates(
                        active_commands,
                        current_command.format_argv(),
                    ))
                )?;
            }
            _ => {}
        }

        Ok(())
    }
}

fn format_traces(
    active_commands: &SmallMap<CommandId, CommandData>,
    current: &CommandData,
) -> String {
    let trace_ids = active_commands
        .values()
        .chain(std::iter::once(current))
        .map(|cmd| &cmd.trace_id)
        .collect::<SmallSet<_>>();

    trace_ids.iter().join(", ")
}

/// Held to execute a command so that when the command is canceled, we properly remove its state
/// from the handler so that it's no longer registered as a ongoing command.
struct OnExecExit(Option<(ConcurrencyHandler, CommandId)>);

impl OnExecExit {
    pub fn new(
        handler: ConcurrencyHandler,
        command: CommandId,
        data: CommandData,
        mut guard: MutexGuard<'_, ConcurrencyHandlerData>,
    ) -> Self {
        guard.active_commands.insert(command, data);
        Self(Some((handler, command)))
    }
}

impl Drop for OnExecExit {
    fn drop(&mut self) {
        let this = self.0.take().expect("dropped twice");
        tracing::info!("Command has exited: {}", this.1);

        tokio::task::spawn(async move {
            let mut data = this.0.data.lock().await;
            data.active_commands
                .remove(&this.1)
                .expect("command was active but not in active_commands");
            tracing::info!("Active command was removed: {}", this.1);

            if data.has_no_active_commands() {
                // we notify all commands since we don't know how many can actually wake up and run
                // concurrently as several of the currently waiting commands could be "equivalent".
                // This could cause commands to wake up out of order and race, such that the longest
                // waiting command might not still be forced to wait. In reality, it is probably not
                // a terrible issue, as we are unlikely to have many concurrent commands, and people
                // are unlikely to usually care about the precise order they get to run.
                this.0.cond.notify_all()
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::task::Poll;
    use std::time::Duration;

    use allocative::Allocative;
    use anyhow::Context;
    use assert_matches::assert_matches;
    use async_trait::async_trait;
    use buck2_core::is_open_source;
    use buck2_events::create_source_sink_pair;
    use buck2_events::dispatch::EventDispatcher;
    use buck2_events::source::ChannelEventSource;
    use buck2_events::span::SpanId;
    use buck2_events::BuckEvent;
    use buck2_wrapper_common::invocation_id::TraceId;
    use derivative::Derivative;
    use dice::DetectCycles;
    use dice::Dice;
    use dice::DiceComputations;
    use dice::DiceTransactionUpdater;
    use dice::InjectedKey;
    use dice::Key;
    use dice::UserComputationData;
    use dupe::Dupe;
    use futures::pin_mut;
    use futures::poll;
    use more_futures::cancellation::CancellationContext;
    use parking_lot::Mutex;
    use tokio::sync::Barrier;
    use tokio::sync::RwLock;
    use tokio::sync::Semaphore;

    use super::*;

    struct NoChanges;

    #[async_trait]
    impl DiceUpdater for NoChanges {
        async fn update(
            &self,
            ctx: DiceTransactionUpdater,
            _user_data: &mut UserComputationData,
        ) -> anyhow::Result<DiceTransactionUpdater> {
            Ok(ctx)
        }
    }

    struct CtxDifferent;

    #[async_trait]
    impl DiceUpdater for CtxDifferent {
        async fn update(
            &self,
            mut ctx: DiceTransactionUpdater,
            _user_data: &mut UserComputationData,
        ) -> anyhow::Result<DiceTransactionUpdater> {
            ctx.changed_to(vec![(K, ())])?;
            Ok(ctx)
        }
    }

    #[derive(Clone, Dupe, Display, Debug, Hash, Eq, PartialEq, Allocative)]
    struct K;

    #[async_trait]
    impl InjectedKey for K {
        type Value = ();

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            false
        }
    }

    struct TestDiceDataProvider;

    #[async_trait]
    impl DiceDataProvider for TestDiceDataProvider {
        async fn provide(&self, _ctx: &DiceComputations) -> anyhow::Result<UserComputationData> {
            Ok(Default::default())
        }
    }

    #[tokio::test]
    async fn nested_invocation_same_transaction() {
        // FIXME: This times out on open source, and we don't know why
        if is_open_source() {
            return;
        }

        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(dice);

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();
        let traces3 = TraceId::new();

        let barrier = Arc::new(Barrier::new(3));

        let fut1 = concurrency.enter(
            EventDispatcher::null_sink_with_trace(traces1),
            &TestDiceDataProvider,
            &NoChanges,
            |_| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            true,
            Vec::new(),
            None,
            false,
            ExplicitCancellationContext::testing(),
        );
        let fut2 = concurrency.enter(
            EventDispatcher::null_sink_with_trace(traces2),
            &TestDiceDataProvider,
            &NoChanges,
            |_| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            true,
            Vec::new(),
            None,
            false,
            ExplicitCancellationContext::testing(),
        );
        let fut3 = concurrency.enter(
            EventDispatcher::null_sink_with_trace(traces3),
            &TestDiceDataProvider,
            &NoChanges,
            |_| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            true,
            Vec::new(),
            None,
            false,
            ExplicitCancellationContext::testing(),
        );

        let (r1, r2, r3) = futures::future::join3(fut1, fut2, fut3).await;
        r1.unwrap();
        r2.unwrap();
        r3.unwrap();
    }

    #[tokio::test]
    async fn nested_invocation_should_error() {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(dice);

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        let barrier = Arc::new(Barrier::new(2));

        let fut1 = concurrency.enter(
            EventDispatcher::null_sink_with_trace(traces1),
            &TestDiceDataProvider,
            &NoChanges,
            |_| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            true,
            Vec::new(),
            None,
            false,
            ExplicitCancellationContext::testing(),
        );

        let fut2 = concurrency.enter(
            EventDispatcher::null_sink_with_trace(traces2),
            &TestDiceDataProvider,
            &CtxDifferent,
            |_| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            true,
            Vec::new(),
            None,
            false,
            ExplicitCancellationContext::testing(),
        );

        match futures::future::try_join(fut1, fut2).await {
            Err(e) => assert!(e.to_string().contains("Recursive invocation")),
            Ok(_) => {
                panic!("Futures should not have completed successfully")
            }
        }
    }

    #[tokio::test]
    async fn parallel_invocation_same_transaction() {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(dice);

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();
        let traces3 = TraceId::new();

        let barrier = Arc::new(Barrier::new(3));

        let fut1 = concurrency.enter(
            EventDispatcher::null_sink_with_trace(traces1),
            &TestDiceDataProvider,
            &NoChanges,
            |_| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            false,
            Vec::new(),
            None,
            false,
            ExplicitCancellationContext::testing(),
        );
        let fut2 = concurrency.enter(
            EventDispatcher::null_sink_with_trace(traces2),
            &TestDiceDataProvider,
            &NoChanges,
            |_| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            false,
            Vec::new(),
            None,
            false,
            ExplicitCancellationContext::testing(),
        );
        let fut3 = concurrency.enter(
            EventDispatcher::null_sink_with_trace(traces3),
            &TestDiceDataProvider,
            &NoChanges,
            |_| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            false,
            Vec::new(),
            None,
            false,
            ExplicitCancellationContext::testing(),
        );

        let (r1, r2, r3) = futures::future::join3(fut1, fut2, fut3).await;
        r1.unwrap();
        r2.unwrap();
        r3.unwrap();
    }

    #[tokio::test]
    async fn parallel_invocation_different_traceid_blocks() -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = traces1.dupe();
        let traces_different = TraceId::new();

        let block1 = Arc::new(RwLock::new(()));
        let blocked1 = block1.write().await;

        let block2 = Arc::new(RwLock::new(()));
        let blocked2 = block2.write().await;

        let barrier1 = Arc::new(Barrier::new(3));
        let barrier2 = Arc::new(Barrier::new(2));

        let arrived = Arc::new(AtomicBool::new(false));

        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier1.dupe();
            let b = block1.dupe();

            async move {
                concurrency
                    .enter(
                        EventDispatcher::null_sink_with_trace(traces1),
                        &TestDiceDataProvider,
                        &NoChanges,
                        |_| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        false,
                        ExplicitCancellationContext::testing(),
                    )
                    .await
            }
        });

        let fut2 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier1.dupe();
            let b = block2.dupe();

            async move {
                concurrency
                    .enter(
                        EventDispatcher::null_sink_with_trace(traces2),
                        &TestDiceDataProvider,
                        &NoChanges,
                        |_| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        false,
                        ExplicitCancellationContext::testing(),
                    )
                    .await
            }
        });

        barrier1.wait().await;

        let fut3 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier2.dupe();
            let arrived = arrived.dupe();

            async move {
                barrier.wait().await;
                concurrency
                    .enter(
                        EventDispatcher::null_sink_with_trace(traces_different),
                        &TestDiceDataProvider,
                        &CtxDifferent,
                        |_| async move {
                            arrived.store(true, Ordering::Relaxed);
                        },
                        false,
                        Vec::new(),
                        None,
                        false,
                        ExplicitCancellationContext::testing(),
                    )
                    .await
            }
        });

        barrier2.wait().await;

        assert!(!arrived.load(Ordering::Relaxed));

        drop(blocked1);
        fut1.await??;

        assert!(!arrived.load(Ordering::Relaxed));

        drop(blocked2);
        fut2.await??;

        fut3.await??;

        assert!(arrived.load(Ordering::Relaxed));

        Ok(())
    }

    #[tokio::test]
    async fn parallel_invocation_exit_when_different_state() -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = traces1.dupe();
        let traces_different = TraceId::new();

        let block1 = Arc::new(RwLock::new(()));
        let blocked1 = block1.write().await;

        let block2 = Arc::new(RwLock::new(()));
        let blocked2 = block2.write().await;

        let barrier1 = Arc::new(Barrier::new(3));
        let barrier2 = Arc::new(Barrier::new(2));

        let arrived = Arc::new(AtomicBool::new(false));

        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier1.dupe();
            let b = block1.dupe();

            async move {
                concurrency
                    .enter(
                        EventDispatcher::null_sink_with_trace(traces1),
                        &TestDiceDataProvider,
                        &NoChanges,
                        |_| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        true,
                        ExplicitCancellationContext::testing(),
                    )
                    .await
            }
        });

        let fut2 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier1.dupe();
            let b = block2.dupe();

            async move {
                concurrency
                    .enter(
                        EventDispatcher::null_sink_with_trace(traces2),
                        &TestDiceDataProvider,
                        &NoChanges,
                        |_| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        true,
                        ExplicitCancellationContext::testing(),
                    )
                    .await
            }
        });

        barrier1.wait().await;

        let fut3 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier2.dupe();
            let arrived = arrived.dupe();

            async move {
                barrier.wait().await;
                concurrency
                    .enter(
                        EventDispatcher::null_sink_with_trace(traces_different),
                        &TestDiceDataProvider,
                        &CtxDifferent,
                        |_| async move {
                            arrived.store(true, Ordering::Relaxed);
                        },
                        false,
                        Vec::new(),
                        None,
                        true,
                        ExplicitCancellationContext::testing(),
                    )
                    .await
            }
        });

        barrier2.wait().await;

        assert!(!arrived.load(Ordering::Relaxed));

        drop(blocked1);
        fut1.await??;

        assert!(!arrived.load(Ordering::Relaxed));

        drop(blocked2);
        fut2.await??;

        let fut3_result = fut3.await?;

        assert!(fut3_result.is_err());
        assert!(
            fut3_result
                .err()
                .unwrap()
                .to_string()
                .contains("daemon is busy")
        );

        Ok(())
    }

    #[derive(Clone, Dupe, Derivative, Allocative, Display)]
    #[derivative(Hash, Eq, PartialEq, Debug)]
    #[display(fmt = "CleanupTestKey")]
    struct CleanupTestKey {
        #[derivative(Debug = "ignore", Hash = "ignore", PartialEq = "ignore")]
        is_executing: Arc<Mutex<()>>,
    }

    #[async_trait::async_trait]
    impl Key for CleanupTestKey {
        type Value = ();

        #[allow(clippy::await_holding_lock)]
        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            cancellation: &CancellationContext,
        ) -> Self::Value {
            let _guard = self.is_executing.lock();

            // TODO: use critical_section as it's simpler, but this stack doesn't have it and
            // this works equally well here :)
            cancellation
                .with_structured_cancellation(|_obs| tokio::time::sleep(Duration::from_secs(1)))
                .await;
        }

        fn equality(_me: &Self::Value, _other: &Self::Value) -> bool {
            true
        }
    }

    #[tokio::test]
    async fn test_cleanup_stage() -> anyhow::Result<()> {
        let key = CleanupTestKey {
            is_executing: Arc::new(Mutex::new(())),
        };

        let key = &key;

        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(dice.dupe());

        // Kick off our computation and wait until it's running.

        concurrency
            .enter(
                EventDispatcher::null(),
                &TestDiceDataProvider,
                &NoChanges,
                |dice| async move {
                    let compute = dice.compute(key).fuse();

                    let started = async {
                        while !key.is_executing.is_locked() {
                            tokio::task::yield_now().await;
                        }
                    }
                    .fuse();

                    // NOTE: We still need to poll `compute` for it to actually spawn, hence the
                    // select below.

                    futures::pin_mut!(compute);
                    futures::pin_mut!(started);

                    futures::select! {
                        _ = compute => panic!("compute finished before started?"),
                        _ = started => {}
                    }
                },
                false,
                Vec::new(),
                None,
                false,
                ExplicitCancellationContext::testing(),
            )
            .await?;

        // Now, re-enter. We expect to reuse and therefore to not wait.

        concurrency
            .enter(
                EventDispatcher::null(),
                &TestDiceDataProvider,
                &NoChanges,
                |_dice| async move {
                    // The key should still be evaluating by now.
                    assert!(key.is_executing.is_locked());
                },
                false,
                Vec::new(),
                None,
                false,
                ExplicitCancellationContext::testing(),
            )
            .await?;

        // Now, enter with a different context. This time, we expect to not reuse.

        concurrency
            .enter(
                EventDispatcher::null(),
                &TestDiceDataProvider,
                &CtxDifferent,
                |_dice| async move {
                    assert!(!key.is_executing.is_locked());
                },
                false,
                Vec::new(),
                None,
                false,
                ExplicitCancellationContext::testing(),
            )
            .await?;

        Ok(())
    }

    async fn wait_for_event<F>(
        source: &mut ChannelEventSource,
        matcher: Box<F>,
    ) -> anyhow::Result<BuckEvent>
    where
        F: Fn(&BuckEvent) -> bool + Send,
    {
        tokio::time::timeout(Duration::from_millis(2), async {
            loop {
                if let Some(event) = source.try_receive() {
                    if let Some(event) = event.unpack_buck() {
                        if matcher(event) {
                            break event.clone();
                        }
                    }
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("Time out waiting for matching buck event")
    }

    async fn wait_for_exclusive_span_start(
        source: &mut ChannelEventSource,
        cmd: Option<&str>,
    ) -> anyhow::Result<Option<SpanId>> {
        let cmd = cmd.map(|c| c.to_owned());
        Ok(wait_for_event(
            source,
            Box::new(|e: &BuckEvent| {
                if let Some(span_start) = &e.span_start_event() {
                    if let Some(buck2_data::span_start_event::Data::ExclusiveCommandWait(data)) =
                        &span_start.data
                    {
                        let ExclusiveCommandWaitStart {
                            command_name: event_cmd,
                        } = data;
                        return event_cmd == &cmd;
                    }
                }
                false
            }),
        )
        .await?
        .span_id())
    }

    async fn wait_for_exclusive_span_end(
        source: &mut ChannelEventSource,
        span_id: Option<SpanId>,
    ) -> anyhow::Result<BuckEvent> {
        wait_for_event(
            source,
            Box::new(|e: &BuckEvent| {
                if let Some(span_end) = &e.span_end_event() {
                    if let Some(buck2_data::span_end_event::Data::ExclusiveCommandWait(_)) =
                        &span_end.data
                    {
                        return e.span_id() == span_id || span_id.is_none();
                    }
                }
                false
            }),
        )
        .await
    }

    #[tokio::test]
    async fn exclusive_command_lock() -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Enabled);
        let concurrency = ConcurrencyHandler::new(dice.dupe());
        let (mut source, sink) = create_source_sink_pair();
        let dispatcher = EventDispatcher::new(TraceId::new(), sink);

        let mutex = Arc::new(Mutex::new(()));
        let command = |exclusive_cmd: Option<&str>, barriers: Option<&Arc<(Barrier, Barrier)>>| {
            tokio::spawn({
                let concurrency = concurrency.dupe();
                let dispatcher = dispatcher.dupe();
                let barriers = barriers.map(|b| b.dupe());
                let exclusive_cmd = exclusive_cmd.map(|b| b.to_owned());
                let mutex = mutex.dupe();
                async move {
                    concurrency
                        .enter(
                            dispatcher,
                            &TestDiceDataProvider,
                            &NoChanges,
                            |_| async move {
                                let _guard = mutex.try_lock().expect("Not exclusive!");
                                if let Some(barriers) = barriers {
                                    barriers.0.wait().await;
                                    barriers.1.wait().await;
                                }
                                tokio::task::yield_now().await;
                            },
                            false,
                            Vec::new(),
                            exclusive_cmd,
                            false,
                            ExplicitCancellationContext::testing(),
                        )
                        .await
                }
            })
        };

        let non_exclusive_barriers = Arc::new((Barrier::new(2), Barrier::new(2)));
        // Start non_exclusive command and enter critical section
        let non_exclusive_fut = command(None, Some(&non_exclusive_barriers.dupe()));
        non_exclusive_barriers.0.wait().await;

        let span_id_non_exclusive = wait_for_exclusive_span_start(&mut source, None).await?;
        wait_for_exclusive_span_end(&mut source, span_id_non_exclusive).await?;

        let command_barriers = Arc::new((Barrier::new(2), Barrier::new(2)));
        // Start exclusive command, blocked by non_exclusive
        let exclusive_fut_1 = command(Some("exclusive_1"), Some(&command_barriers.dupe()));

        let span_id_exclusive_1 = wait_for_exclusive_span_start(&mut source, None).await?;

        // Finish non_exclusive, enter exclusive_1 critical section
        non_exclusive_barriers.1.wait().await;
        non_exclusive_fut.await??;
        command_barriers.0.wait().await;

        wait_for_exclusive_span_end(&mut source, span_id_exclusive_1).await?;

        // Start series of exclusive commands and another second non_exclusive
        let exclusive_fut_2 = command(Some("exclusive_2"), None);
        let span_id_exclusive_2 =
            wait_for_exclusive_span_start(&mut source, Some("exclusive_1")).await?;
        let exclusive_fut_3 = command(Some("exclusive_3"), None);
        let span_id_exclusive_3 =
            wait_for_exclusive_span_start(&mut source, Some("exclusive_1")).await?;
        let non_exclusive_fut = command(None, None);
        let span_id_non_exclusive =
            wait_for_exclusive_span_start(&mut source, Some("exclusive_1")).await?;

        // Unblock first exclusive command, remaining commands are unblocked
        command_barriers.1.wait().await;
        exclusive_fut_1.await??;
        exclusive_fut_2.await??;
        exclusive_fut_3.await??;
        non_exclusive_fut.await??;

        wait_for_exclusive_span_end(&mut source, span_id_exclusive_2).await?;
        wait_for_exclusive_span_end(&mut source, span_id_exclusive_3).await?;
        wait_for_exclusive_span_end(&mut source, span_id_non_exclusive).await?;
        Ok(())
    }

    #[tokio::test]
    async fn test_thundering_herd() -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let concurrency = &concurrency;

        let tasks = (0..3).map(|_i| async {
            concurrency
                .enter(
                    EventDispatcher::null(),
                    &TestDiceDataProvider,
                    &CtxDifferent,
                    |dice| async move {
                        // NOTE: We need to actually compute something for DICE to be not-idle.
                        dice.compute(&K).await.unwrap();
                        tokio::task::yield_now().await;
                    },
                    false,
                    Vec::new(),
                    None,
                    false,
                    ExplicitCancellationContext::testing(),
                )
                .await
        });

        futures::future::try_join_all(tasks).await?;

        assert!(!concurrency.data.lock().await.previously_tainted);

        Ok(())
    }

    #[tokio::test]
    async fn test_updates_are_synchronized() -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(dice.dupe());

        struct Updater {
            should_be_able_to_run: AtomicBool,
            arrived_update: Semaphore,
        }

        #[async_trait]
        impl DiceUpdater for Updater {
            async fn update(
                &self,
                ctx: DiceTransactionUpdater,
                _user_data: &mut UserComputationData,
            ) -> anyhow::Result<DiceTransactionUpdater> {
                self.arrived_update.add_permits(1);
                tokio::task::yield_now().await;

                if self.should_be_able_to_run.load(Ordering::SeqCst) {
                    Ok(ctx)
                } else {
                    panic!("shouldn't be running")
                }
            }
        }

        let updater1 = Updater {
            should_be_able_to_run: AtomicBool::new(false),
            arrived_update: Semaphore::new(0),
        };
        let fut1 = concurrency.enter(
            EventDispatcher::null(),
            &TestDiceDataProvider,
            &updater1,
            |_dice| async move {
                tokio::task::yield_now().await;
            },
            false,
            Vec::new(),
            None,
            false,
            ExplicitCancellationContext::testing(),
        );
        pin_mut!(fut1);

        let updater2 = Updater {
            should_be_able_to_run: AtomicBool::new(false),
            arrived_update: Semaphore::new(0),
        };
        let fut2 = concurrency.enter(
            EventDispatcher::null(),
            &TestDiceDataProvider,
            &updater2,
            |_dice| async move {
                tokio::task::yield_now().await;
            },
            false,
            Vec::new(),
            None,
            false,
            ExplicitCancellationContext::testing(),
        );
        pin_mut!(fut2);

        // poll once will arrive at the `yield` in updater
        assert_matches!(poll!(&mut fut1), Poll::Pending);
        let _g = updater1.arrived_update.acquire().await?;

        // polling multiple times on the second command will all be pending and not complete
        assert_matches!(poll!(&mut fut2), Poll::Pending);
        assert_matches!(poll!(&mut fut2), Poll::Pending);
        assert_matches!(poll!(&mut fut2), Poll::Pending);
        assert_matches!(poll!(&mut fut2), Poll::Pending);

        // now make the first command runnable
        updater1.should_be_able_to_run.store(true, Ordering::SeqCst);
        fut1.await?;

        // 1 is done and dropped, so `2` can now finish
        updater2.should_be_able_to_run.store(true, Ordering::SeqCst);
        fut2.await?;

        Ok(())
    }
}
