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
use std::str::FromStr;
use std::sync::Arc;

use allocative::Allocative;
use async_condvar_fair::Condvar;
use async_trait::async_trait;
use buck2_core::soft_error;
use buck2_core::truncate::truncate;
use buck2_data::DiceBlockConcurrentCommandEnd;
use buck2_data::DiceBlockConcurrentCommandStart;
use buck2_data::DiceEqualityCheck;
use buck2_data::DiceSynchronizeSectionEnd;
use buck2_data::DiceSynchronizeSectionStart;
use buck2_data::ExclusiveCommandWaitEnd;
use buck2_data::ExclusiveCommandWaitStart;
use buck2_data::NoActiveDiceState;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::trace::TraceId;
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
use more_futures::cancellable_future::critical_section;
use parking_lot::lock_api::MutexGuard;
use parking_lot::FairMutex;
use parking_lot::RawFairMutex;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use thiserror::Error;

#[derive(Error, Debug)]
enum ConcurrencyHandlerError {
    #[error(
        "Recursive invocation of Buck, which is discouraged, but will probably work (using the same state). Trace Ids: {0}. Recursive invocation command: `{1}`"
    )]
    NestedInvocationWithSameStates(String, String),
    #[error(
        "Recursive invocation of Buck, with a different state - computation will continue but may produce incorrect results. Trace Ids: {0}. Recursive invocation command: `{1}`"
    )]
    NestedInvocationWithDifferentStates(String, String),
    #[error(
        "Parallel invocation of Buck, with a different state - computation will continue but may produce incorrect results. Trace Ids: {0}"
    )]
    ParallelInvocationWithDifferentStates(String),
}

#[derive(Clone, Dupe, Copy, Debug, Allocative)]
pub enum ParallelInvocation {
    Block,
    Run,
}

#[derive(Clone, Dupe, Copy, Debug, Allocative)]
pub enum NestedInvocation {
    Error,
    Run,
}

#[derive(Clone, Dupe, Copy, Debug, Allocative)]
pub enum DiceCleanup {
    Block,
    Run,
}

#[derive(Error, Debug)]
#[error("Invalid type of `{0}`: `{1}`")]
pub struct InvalidType(String, String);

impl FromStr for ParallelInvocation {
    type Err = InvalidType;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "BLOCK" => Ok(ParallelInvocation::Block),
            "RUN" => Ok(ParallelInvocation::Run),
            _ => Err(InvalidType("ParallelInvocation".to_owned(), s.to_owned())),
        }
    }
}

impl FromStr for NestedInvocation {
    type Err = InvalidType;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "ERROR" => Ok(NestedInvocation::Error),
            "RUN" => Ok(NestedInvocation::Run),
            _ => Err(InvalidType("NestedInvocation".to_owned(), s.to_owned())),
        }
    }
}

impl FromStr for DiceCleanup {
    type Err = InvalidType;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_uppercase().as_str() {
            "BLOCK" => Ok(DiceCleanup::Block),
            "RUN" => Ok(DiceCleanup::Run),
            _ => Err(InvalidType("DiceCleanup".to_owned(), s.to_owned())),
        }
    }
}

#[derive(Clone, Dupe, Copy, Debug)]
pub enum RunState {
    NestedSameState,
    NestedDifferentState,
    ParallelSameState,
    ParallelDifferentState,
}

impl RunState {
    fn will_taint(self) -> bool {
        match self {
            Self::NestedSameState => false,
            Self::NestedDifferentState => true,
            Self::ParallelSameState => false,
            Self::ParallelDifferentState => true,
        }
    }
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
    data: Arc<FairMutex<ConcurrencyHandlerData>>,
    // use an async condvar because the `wait` to `notify` spans across an async function (namely
    // the entire command execution).
    #[allocative(skip)]
    cond: Arc<Condvar>,
    dice: Arc<Dice>,
    // configuration on how to handle nested invocations with different states
    nested_invocation_config: NestedInvocation,
    // configuration on how to handle parallel invocations with different states
    parallel_invocation_config: ParallelInvocation,
    /// Whether to wait for idle DICE.
    dice_cleanup_config: DiceCleanup,
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
    pub fn new(
        dice: Arc<Dice>,
        nested_invocation_config: NestedInvocation,
        parallel_invocation_config: ParallelInvocation,
        dice_cleanup_config: DiceCleanup,
    ) -> Self {
        ConcurrencyHandler {
            data: Arc::new(FairMutex::new(ConcurrencyHandlerData {
                dice_status: DiceStatus::idle(),
                active_commands: SmallMap::new(),
                next_command_id: CommandId(0),
                cleanup_epoch: 0,
                previously_tainted: false,
            })),
            cond: Default::default(),
            dice,
            nested_invocation_config,
            parallel_invocation_config,
            dice_cleanup_config,
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
                    critical_section(|| {
                        self.wait_for_others(
                            data,
                            updates,
                            events,
                            is_nested_invocation,
                            sanitized_argv,
                        )
                    })
                    .await,
                    DiceSynchronizeSectionEnd {},
                )
            })
            .await?;

        Ok(exec(transaction).await)
    }

    #[allow(clippy::await_holding_lock)]
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
    ) -> anyhow::Result<(OnExecExit, DiceTransaction)> {
        let trace = event_dispatcher.trace_id().dupe();

        let span = tracing::span!(tracing::Level::DEBUG, "wait_for_others", trace = %trace);
        let _enter = span.enter();

        let mut data = self.data.lock();

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

                    if matches!(self.dice_cleanup_config, DiceCleanup::Block) {
                        drop(data);
                        event_dispatcher
                            .span_async(
                                buck2_data::DiceCleanupStart { epoch: epoch as _ },
                                async move { (future.await, buck2_data::DiceCleanupEnd {}) },
                            )
                            .await;
                        data = self.data.lock();
                    }

                    data.transition_to_idle(epoch);
                }
                DiceStatus::Available { active } => {
                    tracing::debug!("ActiveDice is available");

                    let dice_was_idle = self.dice.is_idle();

                    // we rerun the updates in case that files on disk have changed between commands.
                    // this might cause some churn, but concurrent commands don't happen much and
                    // isn't a big perf bottleneck. Dice should be able to resurrect nodes properly.

                    let transaction = event_dispatcher
                        .span_async(buck2_data::DiceStateUpdateStart {}, async {
                            (
                                async {
                                    let updater = self.dice.updater();
                                    let user_data =
                                        user_data.provide(&updater.existing_state().await).await?;
                                    let transaction =
                                        updates.update(updater).await?.commit_with_data(user_data);
                                    anyhow::Ok(transaction)
                                }
                                .await,
                                buck2_data::DiceStateUpdateEnd {},
                            )
                        })
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
                                break (transaction, state.will_taint());
                            }
                            BypassSemaphore::Block => {
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
                                                self.cond.wait(data).await,
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
            match self.nested_invocation_config {
                NestedInvocation::Error => BypassSemaphore::Error,
                NestedInvocation::Run => BypassSemaphore::Run(RunState::NestedDifferentState),
            }
        } else {
            match self.parallel_invocation_config {
                ParallelInvocation::Run => BypassSemaphore::Run(RunState::ParallelDifferentState),
                ParallelInvocation::Block => BypassSemaphore::Block,
            }
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
            RunState::NestedDifferentState => {
                soft_error!(
                    "nested_invocation_different_dice_state",
                    anyhow::anyhow!(
                        ConcurrencyHandlerError::NestedInvocationWithDifferentStates(
                            active_commands,
                            current_command.format_argv()
                        ),
                    )
                )?;
            }
            RunState::ParallelDifferentState => {
                soft_error!(
                    "parallel_invocation_different_dice_state",
                    anyhow::anyhow!(
                        ConcurrencyHandlerError::ParallelInvocationWithDifferentStates(
                            active_commands,
                        ),
                    )
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
struct OnExecExit(ConcurrencyHandler, CommandId);

impl OnExecExit {
    pub fn new(
        handler: ConcurrencyHandler,
        command: CommandId,
        data: CommandData,
        mut guard: MutexGuard<'_, RawFairMutex, ConcurrencyHandlerData>,
    ) -> Self {
        guard.active_commands.insert(command, data);
        Self(handler, command)
    }
}

impl Drop for OnExecExit {
    fn drop(&mut self) {
        tracing::info!("Command has exited: {}", self.1);

        let mut data = self.0.data.lock();
        data.active_commands
            .remove(&self.1)
            .expect("command was active but not in active_commands");

        if data.has_no_active_commands() {
            // we notify all commands since we don't know how many can actually wake up and run
            // concurrently as several of the currently waiting commands could be "equivalent".
            // This could cause commands to wake up out of order and race, such that the longest
            // waiting command might not still be forced to wait. In reality, it is probably not
            // a terrible issue, as we are unlikely to have many concurrent commands, and people
            // are unlikely to usually care about the precise order they get to run.
            self.0.cond.notify_all()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::time::Duration;

    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_events::create_source_sink_pair;
    use buck2_events::dispatch::EventDispatcher;
    use buck2_events::trace::TraceId;
    use buck2_events::EventSource;
    use derivative::Derivative;
    use dice::DetectCycles;
    use dice::Dice;
    use dice::DiceComputations;
    use dice::DiceTransactionUpdater;
    use dice::InjectedKey;
    use dice::Key;
    use dice::UserComputationData;
    use dupe::Dupe;
    use more_futures::cancellable_future::with_structured_cancellation;
    use parking_lot::Mutex;
    use tokio::sync::Barrier;
    use tokio::sync::RwLock;

    use super::*;

    struct NoChanges;

    #[async_trait]
    impl DiceUpdater for NoChanges {
        async fn update(
            &self,
            ctx: DiceTransactionUpdater,
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

        fn compare(_x: &Self::Value, _y: &Self::Value) -> bool {
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
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(
            dice,
            NestedInvocation::Run,
            ParallelInvocation::Run,
            DiceCleanup::Block,
        );

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
        );

        let (r1, r2, r3) = futures::future::join3(fut1, fut2, fut3).await;
        r1.unwrap();
        r2.unwrap();
        r3.unwrap();
    }

    #[tokio::test]
    async fn nested_invocation_should_error() {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(
            dice,
            NestedInvocation::Error,
            ParallelInvocation::Run,
            DiceCleanup::Block,
        );

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

        let concurrency = ConcurrencyHandler::new(
            dice,
            NestedInvocation::Run,
            ParallelInvocation::Run,
            DiceCleanup::Block,
        );

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
        );

        let (r1, r2, r3) = futures::future::join3(fut1, fut2, fut3).await;
        r1.unwrap();
        r2.unwrap();
        r3.unwrap();
    }

    #[tokio::test]
    async fn parallel_invocation_different_traceid_blocks() -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(
            dice.dupe(),
            NestedInvocation::Run,
            ParallelInvocation::Block,
            DiceCleanup::Block,
        );

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

    fn has_taint_event(receiver: &mut impl EventSource) -> bool {
        while let Some(event) = receiver.try_receive() {
            match event.unpack_buck().unwrap().data() {
                buck2_data::buck_event::Data::Instant(i) => match &i.data {
                    Some(buck2_data::instant_event::Data::TagEvent(e)) => {
                        if e.tags.iter().any(|e| e == "concurrency-tainted") {
                            return true;
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        false
    }

    #[tokio::test]
    async fn parallel_invocation_different_traceid_bypass_semaphore() -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(
            dice.dupe(),
            NestedInvocation::Run,
            ParallelInvocation::Run,
            DiceCleanup::Block,
        );

        let traces1 = TraceId::new();
        let traces2 = traces1.dupe();
        let traces_different = TraceId::new();

        let (events1, sink1) = create_source_sink_pair();
        let (events2, sink2) = create_source_sink_pair();
        let (events3, sink3) = create_source_sink_pair();

        let barrier = Arc::new(Barrier::new(3));

        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();

            async move {
                concurrency
                    .enter(
                        EventDispatcher::new(traces1, sink1),
                        &TestDiceDataProvider,
                        &NoChanges,
                        |_| async move {
                            barrier.wait().await;
                        },
                        false,
                        Vec::new(),
                        None,
                    )
                    .await
            }
        });

        let fut2 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();

            async move {
                concurrency
                    .enter(
                        EventDispatcher::new(traces2, sink2),
                        &TestDiceDataProvider,
                        &NoChanges,
                        |_| async move {
                            barrier.wait().await;
                        },
                        false,
                        Vec::new(),
                        None,
                    )
                    .await
            }
        });

        let fut3 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();

            async move {
                concurrency
                    .enter(
                        EventDispatcher::new(traces_different, sink3),
                        &TestDiceDataProvider,
                        &CtxDifferent,
                        |_| async move {
                            barrier.wait().await;
                        },
                        false,
                        Vec::new(),
                        None,
                    )
                    .await
            }
        });

        let (r1, r2, r3) = futures::future::join3(fut1, fut2, fut3).await;
        r1??;
        r2??;
        r3??;

        for mut events in [events1, events2, events3] {
            assert!(has_taint_event(&mut events));
        }

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
        async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
            let _guard = self.is_executing.lock();

            // TODO: use critical_section as it's simpler, but this stack doesn't have it and
            // this works equally well here :)
            with_structured_cancellation(|_obs| tokio::time::sleep(Duration::from_secs(1))).await;
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

        let concurrency = ConcurrencyHandler::new(
            dice.dupe(),
            NestedInvocation::Error,
            ParallelInvocation::Block,
            DiceCleanup::Block,
        );

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
            )
            .await?;

        Ok(())
    }

    #[tokio::test]
    async fn test_cleanup_skipped_reports_tainted() -> anyhow::Result<()> {
        let key = CleanupTestKey {
            is_executing: Arc::new(Mutex::new(())),
        };

        let key = &key;

        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(
            dice.dupe(),
            NestedInvocation::Error,
            ParallelInvocation::Block,
            DiceCleanup::Run,
        );

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
            )
            .await?;

        // Now, enter with a different context. We're skipping cleanup.

        let (mut events, sink) = create_source_sink_pair();

        concurrency
            .enter(
                EventDispatcher::new(TraceId::new(), sink),
                &TestDiceDataProvider,
                &CtxDifferent,
                |_dice| async move {
                    // Check that we did in fact re-enter before cleanup was done.
                    assert!(key.is_executing.is_locked());
                },
                false,
                Vec::new(),
                None,
            )
            .await?;

        assert!(has_taint_event(&mut events));

        Ok(())
    }

    #[tokio::test]
    async fn test_thundering_herd() -> anyhow::Result<()> {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new(
            dice.dupe(),
            NestedInvocation::Error,
            ParallelInvocation::Block,
            DiceCleanup::Block,
        );

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
                )
                .await
        });

        futures::future::try_join_all(tasks).await?;

        assert!(!concurrency.data.lock().previously_tainted);

        Ok(())
    }
}
