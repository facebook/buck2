/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Handles command concurrency.
//!
//! `buck2` supports limited concurrency for commands.
//! If there are no buckconfig changes, nor file changes, then commands can be allowed to execute
//! concurrently. Otherwise, `buck2` will block waiting for other commands to finish.

use std::collections::VecDeque;
use std::fmt::Debug;
use std::sync::Arc;

use allocative::Allocative;
use async_condvar_fair::Condvar;
use async_trait::async_trait;
use buck2_cli_proto::client_context::ExitWhen;
use buck2_cli_proto::client_context::PreemptibleWhen;
use buck2_core::soft_error;
use buck2_data::CommandPreempted;
use buck2_data::DiceBlockConcurrentCommandEnd;
use buck2_data::DiceBlockConcurrentCommandStart;
use buck2_data::DiceEqualityCheck;
use buck2_data::DiceSynchronizeSectionEnd;
use buck2_data::DiceSynchronizeSectionStart;
use buck2_data::ExclusiveCommandWaitEnd;
use buck2_data::ExclusiveCommandWaitStart;
use buck2_data::NoActiveDiceState;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_util::early_command_timing::EXCLUSIVE_COMMAND_WAIT;
use buck2_util::early_command_timing::EarlyCommandTimingBuilder;
use buck2_util::truncate::truncate;
use buck2_wrapper_common::invocation_id::TraceId;
use derive_more::Display;
use dice::Dice;
use dice::DiceEquality;
use dice::DiceTransaction;
use dice::DiceTransactionUpdater;
use dice::UserComputationData;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::future;
use futures::future::BoxFuture;
use futures::future::Either;
use futures::future::Future;
use futures::future::FutureExt;
use futures::future::Shared;
use futures::pin_mut;
use itertools::Itertools;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use tokio::sync::Mutex;
use tokio::sync::MutexGuard;
use tokio::sync::oneshot;
use tokio::sync::oneshot::error::RecvError;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum ConcurrencyHandlerError {
    #[error(
        "Recursive invocation of Buck, which is discouraged, but will probably work (using the same state). Trace Ids: {0}. Recursive invocation command: `{1}`"
    )]
    NestedInvocationWithSameStates(String, String),
    #[error(
        "Recursive invocation of Buck, with a different state. Use `--isolation-dir` on the inner invocation to fix this. Trace Ids: {0}. Recursive invocation command: `{1}`"
    )]
    #[buck2(input)]
    NestedInvocationWithDifferentStates(String, String),
    #[error("`--exit-when=differentstate` was set")]
    #[buck2(tag = DaemonIsBusy)]
    ExitWhenDifferentState,

    #[error("`--preemptible` was set, and buck daemon preempted this command as another came in.")]
    #[buck2(tag = DaemonPreempted)]
    ExitOnPreemption,

    #[error("`--exit-when=notidle` was set, and buck daemon is not idle.")]
    #[buck2(tag = DaemonIsBusy)]
    ExitOnDaemonNotIdle,
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
#[derive(Allocative)]
pub struct ConcurrencyHandler {
    data: Mutex<ConcurrencyHandlerData>,
    // use an async condvar because the `wait` to `notify` spans across an async function (namely
    // the entire command execution).
    #[allocative(skip)]
    cond: Condvar,
    dice: Arc<Dice>,
    /// Used to prevent commands (clean --stale) from running in parallel with dice commands
    exclusive_command_lock: ExclusiveCommandLock,
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
    #[allocative(skip)]
    events: Arc<dyn CommandEventSink>,
    preemption_setting: PreemptibleWhen,
    #[allocative(skip)]
    preempt: Option<oneshot::Sender<()>>,
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
        self.events.instant(
            buck2_data::TagEvent {
                tags: vec!["concurrency-tainted".to_owned()],
            }
            .into(),
        );
    }

    fn notify_previously_tainted(&self) {
        self.events.instant(
            buck2_data::TagEvent {
                tags: vec!["concurrency-previously-tainted".to_owned()],
            }
            .into(),
        );
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
}

impl DiceStatus {
    fn idle() -> Self {
        Self::Available { active: None }
    }

    fn active(version: DiceEquality) -> Self {
        Self::Available {
            active: Some(ActiveDice { version }),
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
}

/// Object-safe half of the event interface: what a *registered* command needs. Stored per command,
/// so it must not be generic.
pub trait CommandEventSink: Send + Sync + 'static {
    fn instant(&self, data: buck2_data::instant_event::Data);
    fn trace_id(&self) -> &TraceId;
}

/// Full event interface, including span wrapping. Not object-safe, because the span method is
/// generic over the wrapped future's output, so callers take it as a type parameter.
///
/// `span` must preserve span-entering semantics: spans created inside `fut` parent to this span,
/// and poll time accumulates into the end event. Delegating to an implementation built on
/// `EventDispatcher::span_async` does this; starting a span, awaiting, then ending it does not.
pub trait CommandEvents: CommandEventSink + Dupe {
    fn span<'a, R: Send + 'a>(
        &self,
        start: buck2_data::span_start_event::Data,
        fut: BoxFuture<'a, (R, buck2_data::span_end_event::Data)>,
    ) -> BoxFuture<'a, R>;

    fn sink(&self) -> Arc<dyn CommandEventSink>;
}

#[async_trait]
pub trait DiceUpdater: Send + Sync {
    async fn update(
        &self,
        mut ctx: DiceTransactionUpdater,
        early_timings: &mut EarlyCommandTimingBuilder,
    ) -> buck2_error::Result<(DiceTransactionUpdater, UserComputationData)>;
}

/// Per-command work that needs the committed `DiceTransaction` but is not part of deciding whether
/// the command may run. Returning `Err` fails the command.
///
/// This is invoked while the concurrency lock is held, immediately after the transaction is
/// committed and before the command is registered as active. Implementations that compare against
/// state shared between commands rely on that ordering, so moving the call is a behavioural change,
/// not a refactor.
#[async_trait]
pub trait CommandTransactionObserver: Send + Sync {
    async fn on_transaction_committed(
        &self,
        transaction: &DiceTransaction,
    ) -> buck2_error::Result<()>;
}

#[derive(Allocative)]
struct ExclusiveCommandLock {
    lock: tokio::sync::RwLock<()>,
    owning_command: Arc<parking_lot::Mutex<VecDeque<String>>>,
}

#[allow(dead_code)] // fields never read
enum ExclusiveCommandLockGuard<'a> {
    Shared(tokio::sync::RwLockReadGuard<'a, ()>),
    Exclusive(
        tokio::sync::RwLockWriteGuard<'a, ()>,
        Arc<parking_lot::Mutex<VecDeque<String>>>,
    ),
}

impl Drop for ExclusiveCommandLockGuard<'_> {
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
    /// Helper method to format active commands into a string
    fn format_active_commands(data: &ConcurrencyHandlerData) -> String {
        let active_commands: Vec<String> = data
            .active_commands
            .values()
            .map(|d| TraceId::to_string(&d.trace_id))
            .collect();

        active_commands.join(", ")
    }

    pub fn new(dice: Arc<Dice>) -> Arc<Self> {
        Arc::new(ConcurrencyHandler {
            data: Mutex::new(ConcurrencyHandlerData {
                dice_status: DiceStatus::idle(),
                active_commands: SmallMap::new(),
                next_command_id: CommandId(0),
                cleanup_epoch: 0,
                previously_tainted: false,
            }),
            cond: Condvar::new(),
            dice,
            exclusive_command_lock: ExclusiveCommandLock::new(),
        })
    }

    /// Enters a critical section that requires concurrent command synchronization,
    /// and runs the given `exec` function in the critical section.
    pub async fn enter<F, Fut, R, E>(
        self: &Arc<Self>,
        events: E,
        updates: &dyn DiceUpdater,
        exec: F,
        is_nested_invocation: bool,
        sanitized_argv: Vec<String>,
        exclusive_cmd: Option<String>,
        cancellations: &CancellationContext,
        preemptible: PreemptibleWhen,
        transaction_observer: &dyn CommandTransactionObserver,
        exit_when: ExitWhen,
        mut early_command_timing: EarlyCommandTimingBuilder,
    ) -> buck2_error::Result<R>
    where
        F: FnOnce(DiceTransaction, EarlyCommandTimingBuilder) -> Fut,
        Fut: Future<Output = R> + Send,
        E: CommandEvents,
    {
        let _exclusive_command_guard = events
            .span(
                ExclusiveCommandWaitStart {
                    command_name: self.exclusive_command_lock.owning_command(),
                }
                .into(),
                Box::pin({
                    let early_command_timing = &mut early_command_timing;
                    async move {
                        let guard = if let Some(cmd_name) = exclusive_cmd {
                            early_command_timing.start_span(EXCLUSIVE_COMMAND_WAIT.to_owned());
                            let guard = self.exclusive_command_lock.exclusive_lock(cmd_name).await;
                            self.dice.wait_for_idle().await;

                            guard
                        } else {
                            self.exclusive_command_lock.shared_lock().await
                        };
                        (guard, ExclusiveCommandWaitEnd {}.into())
                    }
                }),
            )
            .await;

        let inner_events = events.dupe();
        let (_guard, transaction, preempt_receiver) = events
            .span(DiceSynchronizeSectionStart {}.into(), {
                let early_command_timing = &mut early_command_timing;

                Box::pin(async move {
                    (
                        cancellations
                            .critical_section(|| {
                                self.wait_for_others(
                                    updates,
                                    early_command_timing,
                                    inner_events,
                                    is_nested_invocation,
                                    sanitized_argv,
                                    preemptible,
                                    transaction_observer,
                                    exit_when,
                                )
                            })
                            .await,
                        DiceSynchronizeSectionEnd {}.into(),
                    )
                })
            })
            .await?;

        let result = exec(transaction, early_command_timing);
        pin_mut!(result);
        pin_mut!(preempt_receiver);

        match future::select(result, preempt_receiver).await {
            Either::Left((result, _)) => Ok(result),
            Either::Right((_preemption, _)) => {
                events.instant(CommandPreempted {}.into());
                Err(ConcurrencyHandlerError::ExitOnPreemption.into())
            }
        }
    }

    // this is normally super unsafe, but because we are using an async condvar that takes care
    // of unlocking this mutex, this mutex is actually essentially never held across awaits.
    // The async condvar will handle properly allowing under threads to proceed, avoiding
    // starvation.
    async fn wait_for_others<E: CommandEvents>(
        self: &Arc<Self>,
        updates: &dyn DiceUpdater,
        early_timings: &mut EarlyCommandTimingBuilder,
        events: E,
        is_nested_invocation: bool,
        sanitized_argv: Vec<String>,
        preemptible: PreemptibleWhen,
        transaction_observer: &dyn CommandTransactionObserver,
        exit_when: ExitWhen,
    ) -> buck2_error::Result<(
        OnExecExit,
        DiceTransaction,
        impl Future<Output = Result<(), RecvError>> + use<E>,
    )> {
        // Have to put it on the function unfortunately, https://github.com/rust-lang/rust-clippy/issues/9047
        #![allow(clippy::await_holding_invalid_type)]

        let trace = events.trace_id().dupe();

        let span = tracing::span!(tracing::Level::DEBUG, "wait_for_others", trace = %trace);
        // FIXME(JakobDegen): Clippy points out that tracing won't know when this future gets
        // descheduled from this executor thread, so this may show up in the wrong places
        let _enter = span.enter();

        let mut data = self.data.lock().await;

        let command_id = data.next_command_id.increment();

        let (preempt_sender, preempt_receiver) = oneshot::channel::<()>();

        let command_data = CommandData {
            trace_id: trace.dupe(),
            argv: sanitized_argv,
            events: events.sink(),
            preemption_setting: preemptible,
            preempt: Some(preempt_sender),
        };

        let (transaction, tainted) = loop {
            match &data.dice_status {
                DiceStatus::Cleanup { future, epoch } => {
                    tracing::debug!("ActiveDice is in cleanup");
                    let future = future.clone();
                    let epoch = *epoch;

                    // block while dice cleans up
                    drop(data);
                    events
                        .span(
                            buck2_data::DiceCleanupStart { epoch: epoch as _ }.into(),
                            Box::pin(async move {
                                (future.await, buck2_data::DiceCleanupEnd {}.into())
                            }),
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

                        let (transaction, user_data) =
                            updates.update(updater, early_timings).await?;

                        let transaction = events
                            .span(
                                buck2_data::DiceStateUpdateStart {}.into(),
                                Box::pin(async {
                                    (
                                        async {
                                            let transaction =
                                                transaction.commit_with_data(user_data).await;
                                            buck2_error::Ok(transaction)
                                        }
                                        .await,
                                        buck2_data::DiceStateUpdateEnd {}.into(),
                                    )
                                }),
                            )
                            .await?;
                        buck2_error::Ok(transaction)
                    }
                    .await?;

                    if let Some(active) = active {
                        // If the --exit-when=notidle option is set for the current command and there is
                        // another command running already, exit immediately with a "daemon is busy" error.
                        if matches!(exit_when, ExitWhen::ExitNotIdle)
                            && !data.active_commands.is_empty()
                        {
                            return Err(ConcurrencyHandlerError::ExitOnDaemonNotIdle)
                                .with_buck_error_context(|| {
                                    format!(
                                        "Buck daemon is busy processing another command: {}",
                                        Self::format_active_commands(&data)
                                    )
                                });
                        }

                        let is_same_state = transaction.equivalent(&active.version);

                        // If we have a different state, attempt to transition to cleanup. This will
                        // succeed only if the current state is not in use.
                        if !is_same_state {
                            // If the active commands are preemptible, preempt them.
                            self.cancel_preemptible_commands(&mut data, is_same_state);

                            // transition to cleanup == "wait until all other blocking commands finish"
                            if data.transition_to_cleanup(&self.dice) {
                                continue;
                            }
                        }

                        tracing::debug!("ActiveDice has an active_transaction");

                        events.instant(
                            DiceEqualityCheck {
                                is_equal: is_same_state,
                            }
                            .into(),
                        );

                        let bypass_semaphore =
                            self.determine_bypass_semaphore(is_same_state, is_nested_invocation);

                        match bypass_semaphore {
                            BypassSemaphore::Error => {
                                return Err(
                                    ConcurrencyHandlerError::NestedInvocationWithDifferentStates(
                                        format_traces(&data.active_commands, &command_data),
                                        command_data.format_argv(),
                                    )
                                    .into(),
                                );
                            }
                            BypassSemaphore::Run(state) => {
                                self.emit_logs(state, &data.active_commands, &command_data)?;
                                self.cancel_preemptible_commands(&mut data, is_same_state);
                                break (transaction, false);
                            }
                            BypassSemaphore::Block => {
                                let early_exit_error: Option<ConcurrencyHandlerError> =
                                    if matches!(exit_when, ExitWhen::ExitDifferentState) {
                                        Some(ConcurrencyHandlerError::ExitWhenDifferentState)
                                    } else {
                                        None
                                    };
                                if let Some(early_exit_error) = early_exit_error {
                                    return Err(early_exit_error).with_buck_error_context(|| {
                                        format!(
                                            "Buck daemon is busy processing another command: {}",
                                            Self::format_active_commands(&data)
                                        )
                                    });
                                }
                                // We should probably show more than the first here, but for now
                                // this is what we have.
                                //
                                // Note: unwrap here relies on the fact that transition_to_cleanup
                                // would have transitioned if we had no active commands.

                                let active_command = data.active_commands.first().unwrap().1;
                                let trace_id = active_command.trace_id.dupe();
                                let argv = active_command.format_argv();

                                data = events
                                    .span(
                                        DiceBlockConcurrentCommandStart {
                                            current_active_trace_id: trace_id.to_string(),
                                            cmd_args: argv,
                                        }
                                        .into(),
                                        Box::pin(async {
                                            (
                                                self.cond.wait((data, &self.data)).await,
                                                DiceBlockConcurrentCommandEnd {
                                                    ending_active_trace_id: trace_id.to_string(),
                                                }
                                                .into(),
                                            )
                                        }),
                                    )
                                    .await;
                            }
                        }
                    } else {
                        tracing::debug!("ActiveDice has no active_transaction");
                        events.instant(NoActiveDiceState {}.into());
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
            // Only the current command is notified, because there is never another one to tell.
            // Taint is only set on the branch that installs a fresh `ActiveDice`, which requires
            // `dice_status` to be `Available { active: None }`. That state implies an empty
            // `active_commands`: the only route back to it is `transition_to_idle`, reachable only
            // from `Cleanup`, and `transition_to_cleanup` refuses to enter `Cleanup` unless
            // `has_no_active_commands()`. Relaxing that guard would make this assertion fire.
            debug_assert!(
                data.has_no_active_commands(),
                "taint implies no registered commands; see transition_to_cleanup's guard"
            );
            command_data.notify_tainted();
            data.previously_tainted = true;
        }

        transaction_observer
            .on_transaction_committed(&transaction)
            .await?;

        // create the on exit drop handler, which will take care of notifying tasks.
        let drop_guard = OnExecExit::new(self.dupe(), command_id, command_data, data)?;
        // This adds the task to the list of all tasks (see ::new impl)

        Ok((drop_guard, transaction, preempt_receiver))
    }

    /// Access dice without locking for dumps.
    pub fn unsafe_dice(&self) -> &Arc<Dice> {
        &self.dice
    }

    fn cancel_preemptible_commands(&self, data: &mut ConcurrencyHandlerData, is_same_state: bool) {
        // If the active commands are preemptible, interrupt them.
        for cmd in data.active_commands.values_mut() {
            if cmd.preemption_setting == PreemptibleWhen::Never {
                continue;
            }
            if is_same_state && cmd.preemption_setting == PreemptibleWhen::OnDifferentState {
                continue;
            }
            if let Some(preempt) = cmd.preempt.take() {
                let _ = preempt.send(());
            }
        }
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
    ) -> buck2_error::Result<()> {
        let active_commands = format_traces(active_commands, current_command);

        if let RunState::NestedSameState = state {
            soft_error!(
                "nested_invocation_same_dice_state",
                ConcurrencyHandlerError::NestedInvocationWithSameStates(
                    active_commands,
                    current_command.format_argv(),
                )
                .into(),
                error_on_oss: true
            )?;
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
struct OnExecExit(Option<(Arc<ConcurrencyHandler>, CommandId)>);

impl OnExecExit {
    pub fn new(
        handler: Arc<ConcurrencyHandler>,
        command: CommandId,
        data: CommandData,
        mut guard: MutexGuard<'_, ConcurrencyHandlerData>,
    ) -> buck2_error::Result<Self> {
        let prev = guard.active_commands.insert(command, data);
        if prev.is_some() {
            return Err(internal_error!(
                "command id `{command}` is already registered"
            ));
        }
        Ok(OnExecExit(Some((handler, command))))
    }
}

impl Drop for OnExecExit {
    fn drop(&mut self) {
        let this = self.0.take().expect("dropped twice");
        tracing::info!("Command has exited: {}", this.1);

        tokio::task::spawn(async move {
            let mut data = this.0.data.lock().await;
            data.active_commands
                .shift_remove(&this.1)
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
    use std::task::Poll;
    use std::time::Duration;
    use std::time::Instant;

    use allocative::Allocative;
    use assert_matches::assert_matches;
    use async_trait::async_trait;
    use buck2_core::is_open_source;
    use buck2_util::early_command_timing::EXCLUSIVE_COMMAND_WAIT;
    use buck2_util::early_command_timing::FILE_WATCHER_WAIT;
    use derivative::Derivative;
    use dice::DetectCycles;
    use dice::DiceComputations;
    use dice::EqualityBehavior;
    use dice::InjectedKey;
    use dice::Key;
    use dice::PagableValueSerialize;
    use dice::ValueSerialize;
    use dice_futures::cancellation::CancellationContext;
    use dupe::Dupe;
    use futures::pin_mut;
    use futures::poll;
    use pagable::Pagable;
    use pagable::pagable_typetag;
    use parking_lot::Mutex;
    use tokio::sync::Barrier;
    use tokio::sync::RwLock;

    use super::*;

    /// Recording stand-in for `EventDispatcher`. The real one lives in `buck2_events`, which this
    /// crate deliberately does not depend on — including in `test_deps`, since those are linked
    /// into the `-unittest` binary that coverage instruments.
    #[derive(Clone, Dupe)]
    struct TestEvents(Arc<TestEventsInner>);

    struct TestEventsInner {
        trace_id: TraceId,
        recorded: Mutex<Vec<RecordedEvent>>,
    }

    #[derive(Clone, Debug)]
    enum RecordedEvent {
        Instant(buck2_data::instant_event::Data),
        SpanStart(buck2_data::span_start_event::Data),
        SpanEnd(buck2_data::span_end_event::Data),
    }

    impl TestEvents {
        fn new() -> Self {
            Self::with_trace(TraceId::new())
        }

        fn with_trace(trace_id: TraceId) -> Self {
            Self(Arc::new(TestEventsInner {
                trace_id,
                recorded: Mutex::new(Vec::new()),
            }))
        }

        fn recorded(&self) -> Vec<RecordedEvent> {
            self.0.recorded.lock().clone()
        }

        /// Waits for a recorded event matching `pred`. Bounded, because a regression should fail in
        /// seconds rather than hang the harness.
        async fn wait_for<F>(&self, pred: F) -> buck2_error::Result<RecordedEvent>
        where
            F: Fn(&RecordedEvent) -> bool,
        {
            self.wait_from(&mut 0, pred).await
        }

        /// As [`Self::wait_for`], but resumes from `cursor` and advances it past the match, so a
        /// sequence of waits observes distinct events the way reading from a channel did.
        async fn wait_from<F>(
            &self,
            cursor: &mut usize,
            pred: F,
        ) -> buck2_error::Result<RecordedEvent>
        where
            F: Fn(&RecordedEvent) -> bool,
        {
            // Short timeouts are too flaky in OD environments under load.
            let (idx, event) = tokio::time::timeout(Duration::from_secs(10), async {
                loop {
                    let recorded = self.recorded();
                    if let Some((i, e)) = recorded
                        .iter()
                        .enumerate()
                        .skip(*cursor)
                        .find(|(_, e)| pred(e))
                    {
                        break (i, e.clone());
                    }
                    tokio::task::yield_now().await;
                }
            })
            .await
            .buck_error_context("Timed out waiting for a matching event")?;
            *cursor = idx + 1;
            Ok(event)
        }
    }

    impl CommandEventSink for TestEvents {
        fn instant(&self, data: buck2_data::instant_event::Data) {
            self.0.recorded.lock().push(RecordedEvent::Instant(data));
        }

        fn trace_id(&self) -> &TraceId {
            &self.0.trace_id
        }
    }

    impl CommandEvents for TestEvents {
        /// Records only: no span parenting, poll timing, or `SpanCancelled` on drop.
        fn span<'a, R: Send + 'a>(
            &self,
            start: buck2_data::span_start_event::Data,
            fut: BoxFuture<'a, (R, buck2_data::span_end_event::Data)>,
        ) -> BoxFuture<'a, R> {
            self.0.recorded.lock().push(RecordedEvent::SpanStart(start));
            let this = self.dupe();
            Box::pin(async move {
                let (r, end) = fut.await;
                this.0.recorded.lock().push(RecordedEvent::SpanEnd(end));
                r
            })
        }

        fn sink(&self) -> Arc<dyn CommandEventSink> {
            Arc::new(self.dupe())
        }
    }

    /// Matches a recorded `TagEvent` carrying `tag`.
    fn is_tag_event(tag: &'static str) -> impl Fn(&RecordedEvent) -> bool {
        move |e: &RecordedEvent| match e {
            RecordedEvent::Instant(buck2_data::instant_event::Data::TagEvent(t)) => {
                t.tags.iter().any(|it| it == tag)
            }
            _ => false,
        }
    }

    /// The production observer emits buckconfig telemetry; concurrency behaviour does not depend
    /// on it, so tests use one that does nothing.
    struct NoTelemetry;

    #[async_trait]
    impl CommandTransactionObserver for NoTelemetry {
        async fn on_transaction_committed(
            &self,
            _transaction: &DiceTransaction,
        ) -> buck2_error::Result<()> {
            Ok(())
        }
    }

    struct NoChanges;

    #[async_trait]
    impl DiceUpdater for NoChanges {
        async fn update(
            &self,
            ctx: DiceTransactionUpdater,
            _early_timings: &mut EarlyCommandTimingBuilder,
        ) -> buck2_error::Result<(DiceTransactionUpdater, UserComputationData)> {
            Ok((ctx, Default::default()))
        }
    }

    struct CtxDifferent;

    #[async_trait]
    impl DiceUpdater for CtxDifferent {
        async fn update(
            &self,
            mut ctx: DiceTransactionUpdater,
            _early_timings: &mut EarlyCommandTimingBuilder,
        ) -> buck2_error::Result<(DiceTransactionUpdater, UserComputationData)> {
            ctx.changed_to(vec![(K, ())])?;
            Ok((ctx, Default::default()))
        }
    }

    #[derive(Clone, Dupe, Display, Debug, Hash, Eq, PartialEq, Allocative, Pagable)]
    #[pagable_typetag(dice::DiceKeyDyn)]
    struct K;

    #[async_trait]
    impl InjectedKey for K {
        type Value = ();

        fn equality_behavior() -> EqualityBehavior<Self::Value> {
            EqualityBehavior::Compare(|_x, _y| false)
        }

        fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
            PagableValueSerialize::<Self::Value>::new()
        }
    }

    /// The concurrency manager itself reads no injected keys; the buckconfig data the old inline
    /// telemetry needed is now the observer's concern, and tests use `NoTelemetry`.
    fn make_default_dice() -> Arc<Dice> {
        Dice::builder().build(DetectCycles::Enabled)
    }

    /// Builder for a test call to [`ConcurrencyHandler::enter`], which otherwise takes twelve
    /// arguments of which most tests care about one or two. Defaults describe a plain command:
    /// not nested, not exclusive, never preemptible, never exiting early.
    struct TestCommand {
        dispatcher: TestEvents,
        preemptible: PreemptibleWhen,
        exit_when: ExitWhen,
        is_nested_invocation: bool,
    }

    impl TestCommand {
        fn new() -> Self {
            Self {
                dispatcher: TestEvents::new(),
                preemptible: PreemptibleWhen::Never,
                exit_when: ExitWhen::ExitNever,
                is_nested_invocation: false,
            }
        }

        fn dispatcher(mut self, dispatcher: TestEvents) -> Self {
            self.dispatcher = dispatcher;
            self
        }

        fn preemptible(mut self, preemptible: PreemptibleWhen) -> Self {
            self.preemptible = preemptible;
            self
        }

        async fn run<F, Fut, R>(
            self,
            concurrency: &Arc<ConcurrencyHandler>,
            updates: &dyn DiceUpdater,
            exec: F,
        ) -> buck2_error::Result<R>
        where
            F: FnOnce(DiceTransaction, EarlyCommandTimingBuilder) -> Fut,
            Fut: Future<Output = R> + Send,
        {
            concurrency
                .enter(
                    self.dispatcher,
                    updates,
                    exec,
                    self.is_nested_invocation,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    self.preemptible,
                    &NoTelemetry,
                    self.exit_when,
                    EarlyCommandTimingBuilder::new(Instant::now()),
                )
                .await
        }
    }

    /// Waits until finished commands have been deregistered.
    ///
    /// `enter` returning does not mean the command is no longer registered: `OnExecExit::drop` only
    /// spawns the removal, so the entry lingers in `active_commands` until that detached task
    /// acquires the lock. Anything that observes registration afterwards — `ExitWhen::ExitNotIdle`
    /// in particular — races the reaper without this.
    async fn wait_for_commands_to_be_reaped(
        concurrency: &ConcurrencyHandler,
    ) -> buck2_error::Result<()> {
        // Short timeouts are too flaky in OD environments under load.
        tokio::time::timeout(Duration::from_secs(10), async {
            while !concurrency.data.lock().await.has_no_active_commands() {
                tokio::task::yield_now().await;
            }
        })
        .await
        .buck_error_context("Timed out waiting for finished commands to be deregistered")
    }

    #[tokio::test]
    async fn nested_invocation_same_transaction() {
        // FIXME: This times out on open source, and we don't know why
        if is_open_source() {
            return;
        }
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice);

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();
        let traces3 = TraceId::new();

        let barrier = Arc::new(Barrier::new(3));

        let fut1 = concurrency.enter(
            TestEvents::with_trace(traces1),
            &NoChanges,
            |_, _timing| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            true,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            &NoTelemetry,
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );
        let fut2 = concurrency.enter(
            TestEvents::with_trace(traces2),
            &NoChanges,
            |_, _timing| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            true,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            &NoTelemetry,
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );
        let fut3 = concurrency.enter(
            TestEvents::with_trace(traces3),
            &NoChanges,
            |_, _timing| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            true,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            &NoTelemetry,
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );

        let (r1, r2, r3) = futures::future::join3(fut1, fut2, fut3).await;
        r1.unwrap();
        r2.unwrap();
        r3.unwrap();
    }

    #[tokio::test]
    async fn nested_invocation_should_error() {
        let dice = make_default_dice();

        let concurrency = ConcurrencyHandler::new(dice);

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        let barrier = Arc::new(Barrier::new(2));

        let fut1 = concurrency.enter(
            TestEvents::with_trace(traces1),
            &NoChanges,
            |_, _timing| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            true,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            &NoTelemetry,
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );

        let fut2 = concurrency.enter(
            TestEvents::with_trace(traces2),
            &CtxDifferent,
            |_, _timing| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            true,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            &NoTelemetry,
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
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
        let dice = make_default_dice();

        let concurrency = ConcurrencyHandler::new(dice);

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();
        let traces3 = TraceId::new();

        let barrier = Arc::new(Barrier::new(3));

        let fut1 = concurrency.enter(
            TestEvents::with_trace(traces1),
            &NoChanges,
            |_, _timing| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            false,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            &NoTelemetry,
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );
        let fut2 = concurrency.enter(
            TestEvents::with_trace(traces2),
            &NoChanges,
            |_, _timing| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            false,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            &NoTelemetry,
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );
        let fut3 = concurrency.enter(
            TestEvents::with_trace(traces3),
            &NoChanges,
            |_, _timing| {
                let b = barrier.dupe();
                async move {
                    b.wait().await;
                }
            },
            false,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            &NoTelemetry,
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );

        let (r1, r2, r3) = futures::future::join3(fut1, fut2, fut3).await;
        r1.unwrap();
        r2.unwrap();
        r3.unwrap();
    }

    #[tokio::test]
    async fn parallel_invocation_different_traceid_blocks() -> buck2_error::Result<()> {
        let dice = make_default_dice();

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
                        TestEvents::with_trace(traces1),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
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
                        TestEvents::with_trace(traces2),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
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
                        TestEvents::with_trace(traces_different),
                        &CtxDifferent,
                        |_, _timing| async move {
                            arrived.store(true, Ordering::Relaxed);
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
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
    async fn parallel_invocation_exit_when_different_state() -> buck2_error::Result<()> {
        let dice = make_default_dice();

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
                        TestEvents::with_trace(traces1),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitDifferentState,
                        EarlyCommandTimingBuilder::new(Instant::now()),
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
                        TestEvents::with_trace(traces2),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitDifferentState,
                        EarlyCommandTimingBuilder::new(Instant::now()),
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
                        TestEvents::with_trace(traces_different),
                        &CtxDifferent,
                        |_, _timing| async move {
                            arrived.store(true, Ordering::Relaxed);
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitDifferentState,
                        EarlyCommandTimingBuilder::new(Instant::now()),
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

        let fut3_error: buck2_error::Error = fut3_result.unwrap_err();
        assert!(
            fut3_error
                .tags()
                .contains(&buck2_error::ErrorTag::DaemonIsBusy),
        );

        Ok(())
    }

    #[tokio::test]
    async fn parallel_invocation_exit_when_preemptible() -> buck2_error::Result<()> {
        let dice = make_default_dice();

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
                        TestEvents::with_trace(traces1),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Always,
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
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
                        TestEvents::with_trace(traces2),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
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
                        TestEvents::with_trace(traces_different),
                        &CtxDifferent,
                        |_, _timing| async move {
                            arrived.store(true, Ordering::Relaxed);
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
                    )
                    .await
            }
        });

        barrier2.wait().await;

        assert!(!arrived.load(Ordering::Relaxed));

        drop(blocked1);
        let fut1_result = fut1.await?;
        let fut1_error: buck2_error::Error = fut1_result.unwrap_err();
        assert!(
            fut1_error
                .tags()
                .contains(&buck2_error::ErrorTag::DaemonPreempted),
        );

        assert!(!arrived.load(Ordering::Relaxed));

        drop(blocked2);
        fut2.await??;
        fut3.await??;

        Ok(())
    }

    /// `PreemptibleWhen::OnDifferentState` must not preempt when the arriving command shares the
    /// active state. This is the `is_same_state` short circuit in `cancel_preemptible_commands`,
    /// reached only from the `BypassSemaphore::Run` call site.
    #[tokio::test]
    async fn on_different_state_survives_a_same_state_command() -> buck2_error::Result<()> {
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice);

        let block = Arc::new(RwLock::new(()));
        let blocked = block.write().await;
        let entered = Arc::new(AtomicBool::new(false));

        let updater = NoChanges;
        let preemptible = TestCommand::new()
            .preemptible(PreemptibleWhen::OnDifferentState)
            .run(&concurrency, &updater, {
                let entered = entered.dupe();
                let block = block.dupe();
                |_, _timing| async move {
                    entered.store(true, Ordering::Relaxed);
                    let _g = block.read().await;
                }
            });
        pin_mut!(preemptible);

        // Drive it until it is inside `exec` and therefore registered as active.
        while !entered.load(Ordering::Relaxed) {
            assert_matches!(poll!(&mut preemptible), Poll::Pending);
            tokio::task::yield_now().await;
        }

        // A command with the same state runs concurrently and completes.
        TestCommand::new()
            .run(&concurrency, &NoChanges, |_, _timing| async move {})
            .await?;

        // Still blocked on the guard, so it cannot have completed normally. Had it been preempted,
        // `enter`'s select would resolve with `DaemonPreempted` regardless of `exec` being stuck —
        // so staying pending is what proves it was not preempted.
        //
        // Asserting this *before* releasing the guard is load-bearing. Releasing first lets the
        // command finish normally in the same window, and `future::select` polls `exec` ahead of
        // the preempt channel, so a real preemption would be discarded and the test would pass
        // whether or not preemption fired.
        assert_matches!(poll!(&mut preemptible), Poll::Pending);

        drop(blocked);
        preemptible.await?;

        Ok(())
    }

    /// The other half of the matrix: `OnDifferentState` is preempted when the arriving command has
    /// a different state. Note the blocking guard is deliberately never released — preemption is
    /// what unblocks the first command, by dropping its `exec` future.
    #[tokio::test]
    async fn on_different_state_is_preempted_by_a_different_state_command()
    -> buck2_error::Result<()> {
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice);

        let block = Arc::new(RwLock::new(()));
        let _blocked = block.write().await;
        let entered = Arc::new(Barrier::new(2));

        let preemptible = tokio::spawn({
            let concurrency = concurrency.dupe();
            let entered = entered.dupe();
            let block = block.dupe();

            async move {
                TestCommand::new()
                    .preemptible(PreemptibleWhen::OnDifferentState)
                    .run(&concurrency, &NoChanges, |_, _timing| async move {
                        entered.wait().await;
                        let _g = block.read().await;
                    })
                    .await
            }
        });

        entered.wait().await;

        let different = tokio::spawn({
            let concurrency = concurrency.dupe();
            async move {
                TestCommand::new()
                    .run(&concurrency, &CtxDifferent, |_, _timing| async move {})
                    .await
            }
        });

        // Bounded: if preemption regresses, the first command stays blocked on a guard that is
        // never released, so without this the test deadlocks rather than failing.
        let result = tokio::time::timeout(Duration::from_secs(10), preemptible)
            .await
            .buck_error_context("Command was never preempted")?;

        let error: buck2_error::Error = result?.unwrap_err();
        assert!(
            error
                .tags()
                .contains(&buck2_error::ErrorTag::DaemonPreempted),
            "Command should have been preempted, got: {error}"
        );

        different.await??;

        Ok(())
    }

    /// The state machine's only externally visible signals. Integration tests key off these, so
    /// they are part of the contract: a command that installs a fresh DICE state reports
    /// `NoActiveDiceState`, and one that joins an equivalent state reports `DiceEqualityCheck`.
    #[tokio::test]
    async fn dice_state_transitions_are_reported_as_events() -> buck2_error::Result<()> {
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice);

        let first = TestEvents::new();
        TestCommand::new()
            .dispatcher(first.dupe())
            .run(&concurrency, &NoChanges, |_, _timing| async move {})
            .await?;

        first
            .wait_for(|e| {
                matches!(
                    e,
                    RecordedEvent::Instant(buck2_data::instant_event::Data::NoActiveDiceState(..))
                )
            })
            .await?;

        // The active DICE version is not cleared when a command exits, so the next command with an
        // equivalent state reuses it rather than installing a new one.
        let second = TestEvents::new();
        TestCommand::new()
            .dispatcher(second.dupe())
            .run(&concurrency, &NoChanges, |_, _timing| async move {})
            .await?;

        second
            .wait_for(|e| {
                matches!(
                    e,
                    RecordedEvent::Instant(buck2_data::instant_event::Data::DiceEqualityCheck(
                        DiceEqualityCheck { is_equal: true }
                    ))
                )
            })
            .await?;

        // Deliberately not asserted: whether a *different* state command emits
        // `DiceEqualityCheck { is_equal: false }` before blocking depends on whether the previous
        // command's entry has been reaped yet, and reaping happens on a detached task.

        Ok(())
    }

    /// A command that takes ownership of an unowned DICE state while work from a previous
    /// transaction is still winding down is tainted, and says so.
    #[tokio::test]
    async fn command_taking_over_non_idle_dice_is_tagged_tainted() -> buck2_error::Result<()> {
        let dice = make_default_dice();

        let key = CleanupTestKey {
            is_executing: Arc::new(Mutex::new(())),
        };
        let key = &key;

        // Abandon a transaction with a computation still running, leaving DICE a task pending
        // cancellation. `CleanupTestKey` holds a cancellation critical section for a second, so it
        // stays pending for the rest of the test.
        {
            let transaction = dice.updater().commit().await;

            let compute = transaction.compute(key).fuse();
            let started = async {
                while !key.is_executing.is_locked() {
                    tokio::task::yield_now().await;
                }
            }
            .fuse();

            futures::pin_mut!(compute);
            futures::pin_mut!(started);

            futures::select! {
                _ = compute => panic!("compute finished before started?"),
                _ = started => {}
            }
        }

        assert!(
            !dice.is_idle().await,
            "DICE should have a task pending cancellation"
        );

        let concurrency = ConcurrencyHandler::new(dice);
        let events = TestEvents::new();

        TestCommand::new()
            .dispatcher(events.dupe())
            .run(&concurrency, &NoChanges, |_, _timing| async move {})
            .await?;

        events.wait_for(is_tag_event("concurrency-tainted")).await?;

        assert!(
            !events
                .recorded()
                .iter()
                .any(is_tag_event("concurrency-previously-tainted")),
            "the command that causes the taint should not also report inheriting it"
        );

        assert!(
            concurrency.data.lock().await.previously_tainted,
            "Taint should latch for subsequent commands"
        );

        // The latch is only observable through the next command: taint is reported once by the
        // command that caused it, and thereafter as `concurrency-previously-tainted` by every
        // command that inherits the tainted state.
        let later = TestEvents::new();
        TestCommand::new()
            .dispatcher(later.dupe())
            .run(&concurrency, &NoChanges, |_, _timing| async move {})
            .await?;

        later
            .wait_for(is_tag_event("concurrency-previously-tainted"))
            .await?;

        assert!(
            !later
                .recorded()
                .iter()
                .any(is_tag_event("concurrency-tainted")),
            "A command inheriting taint should not report itself as the cause"
        );

        Ok(())
    }

    #[derive(Clone, Dupe, Derivative, Allocative, Display, Pagable)]
    #[derivative(Hash, Eq, PartialEq, Debug)]
    #[display("CleanupTestKey")]
    #[pagable_typetag(dice::DiceKeyDyn)]
    struct CleanupTestKey {
        #[derivative(Debug = "ignore", Hash = "ignore", PartialEq = "ignore")]
        #[pagable(discard = "Arc::new(Mutex::new(()))")]
        is_executing: Arc<Mutex<()>>,
    }

    #[async_trait::async_trait]
    impl Key for CleanupTestKey {
        type Value = ();

        #[allow(clippy::await_holding_lock)] // Intentional: testing exclusive access
        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            cancellation: &CancellationContext,
        ) -> Self::Value {
            let _guard = self.is_executing.lock();

            cancellation
                .critical_section(|| tokio::time::sleep(Duration::from_secs(1)))
                .await;
        }

        fn equality_behavior() -> EqualityBehavior<Self::Value> {
            EqualityBehavior::Compare(|_me, _other| true)
        }

        fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
            PagableValueSerialize::<Self::Value>::new()
        }
    }

    #[tokio::test]
    async fn test_cleanup_stage() -> buck2_error::Result<()> {
        let key = CleanupTestKey {
            is_executing: Arc::new(Mutex::new(())),
        };

        let key = &key;

        let dice = make_default_dice();

        let concurrency = ConcurrencyHandler::new(dice.dupe());

        // Kick off our computation and wait until it's running.

        concurrency
            .enter(
                TestEvents::new(),
                &NoChanges,
                |dice, _timing| async move {
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
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        // Now, re-enter. We expect to reuse and therefore to not wait.

        concurrency
            .enter(
                TestEvents::new(),
                &NoChanges,
                |_dice, _timing| async move {
                    // The key should still be evaluating by now.
                    assert!(key.is_executing.is_locked());
                },
                false,
                Vec::new(),
                None,
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        // Now, enter with a different context. This time, we expect to not reuse.

        concurrency
            .enter(
                TestEvents::new(),
                &CtxDifferent,
                |_dice, _timing| async move {
                    assert!(!key.is_executing.is_locked());
                },
                false,
                Vec::new(),
                None,
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        Ok(())
    }

    /// Waits for the start of an `ExclusiveCommandWait` span reporting `cmd` as the current owner.
    async fn wait_for_exclusive_span_start(
        events: &TestEvents,
        cursor: &mut usize,
        cmd: Option<&str>,
    ) -> buck2_error::Result<()> {
        let cmd = cmd.map(|c| c.to_owned());
        events
            .wait_from(cursor, |e| match e {
                RecordedEvent::SpanStart(
                    buck2_data::span_start_event::Data::ExclusiveCommandWait(
                        ExclusiveCommandWaitStart {
                            command_name: event_cmd,
                        },
                    ),
                ) => event_cmd == &cmd,
                _ => false,
            })
            .await?;
        Ok(())
    }

    /// Waits for the next `ExclusiveCommandWait` span end after `cursor`. The channel-based version
    /// of this test paired ends to starts by span id; the fake has no ids, so ordering after the
    /// cursor stands in for that pairing.
    async fn wait_for_exclusive_span_end(
        events: &TestEvents,
        cursor: &mut usize,
    ) -> buck2_error::Result<()> {
        events
            .wait_from(cursor, |e| {
                matches!(
                    e,
                    RecordedEvent::SpanEnd(buck2_data::span_end_event::Data::ExclusiveCommandWait(
                        _
                    ))
                )
            })
            .await?;
        Ok(())
    }

    #[tokio::test]
    #[allow(clippy::await_holding_lock)] // Intentional: testing exclusive access
    async fn exclusive_command_lock() -> buck2_error::Result<()> {
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());
        let events = TestEvents::new();
        let mut cursor = 0usize;

        let mutex = Arc::new(Mutex::new(()));
        let command = |exclusive_cmd: Option<&str>, barriers: Option<&Arc<(Barrier, Barrier)>>| {
            tokio::spawn({
                let concurrency = concurrency.dupe();
                let dispatcher = events.dupe();
                let barriers = barriers.map(|b| b.dupe());
                let exclusive_cmd = exclusive_cmd.map(|b| b.to_owned());
                let mutex = mutex.dupe();
                async move {
                    concurrency
                        .enter(
                            dispatcher,
                            &NoChanges,
                            |_, _timing| async move {
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
                            CancellationContext::testing(),
                            PreemptibleWhen::Never,
                            &NoTelemetry,
                            ExitWhen::ExitNever,
                            EarlyCommandTimingBuilder::new(Instant::now()),
                        )
                        .await
                }
            })
        };

        let non_exclusive_barriers = Arc::new((Barrier::new(2), Barrier::new(2)));
        // Start non_exclusive command and enter critical section
        let non_exclusive_fut = command(None, Some(&non_exclusive_barriers.dupe()));
        non_exclusive_barriers.0.wait().await;

        wait_for_exclusive_span_start(&events, &mut cursor, None).await?;
        wait_for_exclusive_span_end(&events, &mut cursor).await?;

        let command_barriers = Arc::new((Barrier::new(2), Barrier::new(2)));
        // Start exclusive command, blocked by non_exclusive
        let exclusive_fut_1 = command(Some("exclusive_1"), Some(&command_barriers.dupe()));

        wait_for_exclusive_span_start(&events, &mut cursor, None).await?;

        // Finish non_exclusive, enter exclusive_1 critical section
        non_exclusive_barriers.1.wait().await;
        non_exclusive_fut.await??;
        command_barriers.0.wait().await;

        wait_for_exclusive_span_end(&events, &mut cursor).await?;

        // Start series of exclusive commands and another second non_exclusive
        let exclusive_fut_2 = command(Some("exclusive_2"), None);
        wait_for_exclusive_span_start(&events, &mut cursor, Some("exclusive_1")).await?;
        let exclusive_fut_3 = command(Some("exclusive_3"), None);
        wait_for_exclusive_span_start(&events, &mut cursor, Some("exclusive_1")).await?;
        let non_exclusive_fut = command(None, None);
        wait_for_exclusive_span_start(&events, &mut cursor, Some("exclusive_1")).await?;

        // Unblock first exclusive command, remaining commands are unblocked
        command_barriers.1.wait().await;
        exclusive_fut_1.await??;
        exclusive_fut_2.await??;
        exclusive_fut_3.await??;
        non_exclusive_fut.await??;

        wait_for_exclusive_span_end(&events, &mut cursor).await?;
        wait_for_exclusive_span_end(&events, &mut cursor).await?;
        wait_for_exclusive_span_end(&events, &mut cursor).await?;
        Ok(())
    }

    #[tokio::test]
    async fn test_thundering_herd() -> buck2_error::Result<()> {
        let dice = make_default_dice();

        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let concurrency = &concurrency;

        let tasks = (0..3).map(|_i| async {
            concurrency
                .enter(
                    TestEvents::new(),
                    &CtxDifferent,
                    |dice, _timing| async move {
                        // NOTE: We need to actually compute something for DICE to be not-idle.
                        dice.compute(&K).await.unwrap();
                        tokio::task::yield_now().await;
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never,
                    &NoTelemetry,
                    ExitWhen::ExitNever,
                    EarlyCommandTimingBuilder::new(Instant::now()),
                )
                .await
        });

        buck2_util::future::try_join_all(tasks).await?;

        assert!(!concurrency.data.lock().await.previously_tainted);

        Ok(())
    }

    #[tokio::test]
    async fn test_updates_are_synchronized() -> buck2_error::Result<()> {
        async fn wait_on(b: &AtomicBool) {
            while !b.load(Ordering::Relaxed) {
                tokio::task::yield_now().await;
            }
        }

        let dice = make_default_dice();

        let concurrency = ConcurrencyHandler::new(dice.dupe());

        struct Updater {
            // Set when the updater enters the update function
            on_enter: AtomicBool,
            // Set to indicate that the updater should exit its update function
            allow_exit: AtomicBool,
        }
        #[async_trait]
        impl DiceUpdater for Updater {
            async fn update(
                &self,
                ctx: DiceTransactionUpdater,
                _early_timings: &mut EarlyCommandTimingBuilder,
            ) -> buck2_error::Result<(DiceTransactionUpdater, UserComputationData)> {
                self.on_enter.store(true, Ordering::Relaxed);
                wait_on(&self.allow_exit).await;
                Ok((ctx, Default::default()))
            }
        }

        let updater1 = Updater {
            on_enter: AtomicBool::new(false),
            allow_exit: AtomicBool::new(false),
        };
        let fut1 = concurrency.enter(
            TestEvents::new(),
            &updater1,
            |_dice, _timing| async move {
                tokio::task::yield_now().await;
            },
            false,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            &NoTelemetry,
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );
        pin_mut!(fut1);

        let updater2 = Updater {
            on_enter: AtomicBool::new(false),
            // We can set this to true immediately as we don't ever need the
            // second one to wait on anything
            allow_exit: AtomicBool::new(true),
        };
        let fut2 = concurrency.enter(
            TestEvents::new(),
            &updater2,
            |_dice, _timing| async move {
                tokio::task::yield_now().await;
            },
            false,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            &NoTelemetry,
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );
        pin_mut!(fut2);

        // Wait for the first updater's update to be entered
        tokio::select! {
            _ = &mut fut1 => panic!("First should not be able to exit yet"),
            _ = wait_on(&updater1.on_enter) => (),
        }

        // Now the first updater is blocked within its update function. Poll the
        // second one many times so that it makes as much progress as it can
        for _ in 0..100 {
            assert_matches!(poll!(&mut fut2), Poll::Pending);
        }
        // But it should not have entered its update yet
        assert!(
            !updater2.on_enter.load(Ordering::Relaxed),
            "Updaters are not correctly synchronized"
        );

        // Now unblock the first one and let both finish
        updater1.allow_exit.store(true, Ordering::Relaxed);
        let (a, b) = tokio::join!(fut1, fut2);
        a.unwrap();
        b.unwrap();

        Ok(())
    }

    #[tokio::test]
    async fn test_exit_when_not_idle_with_same_state() -> buck2_error::Result<()> {
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        let block1 = Arc::new(RwLock::new(()));
        let blocked1 = block1.write().await;

        let barrier = Arc::new(Barrier::new(2));

        // Start first command (same state, will run)
        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();
            let b = block1.dupe();

            async move {
                concurrency
                    .enter(
                        TestEvents::with_trace(traces1),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
                    )
                    .await
            }
        });

        barrier.wait().await;

        // Start second command with --exit-when=notidle (same state, should fail)
        let fut2 = tokio::spawn(buck2_util::async_move_clone!(concurrency, {
            concurrency
                .enter(
                    TestEvents::with_trace(traces2),
                    &NoChanges,
                    |_, _timing| async move {
                        // Should never reach here
                        panic!("Command should have failed before execution");
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never,
                    &NoTelemetry,
                    ExitWhen::ExitNotIdle,
                    EarlyCommandTimingBuilder::new(Instant::now()),
                )
                .await
        }));

        // Second command should fail immediately
        let fut2_result = fut2.await?;
        let fut2_error: buck2_error::Error = fut2_result.unwrap_err();
        assert!(
            fut2_error
                .tags()
                .contains(&buck2_error::ErrorTag::DaemonIsBusy),
            "Expected DaemonIsBusy error tag"
        );

        // Clean up first command
        drop(blocked1);
        fut1.await??;

        Ok(())
    }

    #[tokio::test]
    async fn test_exit_when_not_idle_with_different_state() -> buck2_error::Result<()> {
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        let block1 = Arc::new(RwLock::new(()));
        let blocked1 = block1.write().await;

        let barrier = Arc::new(Barrier::new(2));

        // Start first command (different state)
        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();
            let b = block1.dupe();

            async move {
                concurrency
                    .enter(
                        TestEvents::with_trace(traces1),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
                    )
                    .await
            }
        });

        barrier.wait().await;

        // Start second command with --exit-when=notidle (different state, should fail)
        let fut2 = tokio::spawn(buck2_util::async_move_clone!(concurrency, {
            concurrency
                .enter(
                    TestEvents::with_trace(traces2),
                    &CtxDifferent, // Different state
                    |_, _timing| async move {
                        // Should never reach here
                        panic!("Command should have failed before execution");
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never,
                    &NoTelemetry,
                    ExitWhen::ExitNotIdle,
                    EarlyCommandTimingBuilder::new(Instant::now()),
                )
                .await
        }));

        // Second command should fail immediately
        let fut2_result = fut2.await?;
        let fut2_error: buck2_error::Error = fut2_result.unwrap_err();
        assert!(
            fut2_error
                .tags()
                .contains(&buck2_error::ErrorTag::DaemonIsBusy),
            "Expected DaemonIsBusy error tag"
        );

        // Clean up first command
        drop(blocked1);
        fut1.await??;

        Ok(())
    }

    // This test was moved to the top of the file

    #[tokio::test]
    async fn test_multiple_exit_when_not_idle_commands_with_same_state() -> buck2_error::Result<()>
    {
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();
        let traces3 = TraceId::new();

        let block1 = Arc::new(RwLock::new(()));
        let blocked1 = block1.write().await;

        let barrier = Arc::new(Barrier::new(2));

        // Start first command with --exit-when=notidle
        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();
            let b = block1.dupe();

            async move {
                concurrency
                    .enter(
                        TestEvents::with_trace(traces1),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNotIdle,
                        EarlyCommandTimingBuilder::new(Instant::now()),
                    )
                    .await
            }
        });

        barrier.wait().await;

        // Start second and third commands with --exit-when=notidle (should both fail)
        let fut2 = tokio::spawn(buck2_util::async_move_clone!(concurrency, {
            concurrency
                .enter(
                    TestEvents::with_trace(traces2),
                    &NoChanges,
                    |_, _timing| async move {
                        panic!("Should not execute");
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never,
                    &NoTelemetry,
                    ExitWhen::ExitNotIdle,
                    EarlyCommandTimingBuilder::new(Instant::now()),
                )
                .await
        }));

        let fut3 = tokio::spawn(buck2_util::async_move_clone!(concurrency, {
            concurrency
                .enter(
                    TestEvents::with_trace(traces3),
                    &NoChanges,
                    |_, _timing| async move {
                        panic!("Should not execute");
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never,
                    &NoTelemetry,
                    ExitWhen::ExitNotIdle,
                    EarlyCommandTimingBuilder::new(Instant::now()),
                )
                .await
        }));

        // Both second and third commands should fail
        let fut2_result = fut2.await?;
        let fut2_error: buck2_error::Error = fut2_result.unwrap_err();
        assert!(
            fut2_error
                .tags()
                .contains(&buck2_error::ErrorTag::DaemonIsBusy)
        );

        let fut3_result = fut3.await?;
        let fut3_error: buck2_error::Error = fut3_result.unwrap_err();
        assert!(
            fut3_error
                .tags()
                .contains(&buck2_error::ErrorTag::DaemonIsBusy)
        );

        // Clean up first command
        drop(blocked1);
        fut1.await??;

        Ok(())
    }

    #[tokio::test]
    async fn test_exit_when_not_idle_with_preemptible_command() -> buck2_error::Result<()> {
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        let block1 = Arc::new(RwLock::new(()));
        let blocked1 = block1.write().await;

        let barrier = Arc::new(Barrier::new(2));

        // Start first command with --preemptible=always (could be preempted)
        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();
            let b = block1.dupe();

            async move {
                concurrency
                    .enter(
                        TestEvents::with_trace(traces1),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Always, // This command is preemptible
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
                    )
                    .await
            }
        });

        barrier.wait().await;

        // Start second command with --exit-when=notidle (should fail)
        // Even though the first command is preemptible, this should still fail
        // because --exit-when=notidle means "only run if daemon is completely idle"
        let fut2 = tokio::spawn(buck2_util::async_move_clone!(concurrency, {
            concurrency
                .enter(
                    TestEvents::with_trace(traces2),
                    &NoChanges,
                    |_, _timing| async move {
                        // Should never reach here
                        panic!("Command should have failed before execution");
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never,
                    &NoTelemetry,
                    ExitWhen::ExitNotIdle,
                    EarlyCommandTimingBuilder::new(Instant::now()),
                )
                .await
        }));

        // Second command should fail immediately, even though first is preemptible
        let fut2_result = fut2.await?;
        let fut2_error: buck2_error::Error = fut2_result.unwrap_err();
        assert!(
            fut2_error
                .tags()
                .contains(&buck2_error::ErrorTag::DaemonIsBusy),
            "Expected DaemonIsBusy error tag, even though previous command is preemptible"
        );

        // The first command should still be running (not preempted)
        // because --exit-when=notidle doesn't preempt, it just fails
        assert!(
            block1.try_write().is_err(),
            "First command should still be running"
        );

        // Clean up first command
        drop(blocked1);
        fut1.await??;

        Ok(())
    }

    #[tokio::test]
    async fn test_exit_when_not_idle_gets_preempted() -> buck2_error::Result<()> {
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        let block1 = Arc::new(RwLock::new(()));
        let blocked1 = block1.write().await;

        let barrier = Arc::new(Barrier::new(2));
        let preempted = Arc::new(AtomicBool::new(false));

        // Start first command with --exit-when=notidle
        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();
            let b = block1.dupe();
            let preempted = preempted.dupe();

            async move {
                let result = concurrency
                    .enter(
                        TestEvents::with_trace(traces1),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            // This should never complete because we'll be preempted
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Always,
                        &NoTelemetry,
                        ExitWhen::ExitNotIdle,
                        EarlyCommandTimingBuilder::new(Instant::now()),
                    )
                    .await;

                // Check if we got preempted
                if let Err(ref e) = result {
                    let error: buck2_error::Error = e.clone();
                    if error
                        .tags()
                        .contains(&buck2_error::ErrorTag::DaemonPreempted)
                    {
                        preempted.store(true, Ordering::Relaxed);
                    }
                }
                result
            }
        });

        barrier.wait().await;

        // Start second command (without any preemptible flag)
        // This should preempt the first command
        let fut2 = tokio::spawn(buck2_util::async_move_clone!(concurrency, {
            concurrency
                .enter(
                    TestEvents::with_trace(traces2),
                    &NoChanges,
                    |_, _timing| async move {
                        // Just a quick task
                        tokio::task::yield_now().await;
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never, // Not preemptible
                    &NoTelemetry,
                    ExitWhen::ExitNever,
                    EarlyCommandTimingBuilder::new(Instant::now()),
                )
                .await
        }));

        // Second command should succeed
        fut2.await??;

        // First command should have been preempted
        let fut1_result = fut1.await?;
        assert!(fut1_result.is_err(), "First command should have failed");
        assert!(
            preempted.load(Ordering::Relaxed),
            "First command should have been preempted"
        );

        // Clean up
        drop(blocked1);

        Ok(())
    }

    #[tokio::test]
    async fn test_multiple_exit_when_not_idle_commands_with_different_state()
    -> buck2_error::Result<()> {
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        let block1 = Arc::new(RwLock::new(()));
        let blocked1 = block1.write().await;

        let barrier = Arc::new(Barrier::new(2));

        // Start first command with --exit-when=notidle
        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();
            let b = block1.dupe();

            async move {
                concurrency
                    .enter(
                        TestEvents::with_trace(traces1),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNotIdle,
                        EarlyCommandTimingBuilder::new(Instant::now()),
                    )
                    .await
            }
        });

        barrier.wait().await;

        // Start second and third commands with --exit-when=notidle (should both fail)
        let fut2 = tokio::spawn(buck2_util::async_move_clone!(concurrency, {
            concurrency
                .enter(
                    TestEvents::with_trace(traces2),
                    &CtxDifferent,
                    |_, _timing| async move {
                        // Just a quick task
                        tokio::task::yield_now().await;
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never,
                    &NoTelemetry,
                    ExitWhen::ExitNotIdle,
                    EarlyCommandTimingBuilder::new(Instant::now()),
                )
                .await
        }));

        // Both second and third commands should fail
        let fut2_result = fut2.await?;
        let fut2_error: buck2_error::Error = fut2_result.unwrap_err();
        assert!(
            fut2_error
                .tags()
                .contains(&buck2_error::ErrorTag::DaemonIsBusy)
        );

        // Clean up first command
        drop(blocked1);
        fut1.await??;

        Ok(())
    }

    #[tokio::test]
    async fn test_exit_when_not_idle_allows_command_when_daemon_idle_with_same_state()
    -> buck2_error::Result<()> {
        // This test verifies that when the daemon is idle (no command is currently running),
        // a command with --exit-when=notidle should succeed if it has the same state as the
        // previous command that has finished.
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        // First command runs to completion
        concurrency
            .enter(
                TestEvents::with_trace(traces1),
                &NoChanges,
                |_, _timing| async move {
                    // Quick task that finishes
                    tokio::task::yield_now().await;
                },
                false,
                Vec::new(),
                None,
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        wait_for_commands_to_be_reaped(&concurrency).await?;

        // Daemon should now be idle
        // Second command with --exit-when=notidle and same state should succeed
        let result = concurrency
            .enter(
                TestEvents::with_trace(traces2),
                &NoChanges, // Same state as first command
                |_, _timing| async move {
                    // Quick task
                    tokio::task::yield_now().await;
                    "success"
                },
                false,
                Vec::new(),
                None,
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNotIdle,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await;

        // Should succeed since daemon is idle
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "success");

        Ok(())
    }

    #[tokio::test]
    async fn test_exit_when_not_idle_allows_command_when_daemon_idle_with_different_state()
    -> buck2_error::Result<()> {
        // This test verifies that when the daemon is idle (no command is currently running),
        // a command with --exit-when=notidle should succeed even if it has a different state
        // than previous commands.
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        // First command runs to completion with NoChanges state
        concurrency
            .enter(
                TestEvents::with_trace(traces1),
                &NoChanges,
                |_, _timing| async move {
                    // Quick task that finishes
                    tokio::task::yield_now().await;
                },
                false,
                Vec::new(),
                None,
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        wait_for_commands_to_be_reaped(&concurrency).await?;

        // Daemon should now be idle
        // Second command with --exit-when=notidle and different state should succeed
        let result = concurrency
            .enter(
                TestEvents::with_trace(traces2),
                &CtxDifferent, // Different state than first command
                |_, _timing| async move {
                    // Quick task
                    tokio::task::yield_now().await;
                    "success"
                },
                false,
                Vec::new(),
                None,
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNotIdle,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await;

        // Should succeed since daemon is idle, regardless of state difference
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "success");

        Ok(())
    }

    fn get_early_command_timing_duration(
        timing: EarlyCommandTimingBuilder,
        key: &str,
    ) -> Option<Duration> {
        let timing = timing.finish_early_command_timing();
        let mut end = timing.early_command_end;
        let mut duration = None;
        for s in timing.early_spans.iter().rev() {
            if s.1 == key {
                let d = end - s.0;
                if let Some(s) = &mut duration {
                    *s += d;
                } else {
                    duration = Some(d)
                }
            }
            end = s.0;
        }
        duration
    }

    fn get_exclusive_command_wait_duration(timing: EarlyCommandTimingBuilder) -> Option<Duration> {
        get_early_command_timing_duration(timing, EXCLUSIVE_COMMAND_WAIT)
    }

    #[tokio::test]
    async fn test_enter_duration_parameter_populated() -> buck2_error::Result<()> {
        // Test that the duration parameter passed to the enter() callback is properly populated
        // when waiting for an exclusive command lock.
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        let block1 = Arc::new(RwLock::new(()));
        let blocked1 = block1.write().await;

        let barrier = Arc::new(Barrier::new(2));
        let duration_captured: Arc<Mutex<Duration>> = Arc::new(Mutex::new(Duration::ZERO));

        // Start first exclusive command
        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();
            let b = block1.dupe();

            async move {
                concurrency
                    .enter(
                        TestEvents::with_trace(traces1),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = b.read().await;
                        },
                        false,
                        Vec::new(),
                        Some("exclusive_test".to_owned()),
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
                    )
                    .await
            }
        });

        barrier.wait().await;

        // Start second exclusive command - it should wait and capture non-zero duration
        let fut2 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let duration_captured = duration_captured.dupe();

            async move {
                concurrency
                    .enter(
                        TestEvents::with_trace(traces2),
                        &NoChanges,
                        |_, timing| {
                            *duration_captured.lock() =
                                get_exclusive_command_wait_duration(timing).unwrap();
                            async move {
                                tokio::task::yield_now().await;
                            }
                        },
                        false,
                        Vec::new(),
                        Some("exclusive_test_2".to_owned()),
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        &NoTelemetry,
                        ExitWhen::ExitNever,
                        EarlyCommandTimingBuilder::new(Instant::now()),
                    )
                    .await
            }
        });

        // Give fut2 time to start waiting
        tokio::time::sleep(Duration::from_millis(50)).await;

        // Unblock the first command
        drop(blocked1);
        fut1.await??;

        // Complete the second command
        fut2.await??;

        // Verify that the duration was captured and is non-zero
        let duration = *duration_captured.lock();
        assert!(
            !duration.is_zero(),
            "Duration should be non-zero since we waited for exclusive lock. Got: {:?}",
            duration
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_enter_duration_parameter_zero_for_non_exclusive() -> buck2_error::Result<()> {
        // Test that the duration parameter is zero when no exclusive command lock is needed.
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces = TraceId::new();
        let duration_captured: Arc<Mutex<Duration>> = Arc::new(Mutex::new(Duration::ZERO));

        // Run a non-exclusive command (None for exclusive_cmd parameter)
        concurrency
            .enter(
                TestEvents::with_trace(traces),
                &NoChanges,
                |_, timing| {
                    *duration_captured.lock() =
                        get_exclusive_command_wait_duration(timing).unwrap_or(Duration::ZERO);
                    async move {
                        tokio::task::yield_now().await;
                    }
                },
                false,
                Vec::new(),
                None, // No exclusive command
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        // Verify that the duration was captured and is zero
        let duration = *duration_captured.lock();
        assert!(
            duration.is_zero(),
            "Duration should be zero for non-exclusive commands. Got: {:?}",
            duration
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_file_watcher_sync_duration_captured() -> buck2_error::Result<()> {
        // Test that file_watcher_sync_duration is properly captured when the updater
        // returns a non-zero duration.
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        struct UpdaterWithDelay;
        #[async_trait]
        impl DiceUpdater for UpdaterWithDelay {
            async fn update(
                &self,
                ctx: DiceTransactionUpdater,
                early_timings: &mut EarlyCommandTimingBuilder,
            ) -> buck2_error::Result<(DiceTransactionUpdater, UserComputationData)> {
                // Simulate file watcher sync taking 50ms
                early_timings.start_span(FILE_WATCHER_WAIT.to_owned());
                tokio::time::sleep(Duration::from_millis(50)).await;
                early_timings.end_known_span();
                Ok((ctx, Default::default()))
            }
        }

        let traces = TraceId::new();
        let file_watcher_duration_captured: Arc<Mutex<Duration>> =
            Arc::new(Mutex::new(Duration::ZERO));

        concurrency
            .enter(
                TestEvents::with_trace(traces),
                &UpdaterWithDelay,
                |_, timing| {
                    let duration_captured = file_watcher_duration_captured.dupe();
                    async move {
                        // Capture the file watcher sync duration (sum of all syncs)
                        let total_duration: std::time::Duration =
                            get_early_command_timing_duration(timing, FILE_WATCHER_WAIT).unwrap();
                        *duration_captured.lock() = total_duration;
                        tokio::task::yield_now().await;
                    }
                },
                false,
                Vec::new(),
                None,
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        // Verify that the file watcher sync duration was captured
        let duration = *file_watcher_duration_captured.lock();
        assert!(
            !duration.is_zero(),
            "File watcher sync duration should be non-zero. Got: {:?}",
            duration
        );
        assert!(
            duration >= Duration::from_millis(50),
            "File watcher sync duration should be at least 50ms. Got: {:?}",
            duration
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_file_watcher_sync_duration_accumulated_across_loop_iterations()
    -> buck2_error::Result<()> {
        // Test that file_watcher_sync_duration is accumulated across multiple loop iterations
        // when the dice state transitions through cleanup.
        let dice = make_default_dice();
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        // First, establish an active DICE state by running a command
        let traces_init = TraceId::new();
        concurrency
            .enter(
                TestEvents::with_trace(traces_init),
                &NoChanges,
                |_, _timing| async move {
                    // Just establish the initial state
                    tokio::task::yield_now().await;
                },
                false,
                Vec::new(),
                None,
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        // Now run the test that changes state, which should trigger a cleanup and re-update
        struct UpdaterWithDelayAndStateChange {
            call_count: AtomicBool,
        }

        #[async_trait]
        impl DiceUpdater for UpdaterWithDelayAndStateChange {
            async fn update(
                &self,
                mut ctx: DiceTransactionUpdater,
                early_timings: &mut EarlyCommandTimingBuilder,
            ) -> buck2_error::Result<(DiceTransactionUpdater, UserComputationData)> {
                // First call changes state, second call doesn't
                let is_first = !self.call_count.swap(true, Ordering::Relaxed);
                if is_first {
                    ctx.changed_to(vec![(K, ())])?;
                }
                // Each call simulates 30ms of file watcher sync
                early_timings.start_span(FILE_WATCHER_WAIT.to_owned());
                tokio::time::sleep(Duration::from_millis(30)).await;
                early_timings.end_known_span();
                Ok((ctx, Default::default()))
            }
        }

        let traces = TraceId::new();
        let file_watcher_duration_captured: Arc<Mutex<Duration>> =
            Arc::new(Mutex::new(Duration::ZERO));

        let updater = UpdaterWithDelayAndStateChange {
            call_count: AtomicBool::new(false),
        };

        concurrency
            .enter(
                TestEvents::with_trace(traces),
                &updater,
                |_, timing| {
                    *file_watcher_duration_captured.lock() =
                        get_early_command_timing_duration(timing, FILE_WATCHER_WAIT).unwrap();
                    async move {
                        tokio::task::yield_now().await;
                    }
                },
                false,
                Vec::new(),
                None,
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                &NoTelemetry,
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        // Verify that the file watcher sync duration was accumulated
        // It should be at least the sum of both iterations (60ms total)
        let duration = *file_watcher_duration_captured.lock();
        assert!(
            duration >= Duration::from_millis(60),
            "File watcher sync duration should be accumulated across loop iterations. Expected at least 60ms, got: {:?}",
            duration
        );

        Ok(())
    }
}
