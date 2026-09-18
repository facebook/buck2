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
use std::fmt;
use std::fmt::Debug;
use std::sync::Arc;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::time::Duration;

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
use tokio::sync::Semaphore;
use tokio::sync::oneshot;
use tokio::sync::oneshot::error::RecvError;
use tokio::time::timeout;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum ConcurrencyHandlerError {
    #[error(
        "Recursive invocation of Buck, which is discouraged, but will probably work (using the same state). Trace Ids: {0}. Recursive invocation command: `{1}`"
    )]
    NestedInvocationWithSameStates(ConcurrentTraces, String),
    #[error(
        "Recursive invocation of Buck, with a different state. Use `--isolation-dir` on the inner invocation to fix this. Trace Ids: {0}. Recursive invocation command: `{1}`"
    )]
    #[buck2(input)]
    NestedInvocationWithDifferentStates(ConcurrentTraces, String),
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
    /// Source of `CommandId`s. Deliberately outside `data` so that a command has an identity
    /// before it competes for the lock.
    next_command_id: AtomicUsize,
    /// Commands waiting for admission. The separate mutex allows synchronous `Drop` cleanup.
    queued_commands: Arc<parking_lot::Mutex<SmallMap<CommandId, TraceId>>>,
    /// Serializes updates independently of the state lock.
    #[allocative(skip)]
    update_permit: Semaphore,
}

#[derive(Allocative)]
struct ConcurrencyHandlerData {
    /// the currently active `Dice` being used. Commands can only run concurrently if these are
    /// "equivalent".
    dice_status: DiceStatus,
    /// A list of the currently running commands.
    active_commands: SmallMap<CommandId, CommandData>,
    /// The epoch of the last ActiveDice we assigned.
    cleanup_epoch: usize,
    /// Whether this has been tainted previously.
    previously_tainted: bool,
}

/// Identifies one entry in `active_commands`. `TraceId` cannot serve this purpose because it is not
/// unique across concurrently live entries.
///
/// Values are distinct and increasing, but not contiguous: every error path between allocation and
/// registration burns one.
#[derive(Allocative, Display, Copy, Clone, Dupe, PartialEq, Eq, Hash)]
struct CommandId(usize);

#[derive(Allocative)]
struct CommandData {
    trace_id: TraceId,
    display_command: String,
    preemption_setting: PreemptibleWhen,
    #[allocative(skip)]
    preempt: Option<oneshot::Sender<()>>,
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

#[derive(Allocative, Debug)]
struct ActiveDice {
    version: DiceEquality,
}

/// Hand-written only to elide the cleanup future. Deriving would print
/// `Shared { inner: .., waker_key: .. }`, which is noise in every log line and assertion failure.
impl fmt::Debug for DiceStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiceStatus::Available { active } => {
                f.debug_struct("Available").field("active", active).finish()
            }
            DiceStatus::Cleanup { epoch, .. } => f
                .debug_struct("Cleanup")
                .field("epoch", epoch)
                .finish_non_exhaustive(),
        }
    }
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

/// Event interface used while admitting a command.
pub trait CommandEvents: Dupe + Send + Sync + 'static {
    fn instant(&self, data: buck2_data::instant_event::Data);
    fn trace_id(&self) -> &TraceId;
    /// Show a warning on the command's console.
    fn console_warning(&self, message: String);
    /// `span` must preserve span-entering semantics: spans created inside `fut` parent to this span,
    /// and poll time accumulates into the end event. Delegating to an implementation built on
    /// `EventDispatcher::span_async` does this; starting a span, awaiting, then ending it does not.
    fn span<'a, R: Send + 'a>(
        &self,
        start: buck2_data::span_start_event::Data,
        fut: BoxFuture<'a, (R, buck2_data::span_end_event::Data)>,
    ) -> BoxFuture<'a, R>;
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
/// the command may run.
///
/// The command is registered before this is invoked, and the concurrency lock is not held. Calls
/// for DICE-equivalent transactions may overlap and have no ordering guarantee, so implementations
/// must synchronize their own shared state. A different-state transaction is not admitted until
/// registered commands finish. Returning `Err` deregisters the command and wakes waiters.
#[async_trait]
pub trait CommandTransactionObserver: Send + Sync {
    async fn on_transaction_committed(
        &self,
        transaction: &DiceTransaction,
    ) -> buck2_error::Result<()>;
}

/// Exclusive commands that hold the lock or are queued for it, oldest first.
///
/// Entries are keyed by ticket rather than positional, because a command can leave the queue in
/// an order other than it joined: cancellation removes an entry that never reached the front.
type ExclusiveWaiters = Arc<parking_lot::Mutex<VecDeque<(u64, String)>>>;

#[derive(Allocative)]
struct ExclusiveCommandLock {
    lock: tokio::sync::RwLock<()>,
    waiters: ExclusiveWaiters,
    next_ticket: AtomicU64,
}

/// Removes its entry from the queue on drop, whether the command acquired the lock or was
/// cancelled while waiting for it.
struct ExclusiveWaiter {
    waiters: ExclusiveWaiters,
    ticket: u64,
}

impl Drop for ExclusiveWaiter {
    fn drop(&mut self) {
        self.waiters
            .lock()
            .retain(|(ticket, _)| *ticket != self.ticket);
    }
}

#[allow(dead_code)] // lock guards are held, not read
enum ExclusiveCommandLockGuard<'a> {
    Shared(tokio::sync::RwLockReadGuard<'a, ()>),
    /// The waiter is first so that it drops first: the name leaves the queue before the write
    /// lock is released, so `owning_command` never names a command that has already finished.
    Exclusive(ExclusiveWaiter, tokio::sync::RwLockWriteGuard<'a, ()>),
    /// A nested invocation, which does not take the gate at all. See `enter`.
    Bypassed,
}

impl ExclusiveCommandLock {
    pub fn new() -> Self {
        ExclusiveCommandLock {
            lock: tokio::sync::RwLock::new(()),
            waiters: Arc::new(parking_lot::Mutex::new(VecDeque::new())),
            next_ticket: AtomicU64::new(0),
        }
    }

    pub async fn exclusive_lock<'a>(&'a self, cmd_name: String) -> ExclusiveCommandLockGuard<'a> {
        // Joined before awaiting the lock, so a command that is queued but not yet holding can
        // still be named as the thing others are waiting on. `waiter` owns the removal, so
        // dropping this future before the lock is granted takes the entry with it.
        let waiter = ExclusiveWaiter {
            waiters: self.waiters.dupe(),
            ticket: self.next_ticket.fetch_add(1, Ordering::Relaxed),
        };
        self.waiters.lock().push_back((waiter.ticket, cmd_name));

        let guard = self.lock.write().await;
        ExclusiveCommandLockGuard::Exclusive(waiter, guard)
    }

    pub async fn shared_lock<'a>(&'a self) -> ExclusiveCommandLockGuard<'a> {
        ExclusiveCommandLockGuard::Shared(self.lock.read().await)
    }

    /// The exclusive command holding the lock, or the oldest one queued for it. `None` when no
    /// exclusive command is in play.
    pub fn owning_command(&self) -> Option<String> {
        self.waiters.lock().front().map(|(_, name)| name.clone())
    }
}

impl ConcurrencyHandler {
    pub fn new(dice: Arc<Dice>) -> Arc<Self> {
        Arc::new(ConcurrencyHandler {
            data: Mutex::new(ConcurrencyHandlerData {
                dice_status: DiceStatus::idle(),
                active_commands: SmallMap::new(),
                cleanup_epoch: 0,
                previously_tainted: false,
            }),
            cond: Condvar::new(),
            dice,
            exclusive_command_lock: ExclusiveCommandLock::new(),
            next_command_id: AtomicUsize::new(0),
            queued_commands: Arc::new(parking_lot::Mutex::new(SmallMap::new())),
            update_permit: Semaphore::new(1),
        })
    }

    /// Allocates the next `CommandId`. Returns a distinct value to every caller.
    fn allocate_command_id(&self) -> CommandId {
        CommandId(self.next_command_id.fetch_add(1, Ordering::Relaxed))
    }

    /// Records a command as queued until the returned token is dropped.
    fn mark_queued(&self, command: CommandId, trace_id: TraceId) -> QueuedCommand {
        self.queued_commands.lock().insert(command, trace_id);
        QueuedCommand {
            queued: self.queued_commands.dupe(),
            command,
        }
    }

    /// Queued commands other than the command receiving the message.
    fn queued_traces(&self, asking: CommandId) -> QueuedTraces {
        QueuedTraces(ConcurrentTraces(
            self.queued_commands
                .lock()
                .iter()
                .filter(|(queued, _)| **queued != asking)
                .map(|(_, trace)| trace.dupe())
                .collect(),
        ))
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
                        } else if is_nested_invocation {
                            // A writer-preferring lock can deadlock a nested invocation behind a
                            // writer waiting for its parent. Once a writer holds the gate, no parent
                            // remains from which a nested invocation can originate.
                            ExclusiveCommandLockGuard::Bypassed
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

    // The async condvar releases the state mutex while commands wait. `Dice::is_idle` below is the
    // remaining await that holds it.
    /// How long a command may block before its user is first told what it is queued
    /// behind. Short, so that a wedged blocking command is identifiable quickly.
    const BLOCKED_COMMAND_FIRST_WARNING: Duration = Duration::from_secs(60);

    /// How often to repeat the warning after the first one. Longer, so a legitimately
    /// slow blocking command doesn't flood the console.
    const BLOCKED_COMMAND_WARNING_INTERVAL: Duration = Duration::from_secs(5 * 60);

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

        let command_id = self.allocate_command_id();

        let (preempt_sender, preempt_receiver) = oneshot::channel::<()>();

        let display_command = format_command(&sanitized_argv);
        let command_data = CommandData {
            trace_id: trace.dupe(),
            display_command,
            preemption_setting: preemptible,
            preempt: Some(preempt_sender),
        };

        // Keep the command visible across cleanup and update retries, not only condvar waits.
        let queued = self.mark_queued(command_id, command_data.trace_id.dupe());

        let mut data = self.data.lock().await;

        let (transaction, tainted, nested_warning, no_active_dice_state) = loop {
            if let DiceStatus::Cleanup { future, epoch } = &data.dice_status {
                tracing::debug!("ActiveDice is in cleanup");
                let future = future.clone();
                let epoch = *epoch;

                // block while dice cleans up
                drop(data);
                events
                    .span(
                        buck2_data::DiceCleanupStart { epoch: epoch as _ }.into(),
                        Box::pin(
                            async move { (future.await, buck2_data::DiceCleanupEnd {}.into()) },
                        ),
                    )
                    .await;
                data = self.data.lock().await;

                data.transition_to_idle(epoch);
                continue;
            }

            tracing::debug!("ActiveDice is available");

            // `--exit-when=notidle` asks only whether anything else is running, so it is answered
            // here rather than after the update. Refusing costs a lock acquisition instead of a
            // file-watcher sync and a DICE commit.
            if matches!(exit_when, ExitWhen::ExitNotIdle) && !data.active_commands.is_empty() {
                let running = ConcurrentTraces::running(&data.active_commands);
                drop(data);
                let queued = self.queued_traces(command_id);
                return Err(ConcurrencyHandlerError::ExitOnDaemonNotIdle).with_buck_error_context(
                    || format!("Buck daemon is busy processing another command: {running}{queued}"),
                );
            }

            // `--exit-when=different-state` cannot be answered until the update has run, because
            // it depends on whether this command's state differs — which is *defined* as whether
            // injecting it altered the graph. Answering it against the state as it is afterwards
            // would make the flag timing-dependent, since the conflicting commands may finish
            // while this one syncs. So the inputs are captured at the top of each attempt, before
            // the update, and the question is settled against them.
            //
            // `None` when there is nothing to conflict with: either no active DICE version, or no
            // other command running, in which case the flag has nothing to fire on.
            let conflict_on_arrival = match &data.dice_status {
                DiceStatus::Available {
                    active: Some(active),
                } if !data.active_commands.is_empty() => Some(active.version),
                _ => None,
            };

            // Sampled before the update, and while the lock is still held, because committing a
            // transaction makes DICE non-idle: read afterwards this would be `false` almost always,
            // and every command would report itself as tainted.
            let dice_was_idle = self.dice.is_idle().await;

            // we rerun the updates in case that files on disk have changed between commands.
            // this might cause some churn, but concurrent commands don't happen much and
            // isn't a big perf bottleneck. Dice should be able to resurrect nodes properly.
            //
            // This runs under `update_permit` and *not* the state lock, so other commands can
            // reach a decision while this one is talking to the file watcher.
            drop(data);
            let transaction = async {
                let _update_permit = self
                    .update_permit
                    .acquire()
                    .await
                    .expect("`update_permit` is never closed");

                let updater = self.dice.updater();

                let (transaction, user_data) = updates.update(updater, early_timings).await?;

                let transaction = events
                    .span(
                        buck2_data::DiceStateUpdateStart {}.into(),
                        Box::pin(async {
                            (
                                async {
                                    let transaction = transaction.commit_with_data(user_data).await;
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
            data = self.data.lock().await;

            // Settled against the arrival snapshot, not the fresh read, so that the answer does
            // not depend on how long the update took. `!is_nested_invocation` because a nested
            // invocation with a differing state is reported as
            // `NestedInvocationWithDifferentStates` below rather than reaching the blocking path
            // this flag short-circuits.
            let refuse_on_different_state = matches!(exit_when, ExitWhen::ExitDifferentState)
                && !is_nested_invocation
                && conflict_on_arrival.is_some_and(|version| !transaction.equivalent(&version));

            if refuse_on_different_state {
                let running = ConcurrentTraces::running(&data.active_commands);
                drop(data);
                let queued = self.queued_traces(command_id);
                return Err(ConcurrencyHandlerError::ExitWhenDifferentState)
                    .with_buck_error_context(|| {
                        format!("Buck daemon is busy processing another command: {running}{queued}")
                    });
            }

            // The status can have moved while the update ran, so the decision is taken against a
            // fresh read rather than the one that selected this branch.
            let is_same_state = match &data.dice_status {
                DiceStatus::Cleanup { .. } => {
                    // Dropping the transaction releases its `ActiveTransactionGuard`. The retry
                    // awaits the cleanup future, which cannot complete while that guard is alive,
                    // so this drop is required for progress and not just tidiness.
                    drop(transaction);
                    continue;
                }
                DiceStatus::Available {
                    active: Some(active),
                } => Some(transaction.equivalent(&active.version)),
                DiceStatus::Available { active: None } => None,
            };

            let Some(is_same_state) = is_same_state else {
                tracing::debug!("ActiveDice has no active_transaction");
                data.dice_status = DiceStatus::active(transaction.equality_token());
                break (transaction, !dice_was_idle, None, true);
            };

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
                    let running =
                        ConcurrentTraces::running_and(&data.active_commands, &command_data);
                    let display_command = command_data.display_command.clone();
                    drop(data);
                    return Err(
                        ConcurrencyHandlerError::NestedInvocationWithDifferentStates(
                            running,
                            display_command,
                        )
                        .into(),
                    );
                }
                BypassSemaphore::Run(state) => {
                    let nested_warning = Self::nested_same_state_warning(
                        state,
                        &data.active_commands,
                        &command_data,
                    );
                    self.cancel_preemptible_commands(&mut data, is_same_state);
                    break (transaction, false, nested_warning, false);
                }
                BypassSemaphore::Block => {
                    let early_exit_error: Option<ConcurrencyHandlerError> =
                        if matches!(exit_when, ExitWhen::ExitDifferentState) {
                            Some(ConcurrencyHandlerError::ExitWhenDifferentState)
                        } else {
                            None
                        };
                    if let Some(early_exit_error) = early_exit_error {
                        let running = ConcurrentTraces::running(&data.active_commands);
                        drop(data);
                        let queued = self.queued_traces(command_id);
                        return Err(early_exit_error).with_buck_error_context(|| {
                            format!(
                                "Buck daemon is busy processing another command: {running}{queued}"
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
                    let display_command = active_command.display_command.clone();

                    data = events
                        .span(
                            DiceBlockConcurrentCommandStart {
                                current_active_trace_id: trace_id.to_string(),
                                cmd_args: display_command.clone(),
                            }
                            .into(),
                            Box::pin(async {
                                // This wait can last arbitrarily long (and forever if
                                // the blocking command is wedged, e.g. on stale Eden
                                // handles), so periodically tell the user what they
                                // are actually waiting on.
                                let wait = self.cond.wait((data, &self.data));
                                pin_mut!(wait);
                                let mut waited = Duration::ZERO;
                                let mut next_warning = Self::BLOCKED_COMMAND_FIRST_WARNING;
                                let data = loop {
                                    match timeout(next_warning, &mut wait).await {
                                        Ok(data) => break data,
                                        Err(_elapsed) => {
                                            waited += next_warning;
                                            next_warning =
                                                Self::BLOCKED_COMMAND_WARNING_INTERVAL;
                                            events.console_warning(format!(
                                                "This command has been waiting for {} for another command to finish: [{}] (trace ID: {}). \
                                                 If that command is not making progress, restarting the buck2 daemon with `buck2 kill` will unblock both",
                                                format_elapsed(waited),
                                                display_command,
                                                trace_id,
                                            ));
                                        }
                                    }
                                };
                                (
                                    data,
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
        };

        tracing::info!("Acquired access to DICE");

        let previously_tainted = data.previously_tainted;

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
            data.previously_tainted = true;
        }

        drop(queued);
        // Registration consumes the guard and releases the state lock. Its drop path also handles
        // observer failures.
        let drop_guard = OnExecExit::new(self.dupe(), command_id, command_data, data)?;

        if no_active_dice_state {
            events.instant(NoActiveDiceState {}.into());
        }

        if previously_tainted {
            events.instant(
                buck2_data::TagEvent {
                    tags: vec!["concurrency-previously-tainted".to_owned()],
                }
                .into(),
            );
        }

        if tainted {
            events.instant(
                buck2_data::TagEvent {
                    tags: vec!["concurrency-tainted".to_owned()],
                }
                .into(),
            );
        }

        // `soft_error!` may perform a synchronous Scribe write, so report after registration has
        // released the state lock. The guard cleans up if the warning is escalated.
        if let Some((running, argv)) = nested_warning {
            soft_error!(
                "nested_invocation_same_dice_state",
                ConcurrencyHandlerError::NestedInvocationWithSameStates(running, argv).into(),
                error_on_oss: true
            )?;
        }

        // The observer may perform blocking config parsing, so run it after releasing the lock.
        transaction_observer
            .on_transaction_committed(&transaction)
            .await?;

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

    /// Captures a recursive same-state warning for reporting after the lock is released.
    fn nested_same_state_warning(
        state: RunState,
        active_commands: &SmallMap<CommandId, CommandData>,
        current_command: &CommandData,
    ) -> Option<(ConcurrentTraces, String)> {
        match state {
            RunState::NestedSameState => Some((
                ConcurrentTraces::running_and(active_commands, current_command),
                current_command.display_command.clone(),
            )),
            RunState::ParallelSameState => None,
        }
    }
}

fn format_command(argv: &[String]) -> String {
    let mut iter = argv.iter();
    // Skip the executable path so the displayed command consistently starts with `buck2`.
    iter.next();

    truncate(&format!("buck2 {}", iter.join(" ")), 500)
}

/// Formats an elapsed wait in whole minutes, or seconds while under a minute, so the
/// message stays meaningful for any `BLOCKED_COMMAND_WARNING_INTERVAL`.
fn format_elapsed(elapsed: Duration) -> String {
    if elapsed.as_secs() < 60 {
        format!("{}s", elapsed.as_secs())
    } else {
        format!("{}m", elapsed.as_secs() / 60)
    }
}

/// Trace IDs captured under the state lock and formatted after it is released.
#[derive(Debug)]
struct ConcurrentTraces(Vec<TraceId>);

impl ConcurrentTraces {
    fn running_and(
        active_commands: &SmallMap<CommandId, CommandData>,
        current: &CommandData,
    ) -> Self {
        Self(
            active_commands
                .values()
                .chain(std::iter::once(current))
                .map(|cmd| cmd.trace_id.dupe())
                .collect(),
        )
    }

    fn running(active_commands: &SmallMap<CommandId, CommandData>) -> Self {
        Self(
            active_commands
                .values()
                .map(|cmd| cmd.trace_id.dupe())
                .collect(),
        )
    }
}

/// Deregisters a queued command, whether it was admitted or gave up waiting.
struct QueuedCommand {
    queued: Arc<parking_lot::Mutex<SmallMap<CommandId, TraceId>>>,
    command: CommandId,
}

impl Drop for QueuedCommand {
    fn drop(&mut self) {
        self.queued.lock().shift_remove(&self.command);
    }
}

/// A trailing clause that renders nothing when the queue is empty.
struct QueuedTraces(ConcurrentTraces);

impl fmt::Display for QueuedTraces {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.0.is_empty() {
            return Ok(());
        }
        write!(f, ". Queued behind: {}", self.0)
    }
}

impl fmt::Display for ConcurrentTraces {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // `TraceId` is not unique across concurrently live entries — a command with a null
        // dispatcher reports `TraceId::null()` — so the same id can appear twice.
        write!(
            f,
            "{}",
            self.0.iter().collect::<SmallSet<_>>().iter().join(", ")
        )
    }
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
        // Checked before inserting. Inserting first would evict the command already registered
        // under this id — including its preempt channel, the daemon's only way to interrupt it —
        // and the error would then be reported against state that had already been destroyed.
        if guard.active_commands.contains_key(&command) {
            return Err(internal_error!(
                "command id `{command}` is already registered"
            ));
        }
        guard.active_commands.insert(command, data);
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

    impl CommandEvents for TestEvents {
        fn instant(&self, data: buck2_data::instant_event::Data) {
            self.0.recorded.lock().push(RecordedEvent::Instant(data));
        }

        fn trace_id(&self) -> &TraceId {
            &self.0.trace_id
        }

        fn console_warning(&self, _message: String) {
            // User-facing chrome only; no test asserts on it.
        }

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

    struct FailingUpdater;

    #[async_trait]
    impl DiceUpdater for FailingUpdater {
        async fn update(
            &self,
            _ctx: DiceTransactionUpdater,
            _early_timings: &mut EarlyCommandTimingBuilder,
        ) -> buck2_error::Result<(DiceTransactionUpdater, UserComputationData)> {
            Err(internal_error!("updater failed"))
        }
    }

    struct FailingObserver;

    #[async_trait]
    impl CommandTransactionObserver for FailingObserver {
        async fn on_transaction_committed(
            &self,
            _transaction: &DiceTransaction,
        ) -> buck2_error::Result<()> {
            Err(internal_error!("observer failed"))
        }
    }

    /// Signals entry into `on_transaction_committed`, then blocks until released.
    struct BlockingObserver {
        entered: Arc<Barrier>,
        release: Arc<Barrier>,
        fail: bool,
    }

    #[async_trait]
    impl CommandTransactionObserver for BlockingObserver {
        async fn on_transaction_committed(
            &self,
            _transaction: &DiceTransaction,
        ) -> buck2_error::Result<()> {
            self.entered.wait().await;
            self.release.wait().await;
            if self.fail {
                return Err(internal_error!("observer failed"));
            }
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

    /// Distinctness of `CommandId` used to be a consequence of the read-modify-write happening
    /// under the state lock. It is now the atomic's job, so it is worth pinning directly: a
    /// collision otherwise surfaces far from its cause, as the duplicate-registration
    /// `internal_error!` in `OnExecExit::new`.
    ///
    /// The multi-threaded flavour is load-bearing: on the default current-thread runtime the tasks
    /// never overlap, and a non-atomic read-modify-write passes.
    #[tokio::test(flavor = "multi_thread", worker_threads = 8)]
    async fn concurrently_allocated_command_ids_are_distinct() {
        const TASKS: usize = 8;
        const PER_TASK: usize = 1024;

        let concurrency = ConcurrencyHandler::new(make_default_dice());
        let start = Arc::new(tokio::sync::Barrier::new(TASKS));

        let handles: Vec<_> = (0..TASKS)
            .map(|_| {
                let concurrency = concurrency.dupe();
                let start = start.dupe();
                tokio::spawn(async move {
                    start.wait().await;
                    (0..PER_TASK)
                        .map(|_| concurrency.allocate_command_id())
                        .collect::<Vec<_>>()
                })
            })
            .collect();

        let mut seen = SmallSet::new();
        for handle in handles {
            for id in handle.await.unwrap() {
                assert!(seen.insert(id), "duplicate CommandId `{id}`");
            }
        }

        assert_eq!(seen.len(), TASKS * PER_TASK);
    }

    #[tokio::test]
    async fn a_queued_command_is_named_only_while_it_waits() {
        let concurrency = ConcurrencyHandler::new(make_default_dice());
        let asking = concurrency.allocate_command_id();
        assert_eq!(concurrency.queued_traces(asking).to_string(), "");

        let trace = TraceId::new();
        let command_id = concurrency.allocate_command_id();
        {
            let _queued = concurrency.mark_queued(command_id, trace.dupe());
            assert_eq!(
                concurrency.queued_traces(asking).to_string(),
                format!(". Queued behind: {trace}")
            );

            // The command being told is queued too, and must not be named to itself.
            assert_eq!(concurrency.queued_traces(command_id).to_string(), "");
        }

        // Dropping the token is the only deregistration, so it has to cover both leaving the
        // blocking path normally and being cancelled inside it.
        assert_eq!(concurrency.queued_traces(asking).to_string(), "");
    }

    /// Blocked commands are not in `active_commands` — nothing registers until after the wait —
    /// so before the queued registry a "daemon is busy" message could not mention them at all.
    #[tokio::test]
    async fn a_blocked_command_is_named_as_queued() -> buck2_error::Result<()> {
        let concurrency = ConcurrencyHandler::new(make_default_dice());

        let block = Arc::new(RwLock::new(()));
        let blocked = block.write().await;
        let barrier = Arc::new(Barrier::new(2));

        let running_trace = TraceId::new();
        let queued_trace = TraceId::new();

        let running = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier.dupe();
            let block = block.dupe();
            let trace = running_trace.dupe();
            async move {
                concurrency
                    .enter(
                        TestEvents::with_trace(trace),
                        &NoChanges,
                        |_, _timing| async move {
                            barrier.wait().await;
                            let _g = block.read().await;
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

        // A differing state, so this one blocks rather than joining.
        let queued = tokio::spawn({
            let concurrency = concurrency.dupe();
            let trace = queued_trace.dupe();
            async move {
                concurrency
                    .enter(
                        TestEvents::with_trace(trace),
                        &CtxDifferent,
                        |_, _timing| async move {},
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

        // Waiting for it to reach the blocking path, rather than merely to be spawned. `probe` is
        // never registered, so nothing is filtered out of the view it gets.
        let probe = concurrency.allocate_command_id();
        tokio::time::timeout(Duration::from_secs(10), async {
            while concurrency.queued_traces(probe).to_string().is_empty() {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("the second command never blocked");

        let refused = concurrency
            .enter(
                TestEvents::new(),
                &NoChanges,
                |_, _timing| async move {},
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
            .expect_err("the daemon is not idle");

        let message = format!("{refused:?}");
        assert!(
            message.contains(&running_trace.to_string()),
            "the running command should be named: {message}"
        );
        assert!(
            message.contains(&queued_trace.to_string()),
            "the queued command should be named: {message}"
        );

        drop(blocked);
        running.await??;
        queued.await??;

        Ok(())
    }

    /// A command cancelled while queued for the exclusive lock never acquires it, so it never
    /// produces a guard whose drop could remove it.
    #[tokio::test]
    async fn a_cancelled_exclusive_command_stops_being_named() {
        let lock = ExclusiveCommandLock::new();

        let held = lock.exclusive_lock("first".to_owned()).await;
        assert_eq!(lock.owning_command().as_deref(), Some("first"));

        {
            let mut queued = Box::pin(lock.exclusive_lock("second".to_owned()));
            assert!(poll!(&mut queued).is_pending(), "`first` still holds it");
            assert_eq!(
                lock.owning_command().as_deref(),
                Some("first"),
                "a queued command must not displace the holder"
            );
            // Dropping `queued` cancels `second` before it ever acquires.
        }

        drop(held);

        // Asserted via a third command rather than by expecting `None` here: the previous
        // implementation would also report `None` at this point, because it inferred emptiness
        // from `try_read()` succeeding rather than from the queue. Taking the lock again is what
        // exposes the abandoned entry.
        let third = lock.exclusive_lock("third".to_owned()).await;
        assert_eq!(
            lock.owning_command().as_deref(),
            Some("third"),
            "a cancelled command was still being reported as the owner"
        );

        drop(third);
        assert_eq!(lock.owning_command(), None);
    }

    /// The duplicate-id path is argued unreachable while `CommandId` is monotonic, so this pins
    /// what happens if that argument ever stops holding: a refusal that leaves the existing
    /// registration intact, rather than one that reports an error about state it just destroyed.
    #[tokio::test]
    async fn duplicate_registration_refuses_without_evicting_the_first() -> buck2_error::Result<()>
    {
        fn command_with(preempt: Option<oneshot::Sender<()>>) -> CommandData {
            CommandData {
                trace_id: TraceId::new(),
                display_command: "buck2".to_owned(),
                preemption_setting: PreemptibleWhen::Never,
                preempt,
            }
        }

        let concurrency = ConcurrencyHandler::new(make_default_dice());
        let command_id = concurrency.allocate_command_id();

        let (preempt_sender, _preempt_receiver) = oneshot::channel::<()>();
        let _first = OnExecExit::new(
            concurrency.dupe(),
            command_id,
            command_with(Some(preempt_sender)),
            concurrency.data.lock().await,
        )?;

        let duplicate = OnExecExit::new(
            concurrency.dupe(),
            command_id,
            command_with(None),
            concurrency.data.lock().await,
        );
        assert!(duplicate.is_err(), "a repeated id should be refused");

        let data = concurrency.data.lock().await;
        let registered = data
            .active_commands
            .get(&command_id)
            .expect("the first command was evicted by the refused one");
        assert!(
            registered.preempt.is_some(),
            "the first command's preempt channel was replaced"
        );

        Ok(())
    }

    /// Rendering moved out of the critical section into this `Display`, and nothing else covers
    /// it: no test asserts on the text of the messages it feeds.
    #[test]
    fn concurrent_traces_names_each_command_once() {
        let a = TraceId::new();
        let b = TraceId::new();

        assert_eq!(ConcurrentTraces(vec![]).to_string(), "");
        assert_eq!(ConcurrentTraces(vec![a.dupe()]).to_string(), a.to_string());
        assert_eq!(
            ConcurrentTraces(vec![a.dupe(), b.dupe()]).to_string(),
            format!("{a}, {b}")
        );

        // A command with a null dispatcher reports `TraceId::null()`, so the same id genuinely
        // reaches this type twice. Naming it once is the point of the dedup.
        assert_eq!(
            ConcurrentTraces(vec![a.dupe(), b.dupe(), a.dupe()]).to_string(),
            format!("{a}, {b}")
        );
    }

    /// The `Debug` impl is hand-written, so it needs its own check — in particular that it elides
    /// the cleanup future, which is the only reason it is not derived.
    #[tokio::test]
    async fn debug_reports_the_state_and_elides_the_cleanup_future() {
        let idle = format!("{:?}", DiceStatus::idle());
        assert!(idle.contains("Available"), "{idle}");
        assert!(idle.contains("None"), "{idle}");

        let dice = make_default_dice();
        let active = format!(
            "{:?}",
            DiceStatus::active(dice.updater().commit().await.equality_token())
        );
        assert!(active.contains("Available"), "{active}");
        assert!(active.contains("ActiveDice"), "{active}");

        let cleanup = format!(
            "{:?}",
            DiceStatus::Cleanup {
                future: futures::future::ready(()).boxed().shared(),
                epoch: 4,
            }
        );
        assert!(cleanup.contains("Cleanup"), "{cleanup}");
        assert!(cleanup.contains('4'), "epoch should be visible: {cleanup}");
        assert!(
            !cleanup.contains("waker_key"),
            "the cleanup future should be elided, not printed: {cleanup}"
        );
    }

    /// Builder for a test call to [`ConcurrencyHandler::enter`], which otherwise takes twelve
    /// arguments of which most tests care about one or two. Defaults describe a plain command:
    /// not nested, not exclusive, never preemptible, never exiting early.
    struct TestCommand {
        dispatcher: TestEvents,
        preemptible: PreemptibleWhen,
        exit_when: ExitWhen,
        is_nested_invocation: bool,
        exclusive_cmd: Option<String>,
    }

    impl TestCommand {
        fn new() -> Self {
            Self {
                dispatcher: TestEvents::new(),
                preemptible: PreemptibleWhen::Never,
                exit_when: ExitWhen::ExitNever,
                is_nested_invocation: false,
                exclusive_cmd: None,
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

        fn nested_invocation(mut self, is_nested_invocation: bool) -> Self {
            self.is_nested_invocation = is_nested_invocation;
            self
        }

        fn exclusive_cmd(mut self, cmd_name: &str) -> Self {
            self.exclusive_cmd = Some(cmd_name.to_owned());
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
            self.run_with_observer(concurrency, updates, &NoTelemetry, exec)
                .await
        }

        async fn run_with_observer<F, Fut, R>(
            self,
            concurrency: &Arc<ConcurrencyHandler>,
            updates: &dyn DiceUpdater,
            observer: &dyn CommandTransactionObserver,
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
                    self.exclusive_cmd,
                    CancellationContext::testing(),
                    self.preemptible,
                    observer,
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

    /// Direct tests of the `ConcurrencyHandlerData` state machine.
    ///
    /// These exercise the transition methods as plain functions rather than through `enter`. That
    /// makes cases reachable that the command path cannot produce deterministically — notably the
    /// stale-epoch guard, which via `enter` needs three commands and a specific scheduling delay.
    ///
    /// The trade is that these pin the transitions' **contract**, not their **reachability**. That
    /// a guard behaves correctly when handed a stale epoch does not demonstrate that a stale epoch
    /// can arise in practice; only an interleaving test would show that, and it needs a
    /// deterministically drivable arbiter.
    mod state_machine {
        use super::*;

        fn cleanup_at(epoch: usize) -> DiceStatus {
            DiceStatus::Cleanup {
                future: futures::future::ready(()).boxed().shared(),
                epoch,
            }
        }

        async fn active_status(dice: &Arc<Dice>) -> DiceStatus {
            DiceStatus::active(dice.updater().commit().await.equality_token())
        }

        fn data_with(dice_status: DiceStatus, cleanup_epoch: usize) -> ConcurrencyHandlerData {
            ConcurrencyHandlerData {
                dice_status,
                active_commands: SmallMap::new(),
                cleanup_epoch,
                previously_tainted: false,
            }
        }

        fn a_command() -> CommandData {
            CommandData {
                trace_id: TraceId::new(),
                display_command: "buck2".to_owned(),
                preemption_setting: PreemptibleWhen::Never,
                preempt: None,
            }
        }

        #[tokio::test]
        async fn transition_to_idle_completes_the_matching_cleanup() {
            let mut data = data_with(cleanup_at(3), 3);
            data.transition_to_idle(3);
            assert_matches!(data.dice_status, DiceStatus::Available { active: None });
        }

        /// Guard A. Two commands can await the same cleanup future; the first to reacquire the lock
        /// completes the transition and may install a fresh `ActiveDice`. The second must not then
        /// clear it.
        #[tokio::test]
        async fn transition_to_idle_is_a_noop_once_already_available() {
            let dice = make_default_dice();
            let mut data = data_with(active_status(&dice).await, 3);
            data.transition_to_idle(3);
            assert_matches!(
                data.dice_status,
                DiceStatus::Available { active: Some(..) },
                "an already-completed cleanup must not discard the active DICE version"
            );
        }

        /// Guard B. A waiter parked across an entire later command can return holding an epoch that
        /// has since been superseded by a *new* cleanup. Completing that stale cleanup would
        /// release commands to run against a DICE state that has not drained.
        #[tokio::test]
        async fn transition_to_idle_is_a_noop_for_a_superseded_epoch() {
            let mut data = data_with(cleanup_at(4), 4);
            data.transition_to_idle(3);
            assert_matches!(
                data.dice_status,
                DiceStatus::Cleanup { epoch: 4, .. },
                "a stale waiter must not complete a newer cleanup"
            );
        }

        #[tokio::test]
        async fn transition_to_cleanup_advances_the_epoch_when_idle() {
            let dice = make_default_dice();
            let mut data = data_with(active_status(&dice).await, 7);

            assert!(data.transition_to_cleanup(&dice));
            assert_eq!(data.cleanup_epoch, 8);
            assert_matches!(
                data.dice_status,
                DiceStatus::Cleanup { epoch: 8, .. },
                "the new cleanup should carry the advanced epoch"
            );
        }

        fn preemptible_command(setting: PreemptibleWhen) -> CommandData {
            // The receiver is dropped immediately; `cancel_preemptible_commands` ignores the send
            // result, and what is under test is whether it takes the sender.
            let (tx, _rx) = oneshot::channel();
            CommandData {
                trace_id: TraceId::new(),
                display_command: "buck2".to_owned(),
                preemption_setting: setting,
                preempt: Some(tx),
            }
        }

        /// Exhaustive mapping for `determine_bypass_semaphore`.
        ///
        /// Region coverage already shows every arm of this function executes, and would continue to
        /// show that if the arms were permuted — which input produces which outcome is precisely
        /// what region coverage cannot see, and what branch coverage would have caught. Hence a
        /// table.
        #[tokio::test]
        async fn bypass_semaphore_mapping() {
            let c = ConcurrencyHandler::new(make_default_dice());

            assert_matches!(
                c.determine_bypass_semaphore(true, true),
                BypassSemaphore::Run(RunState::NestedSameState)
            );
            assert_matches!(
                c.determine_bypass_semaphore(true, false),
                BypassSemaphore::Run(RunState::ParallelSameState)
            );
            assert_matches!(
                c.determine_bypass_semaphore(false, true),
                BypassSemaphore::Error
            );
            assert_matches!(
                c.determine_bypass_semaphore(false, false),
                BypassSemaphore::Block
            );
        }

        /// Pins which run state produces a warning and which commands it names.
        #[tokio::test]
        async fn nested_same_state_warning_mapping() {
            let parent = a_command();
            let parent_trace = parent.trace_id.dupe();
            let mut active = SmallMap::new();
            active.insert(CommandId(0), parent);

            let current = a_command();
            let current_trace = current.trace_id.dupe();

            assert_matches!(
                ConcurrencyHandler::nested_same_state_warning(
                    RunState::ParallelSameState,
                    &active,
                    &current,
                ),
                None
            );

            let (named, _argv) = ConcurrencyHandler::nested_same_state_warning(
                RunState::NestedSameState,
                &active,
                &current,
            )
            .expect("a nested same-state invocation is reported");

            // The warning names both the parent and recursive command.
            assert_eq!(
                named.to_string(),
                format!("{parent_trace}, {current_trace}")
            );
        }

        /// Exhaustive `PreemptibleWhen` x `is_same_state` matrix, for the same reason.
        ///
        /// All six are reachable — `Never` is the proto default, so it is the most common setting
        /// in production. What each call site fixes is `is_same_state`, not the setting: the site
        /// inside `if !is_same_state` always passes `false`, and the one under
        /// `BypassSemaphore::Run` always passes `true`. So both columns occur, and every row is
        /// exercised at both.
        #[tokio::test]
        async fn preemption_matrix() {
            let concurrency = ConcurrencyHandler::new(make_default_dice());
            let settings = [
                PreemptibleWhen::Never,
                PreemptibleWhen::Always,
                PreemptibleWhen::OnDifferentState,
            ];

            //                     Never, Always, OnDifferentState
            for (is_same_state, expected) in
                [(true, [false, true, false]), (false, [false, true, true])]
            {
                let mut data = data_with(cleanup_at(0), 0);
                for (i, setting) in settings.iter().enumerate() {
                    data.active_commands
                        .insert(CommandId(i), preemptible_command(*setting));
                }

                concurrency.cancel_preemptible_commands(&mut data, is_same_state);

                for (i, setting) in settings.iter().enumerate() {
                    let preempted = data
                        .active_commands
                        .get(&CommandId(i))
                        .unwrap()
                        .preempt
                        .is_none();
                    assert_eq!(
                        preempted, expected[i],
                        "{setting:?} with is_same_state={is_same_state}"
                    );
                }
            }
        }

        #[tokio::test]
        async fn transition_to_cleanup_refuses_while_commands_are_active() {
            let dice = make_default_dice();
            let mut data = data_with(active_status(&dice).await, 7);
            data.active_commands.insert(CommandId(0), a_command());

            assert!(!data.transition_to_cleanup(&dice));
            assert_eq!(
                data.cleanup_epoch, 7,
                "a refused transition must not burn an epoch"
            );
            assert_matches!(
                data.dice_status,
                DiceStatus::Available { active: Some(..) },
                "a refused transition must leave the active DICE version in place"
            );
        }
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

    /// A failing `DiceUpdater` fails its command without wedging the handler for the next one.
    #[tokio::test]
    async fn a_failing_updater_leaves_the_handler_usable() -> buck2_error::Result<()> {
        let concurrency = ConcurrencyHandler::new(make_default_dice());

        let failed = TestCommand::new()
            .run(&concurrency, &FailingUpdater, |_, _timing| async move {})
            .await;
        assert!(
            failed.is_err(),
            "the command should surface the update failure"
        );

        TestCommand::new()
            .run(&concurrency, &NoChanges, |_, _timing| async move {})
            .await?;

        Ok(())
    }

    /// A queued exclusive command must not deadlock a nested invocation with its parent.
    #[tokio::test]
    async fn a_queued_exclusive_command_does_not_block_nested_invocations()
    -> buck2_error::Result<()> {
        // Matches the existing nested-invocation test's OSS exclusion.
        if is_open_source() {
            return Ok(());
        }

        let concurrency = ConcurrencyHandler::new(make_default_dice());

        let block = Arc::new(RwLock::new(()));
        let blocked = block.write().await;
        let parent_running = Arc::new(Barrier::new(2));

        let parent = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = parent_running.dupe();
            let block = block.dupe();
            async move {
                TestCommand::new()
                    .run(&concurrency, &NoChanges, |_, _timing| async move {
                        barrier.wait().await;
                        let _g = block.read().await;
                    })
                    .await
            }
        });

        parent_running.wait().await;

        let exclusive = tokio::spawn({
            let concurrency = concurrency.dupe();
            async move {
                TestCommand::new()
                    .exclusive_cmd("clean")
                    .run(&concurrency, &NoChanges, |_, _timing| async move {})
                    .await
            }
        });

        // Wait until the writer starts turning away new readers.
        tokio::time::timeout(Duration::from_secs(10), async {
            while concurrency
                .exclusive_command_lock
                .owning_command()
                .is_none()
            {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("the exclusive command never queued");

        // Bound the child because the regression is a deadlock.
        tokio::time::timeout(
            Duration::from_secs(20),
            TestCommand::new().nested_invocation(true).run(
                &concurrency,
                &NoChanges,
                |_, _timing| async move {},
            ),
        )
        .await
        .expect("a nested invocation was blocked behind the queued exclusive command")?;

        drop(blocked);
        parent.await??;
        exclusive.await??;

        Ok(())
    }

    /// A slow observer runs after registration without holding the state lock.
    #[tokio::test]
    async fn a_slow_observer_runs_after_registration_without_the_state_lock()
    -> buck2_error::Result<()> {
        let concurrency = ConcurrencyHandler::new(make_default_dice());

        let entered = Arc::new(Barrier::new(2));
        let release = Arc::new(Barrier::new(2));
        let observer = Arc::new(BlockingObserver {
            entered: entered.dupe(),
            release: release.dupe(),
            fail: false,
        });
        let exec_ran = Arc::new(AtomicBool::new(false));

        let command = tokio::spawn({
            let concurrency = concurrency.dupe();
            let observer = observer.dupe();
            let exec_ran = exec_ran.dupe();
            async move {
                TestCommand::new()
                    .run_with_observer(
                        &concurrency,
                        &NoChanges,
                        observer.as_ref(),
                        move |_, _timing| async move {
                            exec_ran.store(true, Ordering::SeqCst);
                        },
                    )
                    .await
            }
        });

        tokio::time::timeout(Duration::from_secs(10), entered.wait())
            .await
            .buck_error_context("the observer was never reached")?;

        let data = concurrency
            .data
            .try_lock()
            .expect("the observer is running with the state lock held");
        assert_eq!(
            data.active_commands.len(),
            1,
            "the command must register before its observer runs"
        );
        assert!(
            !exec_ran.load(Ordering::SeqCst),
            "command execution started before its observer completed"
        );
        drop(data);

        release.wait().await;
        let joined = tokio::time::timeout(Duration::from_secs(10), command)
            .await
            .buck_error_context("the command did not finish")?;
        joined??;
        assert!(exec_ran.load(Ordering::SeqCst));

        Ok(())
    }

    /// Observer failure must wake a command waiting for a different state.
    #[tokio::test]
    async fn a_failing_observer_wakes_a_different_state_waiter() -> buck2_error::Result<()> {
        let concurrency = ConcurrencyHandler::new(make_default_dice());

        let entered = Arc::new(Barrier::new(2));
        let release = Arc::new(Barrier::new(2));
        let observer = Arc::new(BlockingObserver {
            entered: entered.dupe(),
            release: release.dupe(),
            fail: true,
        });
        let failed_exec_ran = Arc::new(AtomicBool::new(false));

        let first = tokio::spawn({
            let concurrency = concurrency.dupe();
            let observer = observer.dupe();
            let failed_exec_ran = failed_exec_ran.dupe();
            async move {
                TestCommand::new()
                    .run_with_observer(
                        &concurrency,
                        &NoChanges,
                        observer.as_ref(),
                        move |_, _timing| async move {
                            failed_exec_ran.store(true, Ordering::SeqCst);
                        },
                    )
                    .await
            }
        });

        tokio::time::timeout(Duration::from_secs(10), entered.wait())
            .await
            .buck_error_context("the observer was never reached")?;

        let waiter_events = TestEvents::new();
        let waiter_ran = Arc::new(AtomicBool::new(false));
        let waiter = tokio::spawn({
            let concurrency = concurrency.dupe();
            let waiter_events = waiter_events.dupe();
            let waiter_ran = waiter_ran.dupe();
            async move {
                TestCommand::new()
                    .dispatcher(waiter_events)
                    .run(&concurrency, &CtxDifferent, move |_, _timing| async move {
                        waiter_ran.store(true, Ordering::SeqCst);
                    })
                    .await
            }
        });

        waiter_events
            .wait_for(|event| {
                matches!(
                    event,
                    RecordedEvent::SpanStart(
                        buck2_data::span_start_event::Data::DiceBlockConcurrentCommand(..)
                    )
                )
            })
            .await?;
        assert!(
            !waiter_ran.load(Ordering::SeqCst),
            "the different-state command ran while the observer was active"
        );

        release.wait().await;

        let joined = tokio::time::timeout(Duration::from_secs(10), first)
            .await
            .buck_error_context("the failing command did not finish")?;
        let failed = joined?;
        assert!(failed.is_err(), "the observer failure was not returned");
        assert!(
            !failed_exec_ran.load(Ordering::SeqCst),
            "command execution ran after its observer failed"
        );

        let joined = tokio::time::timeout(Duration::from_secs(10), waiter)
            .await
            .buck_error_context("the different-state waiter was not woken")?;
        joined??;
        assert!(waiter_ran.load(Ordering::SeqCst));

        wait_for_commands_to_be_reaped(&concurrency).await?;
        assert!(concurrency.data.lock().await.has_no_active_commands());

        Ok(())
    }

    /// A failing observer must deregister its already-registered command.
    #[tokio::test]
    async fn a_failing_observer_leaves_the_handler_usable() -> buck2_error::Result<()> {
        let concurrency = ConcurrencyHandler::new(make_default_dice());

        let failed = TestCommand::new()
            .run_with_observer(
                &concurrency,
                &NoChanges,
                &FailingObserver,
                |_, _timing| async move {},
            )
            .await;
        assert!(
            failed.is_err(),
            "the command should surface the observer failure"
        );

        // Asynchronous, because the failed command did register and is removed by its guard.
        wait_for_commands_to_be_reaped(&concurrency).await?;
        {
            let data = concurrency.data.lock().await;
            assert!(
                data.has_no_active_commands(),
                "a command that failed after registering must still be reaped"
            );
        }

        // Same state: reuses the version the failed command installed, rather than installing a
        // fresh one. `DiceEqualityCheck` is only emitted when an existing `ActiveDice` is compared
        // against, so observing it is what distinguishes reuse from teardown-and-reinstall.
        let reuse = TestEvents::new();
        TestCommand::new()
            .dispatcher(reuse.dupe())
            .run(&concurrency, &NoChanges, |_, _timing| async move {})
            .await?;
        reuse
            .wait_for(|e| {
                matches!(
                    e,
                    RecordedEvent::Instant(buck2_data::instant_event::Data::DiceEqualityCheck(
                        DiceEqualityCheck { is_equal: true }
                    ))
                )
            })
            .await?;

        // Deregistration is asynchronous, so without this the next command can still see the
        // previous one registered, take the `Block` path instead of cleanup, and pass without
        // exercising the transition this is meant to cover.
        wait_for_commands_to_be_reaped(&concurrency).await?;

        // Different state: must transition through cleanup despite the orphaned version, which
        // ends with a fresh `ActiveDice` installed and therefore `NoActiveDiceState` reported.
        let cleanup = TestEvents::new();
        TestCommand::new()
            .dispatcher(cleanup.dupe())
            .run(&concurrency, &CtxDifferent, |_, _timing| async move {})
            .await?;
        cleanup
            .wait_for(|e| {
                matches!(
                    e,
                    RecordedEvent::Instant(buck2_data::instant_event::Data::NoActiveDiceState(..))
                )
            })
            .await?;

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
        // second one many times so that it makes as much progress as it can.
        //
        // The `yield_now` is load-bearing. `poll!` drives this future and nothing else, but
        // reaching the update means first awaiting DICE, which only answers when the runtime gets
        // to run its own tasks. Without a yield the second command stalls before it ever reaches
        // the permit, and the assertion below holds for a reason that has nothing to do with
        // synchronization — it passes just as happily with two permits.
        for _ in 0..100 {
            assert_matches!(poll!(&mut fut2), Poll::Pending);
            tokio::task::yield_now().await;
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
        // Bounded: without the `--exit-when=notidle` gate this command blocks forever rather
        // than refusing, so an unbounded await turns a regression into a 10 minute harness
        // timeout instead of a fast failure.
        let fut2_result = tokio::time::timeout(Duration::from_secs(10), fut2)
            .await
            .expect("`--exit-when=notidle` should refuse immediately, not block")?;
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
        // Bounded: without the `--exit-when=notidle` gate this command blocks forever rather
        // than refusing, so an unbounded await turns a regression into a 10 minute harness
        // timeout instead of a fast failure.
        let fut2_result = tokio::time::timeout(Duration::from_secs(10), fut2)
            .await
            .expect("`--exit-when=notidle` should refuse immediately, not block")?;
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
        // Bounded: without the `--exit-when=notidle` gate this command blocks forever rather
        // than refusing, so an unbounded await turns a regression into a 10 minute harness
        // timeout instead of a fast failure.
        let fut2_result = tokio::time::timeout(Duration::from_secs(10), fut2)
            .await
            .expect("`--exit-when=notidle` should refuse immediately, not block")?;
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
        // Bounded: without the `--exit-when=notidle` gate this command blocks forever rather
        // than refusing, so an unbounded await turns a regression into a 10 minute harness
        // timeout instead of a fast failure.
        let fut2_result = tokio::time::timeout(Duration::from_secs(10), fut2)
            .await
            .expect("`--exit-when=notidle` should refuse immediately, not block")?;
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
        // Bounded: without the `--exit-when=notidle` gate this command blocks forever rather
        // than refusing, so an unbounded await turns a regression into a 10 minute harness
        // timeout instead of a fast failure.
        let fut2_result = tokio::time::timeout(Duration::from_secs(10), fut2)
            .await
            .expect("`--exit-when=notidle` should refuse immediately, not block")?;
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
