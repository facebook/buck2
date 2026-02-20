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
use buck2_build_signals::env::EXCLUSIVE_COMMAND_WAIT;
use buck2_build_signals::env::EarlyCommandTimingBuilder;
use buck2_cli_proto::client_context::ExitWhen;
use buck2_cli_proto::client_context::PreemptibleWhen;
use buck2_common::legacy_configs::dice::HasInjectedLegacyConfigs;
use buck2_core::fs::project::ProjectRoot;
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
use buck2_events::dispatch::EventDispatcher;
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

use crate::ctx::LockedPreviousCommandData;
use crate::experiment_util::get_experiment_tags;

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
    dispatcher: EventDispatcher,
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
        early_timings: &mut EarlyCommandTimingBuilder,
    ) -> buck2_error::Result<(DiceTransactionUpdater, UserComputationData)>;
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
    pub async fn enter<F, Fut, R>(
        self: &Arc<Self>,
        event_dispatcher: EventDispatcher,
        updates: &dyn DiceUpdater,
        exec: F,
        is_nested_invocation: bool,
        sanitized_argv: Vec<String>,
        exclusive_cmd: Option<String>,
        cancellations: &CancellationContext,
        preemptible: PreemptibleWhen,
        previous_command_data: Arc<LockedPreviousCommandData>,
        project_root: &ProjectRoot,
        exit_when: ExitWhen,
        mut early_command_timing: EarlyCommandTimingBuilder,
    ) -> buck2_error::Result<R>
    where
        F: FnOnce(DiceTransaction, EarlyCommandTimingBuilder) -> Fut,
        Fut: Future<Output = R> + Send,
    {
        let _exclusive_command_guard = event_dispatcher
            .span_async(
                ExclusiveCommandWaitStart {
                    command_name: self.exclusive_command_lock.owning_command(),
                },
                {
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
                        (guard, ExclusiveCommandWaitEnd {})
                    }
                },
            )
            .await;

        let events = event_dispatcher.dupe();
        let (_guard, transaction, preempt_receiver) = event_dispatcher
            .span_async(DiceSynchronizeSectionStart {}, {
                let early_command_timing = &mut early_command_timing;

                async move {
                    (
                        cancellations
                            .critical_section(|| {
                                self.wait_for_others(
                                    updates,
                                    early_command_timing,
                                    events,
                                    is_nested_invocation,
                                    sanitized_argv,
                                    preemptible,
                                    previous_command_data,
                                    project_root,
                                    exit_when,
                                )
                            })
                            .await,
                        DiceSynchronizeSectionEnd {},
                    )
                }
            })
            .await?;

        let result = exec(transaction, early_command_timing);
        pin_mut!(result);
        pin_mut!(preempt_receiver);

        match future::select(result, preempt_receiver).await {
            Either::Left((result, _)) => Ok(result),
            Either::Right((_preemption, _)) => {
                event_dispatcher.instant_event(CommandPreempted {});
                Err(ConcurrencyHandlerError::ExitOnPreemption.into())
            }
        }
    }

    // this is normally super unsafe, but because we are using an async condvar that takes care
    // of unlocking this mutex, this mutex is actually essentially never held across awaits.
    // The async condvar will handle properly allowing under threads to proceed, avoiding
    // starvation.
    async fn wait_for_others(
        self: &Arc<Self>,
        updates: &dyn DiceUpdater,
        early_timings: &mut EarlyCommandTimingBuilder,
        event_dispatcher: EventDispatcher,
        is_nested_invocation: bool,
        sanitized_argv: Vec<String>,
        preemptible: PreemptibleWhen,
        previous_command_data: Arc<LockedPreviousCommandData>,
        project_root: &ProjectRoot,
        exit_when: ExitWhen,
    ) -> buck2_error::Result<(
        OnExecExit,
        DiceTransaction,
        impl Future<Output = Result<(), RecvError>> + use<>,
    )> {
        // Have to put it on the function unfortunately, https://github.com/rust-lang/rust-clippy/issues/9047
        #![allow(clippy::await_holding_invalid_type)]

        let trace = event_dispatcher.trace_id().dupe();
        let current_sanitized_argv = sanitized_argv.clone();

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
            dispatcher: event_dispatcher.dupe(),
            preemption_setting: preemptible,
            preempt: Some(preempt_sender),
        };

        let (mut transaction, tainted) = loop {
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

                        let (transaction, user_data) =
                            updates.update(updater, early_timings).await?;

                        let transaction = event_dispatcher
                            .span_async(buck2_data::DiceStateUpdateStart {}, async {
                                (
                                    async {
                                        let transaction =
                                            transaction.commit_with_data(user_data).await;
                                        buck2_error::Ok(transaction)
                                    }
                                    .await,
                                    buck2_data::DiceStateUpdateEnd {},
                                )
                            })
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

                        event_dispatcher.instant_event(DiceEqualityCheck {
                            is_equal: is_same_state,
                        });

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

                                data = event_dispatcher
                                    .span_async(
                                        DiceBlockConcurrentCommandStart {
                                            current_active_trace_id: trace_id.to_string(),
                                            cmd_args: argv,
                                        },
                                        async {
                                            (
                                                self.cond.wait((data, &self.data)).await,
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

        if transaction
            .is_injected_external_buckconfig_data_key_set()
            .await?
        {
            let external_configs = transaction.get_injected_external_buckconfig_data().await?;
            let current_external_and_local_configs: Vec<buck2_data::BuckconfigComponent> =
                external_configs
                    .get_buckconfig_components(project_root)
                    .await;

            let mut previous_command_data = previous_command_data.data.lock().unwrap();

            previous_command_data.process_current_command(
                event_dispatcher.dupe(),
                current_external_and_local_configs.clone(),
                current_sanitized_argv,
                trace,
            );

            event_dispatcher.instant_event(buck2_data::TagEvent {
                tags: get_experiment_tags(&current_external_and_local_configs),
            });
            event_dispatcher.instant_event(buck2_data::BuckconfigInputValues {
                components: current_external_and_local_configs,
            });
        }
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
                .into()
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
    use buck2_build_signals::env::EXCLUSIVE_COMMAND_WAIT;
    use buck2_build_signals::env::FILE_WATCHER_WAIT;
    use buck2_common::legacy_configs::dice::SetLegacyConfigs;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::is_open_source;
    #[cfg(fbcode_build)]
    use buck2_events::BuckEvent;
    #[cfg(fbcode_build)]
    use buck2_events::create_source_sink_pair;
    use buck2_events::daemon_id::DaemonId;
    use buck2_events::sink::null::NullEventSink;
    #[cfg(fbcode_build)]
    use buck2_events::source::ChannelEventSource;
    #[cfg(fbcode_build)]
    use buck2_events::span::SpanId;
    use derivative::Derivative;
    use dice::DetectCycles;
    use dice::DiceComputations;
    use dice::InjectedKey;
    use dice::Key;
    use dice_futures::cancellation::CancellationContext;
    use dupe::Dupe;
    use futures::pin_mut;
    use futures::poll;
    use parking_lot::Mutex;
    use tokio::sync::Barrier;
    use tokio::sync::RwLock;

    use super::*;
    use crate::ctx::LockedPreviousCommandData;

    /// Creates a new null Event Dispatcher with trace ID that accepts events but does not write them anywhere.
    fn null_sink_with_trace(trace_id: TraceId) -> EventDispatcher {
        EventDispatcher::new(trace_id, DaemonId::new(), NullEventSink::new())
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

    #[derive(Clone, Dupe, Display, Debug, Hash, Eq, PartialEq, Allocative)]
    struct K;

    #[async_trait]
    impl InjectedKey for K {
        type Value = ();

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            false
        }
    }

    async fn make_default_dice() -> Arc<Dice> {
        let dice = Dice::builder().build(DetectCycles::Enabled);
        let mut updater = dice.updater();
        drop(updater.set_none_legacy_config_external_data());
        updater.commit().await;
        dice
    }

    #[tokio::test]
    async fn nested_invocation_same_transaction() {
        // FIXME: This times out on open source, and we don't know why
        if is_open_source() {
            return;
        }
        let dice = make_default_dice().await;
        let concurrency = ConcurrencyHandler::new(dice);

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();
        let traces3 = TraceId::new();

        let barrier = Arc::new(Barrier::new(3));

        let project_root_temp: ProjectRootTemp = ProjectRootTemp::new().unwrap();

        let fut1 = concurrency.enter(
            null_sink_with_trace(traces1),
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
            LockedPreviousCommandData::default().into(),
            project_root_temp.path(),
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );
        let fut2 = concurrency.enter(
            null_sink_with_trace(traces2),
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
            LockedPreviousCommandData::default().into(),
            project_root_temp.path(),
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );
        let fut3 = concurrency.enter(
            null_sink_with_trace(traces3),
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
            LockedPreviousCommandData::default().into(),
            project_root_temp.path(),
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
        let dice = make_default_dice().await;

        let concurrency = ConcurrencyHandler::new(dice);

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        let barrier = Arc::new(Barrier::new(2));
        let project_root_temp: ProjectRootTemp = ProjectRootTemp::new().unwrap();

        let fut1 = concurrency.enter(
            null_sink_with_trace(traces1),
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
            LockedPreviousCommandData::default().into(),
            project_root_temp.path(),
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );

        let fut2 = concurrency.enter(
            null_sink_with_trace(traces2),
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
            LockedPreviousCommandData::default().into(),
            project_root_temp.path(),
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
        let dice = make_default_dice().await;

        let concurrency = ConcurrencyHandler::new(dice);

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();
        let traces3 = TraceId::new();

        let barrier = Arc::new(Barrier::new(3));

        let project_root_temp: ProjectRootTemp = ProjectRootTemp::new().unwrap();

        let fut1 = concurrency.enter(
            null_sink_with_trace(traces1),
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
            LockedPreviousCommandData::default().into(),
            project_root_temp.path(),
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );
        let fut2 = concurrency.enter(
            null_sink_with_trace(traces2),
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
            LockedPreviousCommandData::default().into(),
            project_root_temp.path(),
            ExitWhen::ExitNever,
            EarlyCommandTimingBuilder::new(Instant::now()),
        );
        let fut3 = concurrency.enter(
            null_sink_with_trace(traces3),
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
            LockedPreviousCommandData::default().into(),
            project_root_temp.path(),
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
        let dice = make_default_dice().await;

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
                        null_sink_with_trace(traces1),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                        null_sink_with_trace(traces2),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                        null_sink_with_trace(traces_different),
                        &CtxDifferent,
                        |_, _timing| async move {
                            arrived.store(true, Ordering::Relaxed);
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;

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
                        null_sink_with_trace(traces1),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                        null_sink_with_trace(traces2),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                        null_sink_with_trace(traces_different),
                        &CtxDifferent,
                        |_, _timing| async move {
                            arrived.store(true, Ordering::Relaxed);
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;

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
                        null_sink_with_trace(traces1),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                        null_sink_with_trace(traces2),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                        null_sink_with_trace(traces_different),
                        &CtxDifferent,
                        |_, _timing| async move {
                            arrived.store(true, Ordering::Relaxed);
                        },
                        false,
                        Vec::new(),
                        None,
                        CancellationContext::testing(),
                        PreemptibleWhen::Never,
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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

    #[derive(Clone, Dupe, Derivative, Allocative, Display)]
    #[derivative(Hash, Eq, PartialEq, Debug)]
    #[display("CleanupTestKey")]
    struct CleanupTestKey {
        #[derivative(Debug = "ignore", Hash = "ignore", PartialEq = "ignore")]
        is_executing: Arc<Mutex<()>>,
    }

    #[async_trait::async_trait]
    impl Key for CleanupTestKey {
        type Value = ();

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
    async fn test_cleanup_stage() -> buck2_error::Result<()> {
        let key = CleanupTestKey {
            is_executing: Arc::new(Mutex::new(())),
        };

        let key = &key;

        let dice = make_default_dice().await;

        let concurrency = ConcurrencyHandler::new(dice.dupe());

        // Kick off our computation and wait until it's running.

        concurrency
            .enter(
                EventDispatcher::null(),
                &NoChanges,
                |mut dice, _timing| async move {
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
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        // Now, re-enter. We expect to reuse and therefore to not wait.

        concurrency
            .enter(
                EventDispatcher::null(),
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
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        // Now, enter with a different context. This time, we expect to not reuse.

        concurrency
            .enter(
                EventDispatcher::null(),
                &CtxDifferent,
                |_dice, _timing| async move {
                    assert!(!key.is_executing.is_locked());
                },
                false,
                Vec::new(),
                None,
                CancellationContext::testing(),
                PreemptibleWhen::Never,
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        Ok(())
    }

    #[cfg(fbcode_build)]
    async fn wait_for_event<F>(
        source: &mut ChannelEventSource,
        matcher: Box<F>,
    ) -> buck2_error::Result<BuckEvent>
    where
        F: Fn(&BuckEvent) -> bool + Send,
    {
        // 2 millis was too short on windows, fails concurrency::tests::exclusive_command_lock
        tokio::time::timeout(Duration::from_millis(4), async {
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
        .buck_error_context("Time out waiting for matching buck event")
    }

    #[cfg(fbcode_build)]
    async fn wait_for_exclusive_span_start(
        source: &mut ChannelEventSource,
        cmd: Option<&str>,
    ) -> buck2_error::Result<Option<SpanId>> {
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

    #[cfg(fbcode_build)]
    async fn wait_for_exclusive_span_end(
        source: &mut ChannelEventSource,
        span_id: Option<SpanId>,
    ) -> buck2_error::Result<BuckEvent> {
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

    #[cfg(fbcode_build)]
    #[tokio::test]
    async fn exclusive_command_lock() -> buck2_error::Result<()> {
        let dice = make_default_dice().await;
        let concurrency = ConcurrencyHandler::new(dice.dupe());
        let (mut source, sink) = create_source_sink_pair();
        let dispatcher = EventDispatcher::new(TraceId::new(), DaemonId::new(), sink);

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
                            LockedPreviousCommandData::default().into(),
                            ProjectRootTemp::new().unwrap().path(),
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
    async fn test_thundering_herd() -> buck2_error::Result<()> {
        let dice = make_default_dice().await;

        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let concurrency = &concurrency;

        let tasks = (0..3).map(|_i| async {
            concurrency
                .enter(
                    EventDispatcher::null(),
                    &CtxDifferent,
                    |mut dice, _timing| async move {
                        // NOTE: We need to actually compute something for DICE to be not-idle.
                        dice.compute(&K).await.unwrap();
                        tokio::task::yield_now().await;
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never,
                    LockedPreviousCommandData::default().into(),
                    ProjectRootTemp::new().unwrap().path(),
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

        let dice = make_default_dice().await;

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
        let project_root_temp = ProjectRootTemp::new().unwrap();
        let fut1 = concurrency.enter(
            EventDispatcher::null(),
            &updater1,
            |_dice, _timing| async move {
                tokio::task::yield_now().await;
            },
            false,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            LockedPreviousCommandData::default().into(),
            project_root_temp.path(),
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
            EventDispatcher::null(),
            &updater2,
            |_dice, _timing| async move {
                tokio::task::yield_now().await;
            },
            false,
            Vec::new(),
            None,
            CancellationContext::testing(),
            PreemptibleWhen::Never,
            LockedPreviousCommandData::default().into(),
            project_root_temp.path(),
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
        let dice = make_default_dice().await;
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
                        null_sink_with_trace(traces1),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                    null_sink_with_trace(traces2),
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
                    LockedPreviousCommandData::default().into(),
                    ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
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
                        null_sink_with_trace(traces1),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                    null_sink_with_trace(traces2),
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
                    LockedPreviousCommandData::default().into(),
                    ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
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
                        null_sink_with_trace(traces1),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                    null_sink_with_trace(traces2),
                    &NoChanges,
                    |_, _timing| async move {
                        panic!("Should not execute");
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never,
                    LockedPreviousCommandData::default().into(),
                    ProjectRootTemp::new().unwrap().path(),
                    ExitWhen::ExitNotIdle,
                    EarlyCommandTimingBuilder::new(Instant::now()),
                )
                .await
        }));

        let fut3 = tokio::spawn(buck2_util::async_move_clone!(concurrency, {
            concurrency
                .enter(
                    null_sink_with_trace(traces3),
                    &NoChanges,
                    |_, _timing| async move {
                        panic!("Should not execute");
                    },
                    false,
                    Vec::new(),
                    None,
                    CancellationContext::testing(),
                    PreemptibleWhen::Never,
                    LockedPreviousCommandData::default().into(),
                    ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
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
                        null_sink_with_trace(traces1),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                    null_sink_with_trace(traces2),
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
                    LockedPreviousCommandData::default().into(),
                    ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
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
                        null_sink_with_trace(traces1),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                    null_sink_with_trace(traces2),
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
                    LockedPreviousCommandData::default().into(),
                    ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
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
                        null_sink_with_trace(traces1),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                    null_sink_with_trace(traces2),
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
                    LockedPreviousCommandData::default().into(),
                    ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        // First command runs to completion
        concurrency
            .enter(
                null_sink_with_trace(traces1),
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
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        // Wait for a moment to let async cleanup processes finish.
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

        // Daemon should now be idle
        // Second command with --exit-when=notidle and same state should succeed
        let result = concurrency
            .enter(
                null_sink_with_trace(traces2),
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
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();

        // First command runs to completion with NoChanges state
        concurrency
            .enter(
                null_sink_with_trace(traces1),
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
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
                ExitWhen::ExitNever,
                EarlyCommandTimingBuilder::new(Instant::now()),
            )
            .await?;

        // Wait for a moment to let async cleanup processes finish.
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

        // Daemon should now be idle
        // Second command with --exit-when=notidle and different state should succeed
        let result = concurrency
            .enter(
                null_sink_with_trace(traces2),
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
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
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
                        null_sink_with_trace(traces1),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
                        null_sink_with_trace(traces2),
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
                        LockedPreviousCommandData::default().into(),
                        ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        let traces = TraceId::new();
        let duration_captured: Arc<Mutex<Duration>> = Arc::new(Mutex::new(Duration::ZERO));

        // Run a non-exclusive command (None for exclusive_cmd parameter)
        concurrency
            .enter(
                null_sink_with_trace(traces),
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
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
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
                null_sink_with_trace(traces),
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
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
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
        let dice = make_default_dice().await;
        let concurrency = ConcurrencyHandler::new(dice.dupe());

        // First, establish an active DICE state by running a command
        let traces_init = TraceId::new();
        concurrency
            .enter(
                null_sink_with_trace(traces_init),
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
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
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
                null_sink_with_trace(traces),
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
                LockedPreviousCommandData::default().into(),
                ProjectRootTemp::new().unwrap().path(),
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
