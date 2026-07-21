/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! A task stored by Dice that is shared for all transactions at the same version
use std::cell::UnsafeCell;
use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::task::Context;
use std::task::Poll;
use std::task::Waker;

use allocative::Allocative;
use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dice_futures::cancellation::CancellationContext;
use dice_futures::cancellation::DropcancelHandle;
use dice_futures::spawn::CancellableFutureSpawner;
use dice_futures::spawn::prepare_detached_cancellation;
use dice_futures::spawner::Spawner;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::BoxFuture;
use parking_lot::Mutex;

use super::handle::DiceTaskHandle;
use crate::GlobalStats;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::task::atomic_list::SimpleAtomicList;
use crate::impls::task::atomic_list::SimpleAtomicListNodeSpec;
use crate::impls::task::promise::DicePromise;
use crate::impls::task::promise::DiceSyncResult;
use crate::impls::value::DiceComputedValue;
use crate::introspection::DiceTaskState;

/// The shared, reference-counted state for computing a single key at a single version.
///
/// It behaves like a shared, spawned future: many dependents can await the same in-flight
/// computation, but none of them drive it. The value is produced by a separately spawned worker
/// (see `spawn_prepared_task`) that reports the result through a `DiceTaskCompletionHandle`.
/// Polling a dependent only registers a waker; it never does real work, and a waker is only woken
/// once the result is ready or the generation is cancelled/restarted.
///
/// Each `DicePromise` / `DiceTaskDependentFuture` is a dependent ("strong reference"), counted by
/// `strong_count`. When the last dependent goes away the in-flight computation is cancelled, but
/// the `DiceTask` is not discarded; the next dependent to arrive *restarts* it in place as a new
/// "generation", so the same `DiceTask` can outlive several cancelled computations. See
/// `depended_on_by` and the `current_generation` / `terminated_generation` fields for how
/// generations encode the task's state.
#[derive(Allocative, Clone, Dupe)]
#[repr(transparent)]
pub(crate) struct DiceTask {
    pub(crate) internal: crate::arc::Arc<DiceTaskInternal>,
}

impl DiceTask {
    pub(crate) fn as_ref(&self) -> DiceTaskRef<'_> {
        DiceTaskRef {
            internal: self.internal.borrow_arc(),
        }
    }

    #[cfg(test)]
    pub(crate) fn depended_on_by(&self, k: ParentKey) -> DiceTaskDependedOnByResult {
        self.as_ref().depended_on_by(k)
    }

    #[cfg(test)]
    pub(crate) fn waiters_count(&self) -> u32 {
        self.as_ref().waiters_count()
    }

    #[cfg(test)]
    pub(crate) fn is_ready(&self) -> bool {
        self.as_ref().is_ready()
    }

    #[cfg(test)]
    pub(crate) fn is_cancelled(&self) -> bool {
        self.as_ref().is_cancelled()
    }

    pub(crate) fn is_pending(&self) -> bool {
        self.as_ref().is_pending()
    }
}

#[derive(Copy, Clone, Dupe)]
pub(crate) struct DiceTaskRef<'d> {
    pub(crate) internal: crate::arc::ArcBorrow<'d, DiceTaskInternal>,
}

impl DiceTaskRef<'_> {
    pub(crate) fn clone_arc(self) -> DiceTask {
        DiceTask {
            internal: self.internal.clone_arc(),
        }
    }
}

#[derive(Allocative)]
#[allocative(skip)]
pub(crate) struct DiceTaskInternal {
    pub(crate) key: DiceKey,

    /// Holds some data that actually needs to be lock protected.
    ///
    /// This is `None` exactly once the task has been sealed (a worker finished successfully and set
    /// `terminated_generation` to 0). At that point the cancellation handle and sync-projection slot
    /// are useless, and since sealed tasks live in the cache indefinitely we drop the
    /// `Box<Critical>` to save memory. While the task is unsealed this is always `Some`.
    ///
    /// Additionally, there is an invariant that whenever any of the following happen:
    ///  - `strong_count` is changed 1 -> 0
    ///  - `strong_count` is changed 0 -> 1
    ///  - Either `current_generation` or `terminated_generation` are changed
    ///
    /// That must be done *with this lock held.*
    critical: Mutex<Option<Box<Critical>>>,

    /// The number of things waiting on this task.
    ///
    /// Changing this `0 -> 1` requires a release store (and subsequent acquire loads) so that the
    /// changes to `Critical` and other fields in this type are visible to other waiters later
    /// grabbing handles by incrementing this.
    strong_count: AtomicU32,

    /// Because things may be cancelled, it may take multiple attempts ("generations") before an
    /// execution is actually driven to completion. These fields track that.
    ///
    /// The `current_generation` is the most recently created generation. A new one is created when
    /// the strong count goes 0 -> 1, and the worker for the generation is cancelled when the strong
    /// count goes 1 -> 0.
    ///
    /// The `terminated_generation` is the most recent generation for which the associated worker
    /// has *fully* terminated, either by succeeding or by its cancellation completing. Note that
    /// cancellation is an async operation, so there may be some delay between strong count going to
    /// zero and the termination incrementing.
    ///
    /// Normally, it is the case that `current_generation` is at most `terminated_generation + 1`,
    /// but in the face of rapid cancellation that is not strictly required.
    ///
    /// When a worker finishes successfully (ie not via cancelling), `terminated_generation` is set
    /// to zero which "seals" this task in some sense.
    current_generation: AtomicU32,
    terminated_generation: AtomicU32,

    /// Wakers of everything currently parked on this task; as a mental model this can be thought of
    /// as having a number of entries equalling the strong count.
    ///
    /// In practice though there can be a lot of other stuff here (such as `TerminationObserver`s,
    /// which don't hold a strong count), and wakers here may actually be waiting on different
    /// generations. We don't attempt to be smart; when we wake anything, we wake everything, and
    /// things that didn't need to be woken just re-register themselves.
    wakers: SimpleAtomicList<WakeOnDrain>,

    /// The computed value; the access invariant is as follows:
    ///  1. Initially, only the `DiceTaskCompletionHandle` may access this; it writes `Some` here
    ///     when it's ready.
    ///  2. The `DiceTaskCompletionHandle` `Release` stores `0` into `terminated_generation` above.
    ///     This value is immutable from then on.
    ///  3. Anything else which `Acquire` loads the `0` from `terminated_generation` may read this.
    maybe_value: UnsafeCell<Option<DiceComputedValue>>,

    /// The most recently provided cancellation reason
    ///
    /// FIXME(JakobDegen): This is also kind of messy, users of this might want the cancellation
    /// reason for a specific generation, not whatever this is. But also, a previous version of this
    /// code made a (poor) attempt to have some distinction between final cancellations - those that
    /// are expected to retire the task in the minor version for good - and cancellations that apply
    /// to just one generation. Possibly we should attempt to do that right.
    last_cancellation_reason: Mutex<Option<CancellationReason>>,
}

struct WakeOnDrain;
impl SimpleAtomicListNodeSpec for WakeOnDrain {
    type Value = Waker;
    fn drain(v: Waker) {
        v.wake();
    }
}

unsafe impl Send for DiceTaskInternal {}
unsafe impl Sync for DiceTaskInternal {}

struct Critical {
    /// Cancellation handle for the most recently created generation. The actual computation on the
    /// other side of this may have not yet started, or already been cancelled.
    cancellation_handle: DropcancelHandle,
    /// See `DiceSyncResult::sync_result` for what this is
    sync_value: Option<DiceComputedValue>,
}

pub(crate) enum ReadValueResult<'d> {
    Finished(&'d DiceComputedValue),
    Pending { terminated_generation: u32 },
}

/// Future that resolves when a task is finished or fully cancelled and terminated.
pub(crate) enum TerminationObserver {
    Done(Option<DiceComputedValue>),
    Pending { task: DiceTask, generation: u32 },
}

impl TerminationObserver {
    pub(crate) fn is_terminated(&self) -> bool {
        match self {
            TerminationObserver::Done(_) => true,
            TerminationObserver::Pending { task, generation } => {
                match task.as_ref().state_at(*generation) {
                    StateAtGeneration::TerminatedSuccess(_) => true,
                    StateAtGeneration::TerminatedCancelled => true,
                    StateAtGeneration::Pending => false,
                }
            }
        }
    }
}

impl Future for TerminationObserver {
    type Output = Option<DiceComputedValue>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let (task, generation) = match self.get_mut() {
            TerminationObserver::Done(v) => return Poll::Ready(v.take()),
            TerminationObserver::Pending { task, generation } => (&*task, *generation),
        };

        // Fast-path to avoid waker registration
        match task.as_ref().state_at(generation) {
            StateAtGeneration::TerminatedSuccess(v) => return Poll::Ready(Some(v.dupe())),
            StateAtGeneration::TerminatedCancelled => return Poll::Ready(None),
            StateAtGeneration::Pending => {}
        }

        // Register our waker, then re-check. The generation may have terminated between the check
        // above and this registration, and the `drain`/`close` that would have woken us may already
        // have fired — so we must report `Ready` ourselves rather than rely on a missed wakeup. The
        // re-check is correctly synchronized because `push` acquires from whatever last reset or
        // closed the list (see `SimpleAtomicList`).
        task.internal.wakers.push(cx.waker().clone());
        match task.as_ref().state_at(generation) {
            StateAtGeneration::TerminatedSuccess(v) => Poll::Ready(Some(v.dupe())),
            StateAtGeneration::TerminatedCancelled => Poll::Ready(None),
            StateAtGeneration::Pending => Poll::Pending,
        }
    }
}

/// Result of registering as a dependent of a task via `depended_on_by`.
pub(crate) enum DiceTaskDependedOnByResult<'d> {
    Finished(&'d DiceComputedValue),
    Pending(DicePromise<'d>),
    /// The task had been cancelled and this caller won the race to restart it. The caller must
    /// spawn the worker on the freshly prepared (next-generation) task, after awaiting termination
    /// of the previous generation.
    NeedsRestart(PreparedDiceTask<'d>, PreviouslyCancelledTask),
}

impl<'d> DiceTaskDependedOnByResult<'d> {
    /// Unwrap to a `DicePromise`. `Finished(v)` returns a ready promise; `Pending`
    /// returns the inner promise. Panics on `NeedsRestart` because callers using
    /// `unwrap` haven't agreed to drive the restart. Used in tests where the
    /// caller knows the task is in-flight.
    #[cfg(test)]
    #[track_caller]
    pub(crate) fn unwrap(self) -> DicePromise<'d> {
        match self {
            DiceTaskDependedOnByResult::Pending(p) => p,
            DiceTaskDependedOnByResult::Finished(v) => DicePromise::ready(v),
            DiceTaskDependedOnByResult::NeedsRestart(_, _) => {
                panic!("DiceTaskDependedOnByResult::unwrap called on NeedsRestart");
            }
        }
    }
}

/// Everything needed to start (or restart) one generation of a `DiceTask`: hand it to
/// `spawn_prepared_task` to launch the worker and obtain a `DicePromise`. All three members are
/// minted for the same generation.
pub(crate) struct PreparedDiceTask<'d> {
    /// Lets the worker report the value or cancellation for this generation.
    pub(crate) completion_handle: DiceTaskCompletionHandle,
    /// The first dependent of this generation; becomes the promise returned to the spawner.
    pub(crate) dependent_future: DiceTaskDependentFuture<'d>,
    /// Spawns the cancellable worker future for this generation.
    pub(crate) task_spawner: DiceTaskSpawner,
}

impl<'d> PreparedDiceTask<'d> {
    #[cfg(test)]
    pub(crate) fn task(&self) -> DiceTaskRef<'d> {
        self.dependent_future.task
    }
}

enum StateAtGeneration<'d> {
    Pending,
    TerminatedCancelled,
    TerminatedSuccess(&'d DiceComputedValue),
}

impl DiceTask {
    #[cfg(test)]
    pub(crate) fn prepare_testing(key: DiceKey) -> PreparedDiceTask<'static> {
        // Spawning a dice task normally needs some kind of arena to allocate the task into; we don't
        // have one, but this is tests, so just leak the thing
        DiceTask::prepare::<()>(key, |t| Ok(Box::leak(Box::new(t)).as_ref())).unwrap()
    }

    /// Prepare a task for execution.
    ///
    /// Takes an alloc callback which should allocate the task into something long lived so that
    /// further code can use references to it.
    ///
    /// Note: This API is just a little bit unsound, don't do anything funky in `alloc` by returning
    /// a ref to an unrelated dice task. Should be fine.
    pub(crate) fn prepare<'d, E>(
        key: DiceKey,
        alloc: impl FnOnce(DiceTask) -> Result<DiceTaskRef<'d>, E>,
    ) -> Result<PreparedDiceTask<'d>, E> {
        let (future_spawner, cancellation_handle) = prepare_detached_cancellation();

        let cancellation_handle = cancellation_handle.into_dropcancel();

        let task = Self {
            internal: crate::arc::Arc::new(DiceTaskInternal {
                key,
                strong_count: AtomicU32::new(1),
                // First generation. Per the invariant on `current_generation`, `term (1) ==
                // cur (2) - 1` is the "in progress" state; `0` is reserved as the "finished"
                // sentinel, so generations start at 2/1 rather than 1/0.
                current_generation: AtomicU32::new(2),
                terminated_generation: AtomicU32::new(1),
                maybe_value: UnsafeCell::new(None),
                wakers: SimpleAtomicList::new(),
                critical: Mutex::new(Some(Box::new(Critical {
                    cancellation_handle,
                    sync_value: None,
                }))),
                last_cancellation_reason: Mutex::new(None),
            }),
        };

        let task = alloc(task)?;
        debug_assert_eq!(
            task.internal.key, key,
            "alloc returned a ref for the wrong key"
        );

        Ok(PreparedDiceTask {
            task_spawner: DiceTaskSpawner {
                inner: future_spawner,
            },
            dependent_future: DiceTaskDependentFuture {
                task,
                completed: false,
                generation: 2,
            },
            completion_handle: DiceTaskCompletionHandle {
                generation: 2,
                task: task.clone_arc(),
            },
        })
    }
}

impl<'d> DiceTaskRef<'d> {
    /// `k` depends on this task, returning a `DicePromise` that will complete when this task
    /// completes
    pub(crate) fn depended_on_by(self, _k: ParentKey) -> DiceTaskDependedOnByResult<'d> {
        if let Some(v) = self.get_finished_value() {
            return DiceTaskDependedOnByResult::Finished(v);
        }

        let internal = &*self.internal;
        if internal
            .strong_count
            .fetch_update(Ordering::Relaxed, Ordering::Acquire, |v| {
                if v == 0 { None } else { Some(v + 1) }
            })
            .is_ok()
        {
            // fast path, strong_count > 0: task is in progress
            let generation = internal.current_generation.load(Ordering::Relaxed);
            return DiceTaskDependedOnByResult::Pending(DicePromise::pending(
                DiceTaskDependentFuture {
                    task: self,
                    completed: false,
                    generation,
                },
            ));
        }

        // strong_count == 0: task was cancelled, need to restart it.
        let mut guard = self.internal.critical.lock();

        // Now need to recheck in case we raced with something.
        if let Some(v) = self.get_finished_value() {
            // Something already finished it.
            return DiceTaskDependedOnByResult::Finished(v);
        }

        // We cannot update this 0 -> 1 until after we've stored our generation as current, but we can't do that
        // until we confirm that we are actually the restarter.
        if internal
            .strong_count
            .fetch_update(Ordering::Relaxed, Ordering::Acquire, |v| {
                if v == 0 { None } else { Some(v + 1) }
            })
            .is_ok()
        {
            let prev_generation = internal.current_generation.load(Ordering::Relaxed);
            // Something already restarted it.
            return DiceTaskDependedOnByResult::Pending(DicePromise::pending(
                DiceTaskDependentFuture {
                    task: self,
                    completed: false,
                    generation: prev_generation,
                },
            ));
        }

        // We are the restarter.
        let prev_generation = internal.current_generation.load(Ordering::Relaxed);
        let new_generation = prev_generation + 1;
        self.internal
            .current_generation
            .store(new_generation, Ordering::Relaxed);

        // Build new cancellation infrastructure for the next generation.
        let (future_spawner, cancellation_handle) = prepare_detached_cancellation();
        let cancellation_handle = cancellation_handle.into_dropcancel();
        guard.as_mut().unwrap().cancellation_handle = cancellation_handle;

        // Setting this allows other calls to follow the fast path. We could probably do this just after
        // updating current_generation, but it's a bit easier to reason about if we finish all our updates first.
        self.internal.strong_count.store(1, Ordering::Release);

        drop(guard);

        let previously_cancelled = PreviouslyCancelledTask::new(TerminationObserver::Pending {
            task: self.clone_arc(),
            generation: prev_generation,
        });

        let prepared = PreparedDiceTask {
            task_spawner: DiceTaskSpawner {
                inner: future_spawner,
            },
            dependent_future: DiceTaskDependentFuture {
                task: self,
                completed: false,
                generation: new_generation,
            },
            completion_handle: DiceTaskCompletionHandle {
                generation: new_generation,
                task: self.clone_arc(),
            },
        };

        DiceTaskDependedOnByResult::NeedsRestart(prepared, previously_cancelled)
    }

    pub(crate) fn get_finished_value(self) -> Option<&'d DiceComputedValue> {
        match self.internal.get().read_value() {
            ReadValueResult::Finished(v) => Some(v),
            ReadValueResult::Pending { .. } => None,
        }
    }

    #[cfg(test)]
    pub(crate) fn waiters_count(&self) -> u32 {
        self.internal.strong_count.load(Ordering::Relaxed)
    }

    /// Returns whether the task may have any outstanding work.
    pub(crate) fn is_pending(&self) -> bool {
        match self.internal.read_value() {
            ReadValueResult::Finished(_) => false,
            ReadValueResult::Pending {
                terminated_generation,
            } => terminated_generation < self.internal.current_generation.load(Ordering::Relaxed),
        }
    }

    pub(crate) fn introspect_state(&self) -> DiceTaskState {
        match self.internal.read_value() {
            ReadValueResult::Finished(_) => DiceTaskState::Ready,
            ReadValueResult::Pending {
                terminated_generation,
            } if terminated_generation
                < self.internal.current_generation.load(Ordering::Relaxed) =>
            {
                DiceTaskState::InProgress
            }
            ReadValueResult::Pending { .. } => DiceTaskState::Terminated,
        }
    }

    pub(crate) fn cancel(&self, reason: CancellationReason) {
        let guard = self.internal.critical.lock();
        *self.internal.last_cancellation_reason.lock() = Some(reason);
        GlobalStats::record_cancellation();
        if let Some(critical) = guard.as_ref() {
            critical.cancellation_handle.cancel();
        }
    }

    #[cfg(test)]
    pub(crate) fn is_ready(&self) -> bool {
        match self.internal.read_value() {
            ReadValueResult::Finished(_) => true,
            ReadValueResult::Pending { .. } => false,
        }
    }

    #[cfg(test)]
    pub(crate) fn is_cancelled(&self) -> bool {
        // Three states (per the generation invariant):
        //   * Finished:  terminated_generation == 0
        //   * Pending:   terminated_generation == current_generation - 1
        //   * Cancelled: terminated_generation == current_generation
        // Use read_value() (which returns Pending(term)) and compare against
        // current. >= is defensive in case ordering ever briefly violates the
        // invariant.
        match self.internal.read_value() {
            ReadValueResult::Finished(_) => false,
            ReadValueResult::Pending {
                terminated_generation,
            } => terminated_generation >= self.internal.current_generation.load(Ordering::Relaxed),
        }
    }

    /// Returns a future that resolves when this task finishes or is fully cancelled
    /// and terminated. Dropping the returned `TerminationObserver` does NOT cancel the
    /// task — observers are passive watchers.
    ///
    /// Used for:
    /// - Tests: observing when a cancelled task has fully terminated.
    /// - Task restart: a restarted task awaits termination of its previous computation
    ///   before proceeding.
    /// - `wait_for_idle`: DICE collects pending tasks from core state and awaits their
    ///   termination observers to ensure all in-flight work has settled.
    pub(crate) fn await_termination(&self) -> TerminationObserver {
        match self.internal.read_value() {
            ReadValueResult::Finished(v) => TerminationObserver::Done(Some(v.dupe())),
            ReadValueResult::Pending { .. } => {
                let generation = self.internal.current_generation.load(Ordering::Relaxed);
                TerminationObserver::Pending {
                    task: self.clone_arc(),
                    generation,
                }
            }
        }
    }

    /// Synchronously get the value of this task, or compute it via a sync projection.
    ///
    /// This encapsulates the entire sync projection protocol:
    /// 1. Check if the task already has a completed value
    /// 2. Check if a sync projection value already exists
    /// 3. Compute the sync value under write lock
    /// 4. Spawn a background task to complete the async part
    pub(crate) fn sync_get_or_complete(
        &self,
        prevent_cancellation_future: DiceTaskDependentFuture<'_>,
        f: impl FnOnce() -> DiceSyncResult,
    ) -> CancellableResult<DiceComputedValue> {
        // 1. Already finished?
        if let Some(v) = self.get_finished_value() {
            return Ok(v.dupe());
        }

        let result = {
            let mut guard = self.internal.critical.lock();
            // May have raced, so need to check again
            if let Some(v) = self.get_finished_value() {
                return Ok(v.dupe());
            }

            // Not finished (checked above, under the lock), so the task isn't sealed and `Critical`
            // is still present.
            let critical = guard
                .as_mut()
                .expect("Critical is only dropped when the task is sealed");

            // 2. Sync value already computed by a previous call?
            if let Some(sync_res) = &critical.sync_value {
                return Ok(sync_res.dupe());
            }

            // 3. Compute under lock.
            let result = f();
            critical.sync_value = Some(result.sync_result.dupe());
            result
        };

        // 4. Spawn the async completion.
        //
        // Because this is async and we don't really keep track of it, things would get very messy
        // if we started attempting to reason about cancellation here; for that reason, we avoid it.
        // Once we get to this point, we commit to ensuring that this generation will complete with
        // success. We do so by just bumping the strong count and "leaking" a waiter.
        self.internal.strong_count.fetch_add(1, Ordering::Relaxed);

        // Note that we know that this increment does not change the strong count 0 -> 1 because -
        // until here - we have a `prevent_cancellation_future` which itself holds a strong count
        let task = prevent_cancellation_future.task.clone_arc();
        let generation = prevent_cancellation_future.generation;
        drop(prevent_cancellation_future);
        tokio::spawn({
            let future = result.state_future;
            async move {
                let res = future.await;
                // Clear sync_value now that the async result is in. If the task has already been
                // sealed (by another generation), `Critical` is gone and there is nothing to clear.
                let mut guard = task.internal.critical.lock();
                if let Some(critical) = guard.as_mut() {
                    critical.sync_value = None;
                }
                // Finalize the generation in the standard way
                //
                // FIXME(JakobDegen): `DiceCompletionHandle` is supposed to be the only way to do
                // this, so it's not a great sign that we're not using it. We also can't just have
                // the caller pass it in - the waiter that actually gets the `DiceCompletionHandle`
                // might not have been the one that wins the race to the lock earlier. What happens
                // right now is that the `DiceCompletionHandle` just gets discarded which works
                // correctly, but if that ever changes things will go very wrong here. We should
                // probably have different APIs to avoid handing one out in the sync case.
                match res {
                    Ok(v) => {
                        task.as_ref().task_finished_success(guard, generation, v);
                    }
                    Err(e) => {
                        task.as_ref().task_finished_cancelled(guard, generation, e);
                    }
                }
            }
        });

        Ok(result.sync_result)
    }

    fn task_finished<'a>(
        &'_ self,
        guard: parking_lot::MutexGuard<'a, Option<Box<Critical>>>,
        generation: u32,
    ) -> Option<parking_lot::MutexGuard<'a, Option<Box<Critical>>>> {
        let internal = &*self.internal;
        let last_terminated = internal.terminated_generation.load(Ordering::Relaxed);
        if last_terminated == 0 {
            // Everything was sealed earlier, so at this point it's completely safe to ignore this
            return None;
        }
        if last_terminated + 1 != generation {
            // Otherwise, we verify that the generations are cancelled in order, not out of order.
            // To see why this is locally important for correctness, consider what happens if 9
            // reports completion before 8. 9 must have been cancelled for that to be possible, but
            // we would not be allowed to mark it as terminated, since that would imply that 8
            // finished too. But when 8 eventually does come back as fully cancelled, we won't know
            // to mark 9 as cancelled too, and everything will stall.
            //
            // FIXME(JakobDegen): It's a very non-local property that this is ok; in practice the
            // worker disables cancellations at the right times to make things work out, but it'd be
            // good if that were a lot more obvious.
            panic!("Generations did not terminate in order");
        }
        Some(guard)
    }

    /// Mark that a generation has terminated successfully.
    ///
    /// Calling this on a subsequent generation after a previous one succeeded is tolerated.
    fn task_finished_success(
        &self,
        guard: parking_lot::MutexGuard<'_, Option<Box<Critical>>>,
        generation: u32,
        value: DiceComputedValue,
    ) {
        let internal = &*self.internal;
        let Some(mut guard) = self.task_finished(guard, generation) else {
            return;
        };
        unsafe { *internal.maybe_value.get() = Some(value) };
        internal.terminated_generation.store(0, Ordering::Release);
        internal.wakers.close();
        // The task is now sealed for good: the cancellation handle and sync-projection slot in
        // `Critical` can never be needed again. Sealed tasks live in the cache indefinitely, so drop
        // the `Box<Critical>` to reclaim that memory.
        *guard = None;
    }

    /// Mark that a generation has terminated in cancellation.
    fn task_finished_cancelled(
        &self,
        guard: parking_lot::MutexGuard<'_, Option<Box<Critical>>>,
        generation: u32,
        reason: CancellationReason,
    ) {
        let internal = &*self.internal;
        let maybe_guard = self.task_finished(guard, generation);
        if maybe_guard.is_none() {
            return;
        }

        if reason != CancellationReason::HandleDropped {
            // In the handle dropped case it was probably dropped because of a cancellation reason
            // already stored here.
            //
            // FIXME(JakobDegen): Messy.
            *internal.last_cancellation_reason.lock() = Some(reason);
        }

        internal
            .terminated_generation
            .store(generation, Ordering::Relaxed);
        internal.wakers.drain();
    }

    /// Returns the state of the given generation.
    fn state_at(&self, generation: u32) -> StateAtGeneration<'_> {
        // FIXME(JakobDegen): Doesn't really do what it says in the comment, since if a generation
        // after the provided one finished, it'll report that it finished.
        match self.internal.read_value() {
            ReadValueResult::Finished(v) => StateAtGeneration::TerminatedSuccess(v),
            ReadValueResult::Pending {
                terminated_generation,
            } => {
                if terminated_generation >= generation {
                    StateAtGeneration::TerminatedCancelled
                } else {
                    StateAtGeneration::Pending
                }
            }
        }
    }

    /// Drop one dependent.
    fn drop_waiter(&self) {
        // Fast path: decrement strong_count if > 1.
        if self
            .internal
            .strong_count
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |v| {
                if v == 1 { None } else { Some(v - 1) }
            })
            .is_ok()
        {
            return;
        }

        // Slow path: we appear to be the last dependent.
        let guard = self.internal.critical.lock();
        let prev = self.internal.strong_count.fetch_sub(1, Ordering::Relaxed);
        if prev != 1 {
            // A new dependent registered.
            return;
        }

        // We were the last dependent. Cancel the spawned task — unless the task is already sealed
        // (`Critical` dropped), in which case there is nothing running to cancel.
        if let Some(critical) = guard.as_ref() {
            GlobalStats::record_cancellation();
            critical.cancellation_handle.cancel();
        }
    }
}

impl DiceTaskInternal {
    pub(crate) fn read_value(&self) -> ReadValueResult<'_> {
        match self.terminated_generation.load(Ordering::Acquire) {
            0 => ReadValueResult::Finished(
                // SAFETY: `Acquire` load of `0` synchronizes with the op that wrote this, as per
                // the invariant on this
                unsafe { &*self.maybe_value.get() }.as_ref().unwrap(),
            ),
            terminated_generation => ReadValueResult::Pending {
                terminated_generation,
            },
        }
    }
}

pub(crate) fn spawn_prepared_task<'d, S>(
    task: PreparedDiceTask<'d>,
    spawner: &dyn dice_futures::spawner::Spawner<S>,
    ctx: &S,
    f: impl for<'a> FnOnce(&'a mut DiceTaskHandle) -> BoxFuture<'a, ()> + Send,
) -> DicePromise<'d> {
    use dice_futures::owning_future::OwningFuture;
    let PreparedDiceTask {
        completion_handle,
        dependent_future,
        task_spawner,
    } = task;

    task_spawner.spawn_cancellable(
        |cancellations| {
            let handle = DiceTaskHandle::new(completion_handle, cancellations);
            OwningFuture::new(handle, f).boxed()
        },
        spawner,
        ctx,
    );

    DicePromise::pending(dependent_future)
}

#[cfg(test)]
pub(crate) fn spawn_dice_task<S>(
    key: DiceKey,
    spawner: &dyn dice_futures::spawner::Spawner<S>,
    ctx: &S,
    f: impl for<'a> FnOnce(
        &'a mut DiceTaskHandle,
    ) -> futures::future::BoxFuture<'a, Box<dyn std::any::Any + Send>>
    + Send
    + 'static,
) -> DiceTask {
    let prepared_task = DiceTask::prepare_testing(key);
    let task = prepared_task.task().clone_arc();
    let promise = spawn_prepared_task(prepared_task, spawner, ctx, |handle| {
        async move {
            let _ignored = f(handle).await;
        }
        .boxed()
    });
    std::mem::forget(promise);
    task
}

pub struct DiceTaskSpawner {
    pub(crate) inner: CancellableFutureSpawner,
}

impl DiceTaskSpawner {
    pub(crate) fn spawn_cancellable<S>(
        self,
        f: impl for<'a> FnOnce(&'a CancellationContext) -> BoxFuture<'a, ()> + Send,
        spawner: &dyn Spawner<S>,
        ctx: &S,
    ) {
        self.inner.spawn(f, spawner, ctx);
    }
}

/// The sole right to finalize one generation of a `DiceTask`. Held by the spawned worker (inside
/// `DiceTaskHandle`) or by an in-flight sync projection. Finalization is generation-checked: if the
/// task has since been restarted, `completed`/`terminated` for this stale generation are no-ops, so
/// a slow or duplicate completion can't clobber a newer computation.
pub(crate) struct DiceTaskCompletionHandle {
    /// The generation this handle is allowed to finalize.
    pub(super) generation: u32,
    pub(super) task: DiceTask,
}

impl DiceTaskCompletionHandle {
    pub(crate) fn terminated(self, reason: CancellationReason) {
        self.task.as_ref().task_finished_cancelled(
            self.task.internal.critical.lock(),
            self.generation,
            reason,
        );
    }

    pub(crate) fn completed(self, v: DiceComputedValue) {
        self.task.as_ref().task_finished_success(
            self.task.internal.critical.lock(),
            self.generation,
            v,
        );
    }
}

impl Drop for DiceTaskCompletionHandle {
    fn drop(&mut self) {
        // Intentionally does nothing, and this is important because of the FIXME in the sync case
        // below
    }
}

/// A dependent's view of an in-flight `DiceTask`, pollable for its result. Each live instance
/// corresponds to one unit of `strong_count`; its drop decrements via `drop_waiter`, which is how a
/// task notices when its last dependent is gone. Resolves to the value when the task finishes, or
/// to a cancellation error once `terminated_generation` reaches the generation it is watching.
pub(crate) struct DiceTaskDependentFuture<'d> {
    task: DiceTaskRef<'d>,
    /// The generation whose outcome this future is waiting for.
    generation: u32,
    completed: bool,
}

impl<'d> DiceTaskDependentFuture<'d> {
    pub(crate) fn try_get(&self) -> Option<CancellableResult<&'d DiceComputedValue>> {
        match self.task.internal.get().read_value() {
            ReadValueResult::Finished(v) => Some(Ok(v)),
            ReadValueResult::Pending {
                terminated_generation,
            } if terminated_generation >= self.generation => {
                let r = *self.task.internal.last_cancellation_reason.lock();
                // FIXME(JakobDegen): Probably shouldn't have a default here
                Some(Err(r.unwrap_or(CancellationReason::HandleDropped)))
            }
            _ => None,
        }
    }

    pub(crate) fn task(&self) -> DiceTaskRef<'d> {
        self.task
    }
}

impl<'d> Drop for DiceTaskDependentFuture<'d> {
    fn drop(&mut self) {
        self.task.drop_waiter();
    }
}

impl<'d> Future for DiceTaskDependentFuture<'d> {
    type Output = CancellableResult<&'d DiceComputedValue>;

    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        if let Some(v) = self.try_get() {
            self.completed = true;
            return Poll::Ready(v);
        }

        // Register our waker, then re-check. The task may have terminated between the check above
        // and this registration, and the `drain`/`close` that would have woken us may already have
        // fired — so we must report `Ready` ourselves rather than rely on a missed wakeup. The
        // re-check is correctly synchronized because `push` acquires from whatever last reset or
        // closed the list (see `SimpleAtomicList`); the `push` return value is therefore irrelevant.
        self.task.internal.wakers.push(cx.waker().clone());
        match self.try_get() {
            Some(v) => {
                self.completed = true;
                Poll::Ready(v)
            }
            None => Poll::Pending,
        }
    }
}

#[cfg(test)]
pub(crate) mod testing_helpers {
    use crate::api::key::Key;
    use crate::impls::key::DiceKey;
    use crate::impls::task::dice::DiceTask;
    use crate::impls::value::DiceComputedValue;
    use crate::impls::value::DiceKeyValue;
    use crate::impls::value::DiceValidValue;
    use crate::impls::value::MaybeValidDiceValue;
    use crate::impls::value::TrackedInvalidationPaths;
    use crate::versions::VersionRanges;

    pub(crate) fn make_completed_task<K: Key>(key: DiceKey, val: K::Value) -> DiceTask {
        make_completed_task_with_computed_value(
            key,
            DiceComputedValue::new(
                MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(
                    val,
                ))),
                crate::arc::Arc::new(VersionRanges::new()),
                TrackedInvalidationPaths::clean(),
            ),
        )
    }

    pub(crate) fn make_completed_task_with_computed_value(
        key: DiceKey,
        val: DiceComputedValue,
    ) -> DiceTask {
        let prepared = DiceTask::prepare_testing(key);
        let task = prepared.task().clone_arc();
        prepared.completion_handle.completed(val);
        task
    }
}
