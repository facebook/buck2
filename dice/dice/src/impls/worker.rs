/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The main worker thread for the dice task

use std::any::Any;

use dupe::Dupe;
use futures::FutureExt;
use more_futures::cancellation::CancellationContext;
use tracing::Instrument;

use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::incremental::IncrementalEngine;
use crate::impls::key::DiceKey;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::handle::DiceTaskHandle;
use crate::impls::task::spawn_dice_task;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::result::CancellableResult;
use crate::result::Cancelled;

/// Represents when we are in a spawned dice task worker and are currently waiting for the previous
/// cancelled instance of this task to finish cancelling.
pub(crate) struct DiceWorkerStateAwaitingPrevious<'a> {
    internals: DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateAwaitingPrevious<'a> {
    pub(crate) fn previously_finished(
        self,
        value: DiceComputedValue,
    ) -> DiceWorkerStateFinishedAndCached {
        self.internals.finished(value);

        DiceWorkerStateFinishedAndCached {}
    }

    pub(crate) fn previously_cancelled(self) -> DiceWorkerStateLookupNode<'a> {
        self.internals.report_initial_lookup();

        DiceWorkerStateLookupNode {
            internals: self.internals,
        }
    }

    pub(crate) fn no_previous_task(self) -> DiceWorkerStateLookupNode<'a> {
        self.internals.report_initial_lookup();

        DiceWorkerStateLookupNode {
            internals: self.internals,
        }
    }
}

/// Represents when we are currently looking up the current requested key from the core state, and
/// are waiting for it to respond.
pub(crate) struct DiceWorkerStateLookupNode<'a> {
    internals: DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateLookupNode<'a> {
    pub(crate) fn checking_deps(self) -> DiceWorkerStateCheckingDeps<'a> {
        self.internals.checking_deps();

        DiceWorkerStateCheckingDeps {
            internals: self.internals,
        }
    }

    pub(crate) fn lookup_dirtied(self) -> DiceWorkerStateComputing<'a> {
        self.internals.computing();

        DiceWorkerStateComputing {
            internals: self.internals,
        }
    }

    pub(crate) fn lookup_matches(
        self,
        value: DiceComputedValue,
    ) -> DiceWorkerStateFinishedAndCached {
        self.internals.finished(value);

        DiceWorkerStateFinishedAndCached {}
    }
}

/// When the spawned dice task worker is checking if the dependencies have changed since the last
/// time this node was verified, and are waiting for the results of the dependency re-computation.
pub(crate) struct DiceWorkerStateCheckingDeps<'a> {
    internals: DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateCheckingDeps<'a> {
    pub(crate) fn deps_not_match(self) -> DiceWorkerStateComputing<'a> {
        self.internals.computing();

        DiceWorkerStateComputing {
            internals: self.internals,
        }
    }

    pub(crate) fn deps_match(self) -> DiceWorkerStateFinished<'a> {
        DiceWorkerStateFinished {
            internals: self.internals,
        }
    }

    #[cfg(test)]
    pub(crate) fn testing(k: DiceKey) -> Self {
        DiceWorkerStateCheckingDeps {
            internals: DiceTaskHandle::testing_new(k),
        }
    }
}

/// When the spawned dice worker is currently computing the requested Key.
pub(crate) struct DiceWorkerStateComputing<'a> {
    internals: DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateComputing<'a> {
    pub(crate) fn cancellation_ctx(&self) -> &CancellationContext {
        self.internals.cancellation_ctx()
    }

    pub(crate) fn finished(self) -> DiceWorkerStateFinished<'a> {
        DiceWorkerStateFinished {
            internals: self.internals,
        }
    }
}

/// When the spawned dice worker is finished checking dependencies or finished computing the key.
/// At this point, the value of the node is known. We are just waiting for core state to finish
/// updating the caches and return the correct instance of the value.
pub(crate) struct DiceWorkerStateFinished<'a> {
    internals: DiceTaskHandle<'a>,
}

impl<'a> DiceWorkerStateFinished<'a> {
    pub(crate) fn cached(self, value: DiceComputedValue) -> DiceWorkerStateFinishedAndCached {
        self.internals.finished(value);

        DiceWorkerStateFinishedAndCached {}
    }
}

/// When the spawned dice worker is done computing and saving the value to core state cache.
/// The final value is known.
pub(crate) struct DiceWorkerStateFinishedAndCached {}

/// The worker on the spawned dice task
pub(crate) struct DiceTaskWorker {
    k: DiceKey,
    eval: AsyncEvaluator,
    cycles: UserCycleDetectorData,
    events_dispatcher: DiceEventDispatcher,
    previously_cancelled_task: Option<PreviouslyCancelledTask>,
    incremental: IncrementalEngine,
}

impl DiceTaskWorker {
    pub(crate) fn spawn(
        k: DiceKey,
        eval: AsyncEvaluator,
        cycles: UserCycleDetectorData,
        events_dispatcher: DiceEventDispatcher,
        previously_cancelled_task: Option<PreviouslyCancelledTask>,
        incremental: IncrementalEngine,
    ) -> DiceTask {
        let span = debug_span!(parent: None, "spawned_dice_task", k = ?k, v = %eval.per_live_version_ctx.get_version(), v_epoch = %incremental.version_epoch);
        spawn_dice_task(
            k,
            &*eval.user_data.spawner.dupe(),
            &eval.user_data.dupe(),
            move |handle| {
                async move {
                    let worker = DiceTaskWorker::new(
                        k,
                        eval,
                        cycles,
                        events_dispatcher,
                        previously_cancelled_task,
                        incremental,
                    );

                    match worker
                        .do_work(DiceWorkerStateAwaitingPrevious { internals: handle })
                        .await
                    {
                        Ok(_res) => {
                            // finished and cached.
                        }
                        Err(Cancelled) => {
                            // we drop the current handle, leaving the original `DiceTask` as terminated
                            // state
                        }
                    }

                    Box::new(()) as Box<dyn Any + Send + 'static>
                }
                .instrument(span)
                .boxed()
            },
        )
    }

    fn new(
        k: DiceKey,
        eval: AsyncEvaluator,
        cycles: UserCycleDetectorData,
        events_dispatcher: DiceEventDispatcher,
        previously_cancelled_task: Option<PreviouslyCancelledTask>,
        incremental: IncrementalEngine,
    ) -> Self {
        Self {
            k,
            eval,
            cycles,
            events_dispatcher,
            previously_cancelled_task,
            incremental,
        }
    }

    pub(crate) async fn do_work(
        self,
        state: DiceWorkerStateAwaitingPrevious<'_>,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        let state = if let Some(previous) = self.previously_cancelled_task {
            debug!(msg = "waiting for previously cancelled task");
            previous.previous.await_termination().await;
            // old task actually finished, so just use that result if it wasn't
            // cancelled

            match previous
                .previous
                .get_finished_value()
                .expect("Terminated task must have finished value")
            {
                Ok(res) => {
                    debug!(msg = "previously cancelled task actually finished");

                    return Ok(state.previously_finished(res));
                }
                Err(Cancelled) => {
                    // actually was cancelled, so just continue re-evaluating
                }
            }

            state.previously_cancelled()
        } else {
            state.no_previous_task()
        };

        let result = self
            .incremental
            .eval_entry_versioned(
                self.k,
                self.eval,
                self.cycles,
                self.events_dispatcher,
                state,
            )
            .await;

        debug!("finished versioned evaluation");

        result
    }
}
