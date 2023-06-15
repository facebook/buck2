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
use tracing::Instrument;

use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::incremental::IncrementalEngine;
use crate::impls::key::DiceKey;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::spawn_dice_task;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::worker::state::DiceWorkerStateAwaitingPrevious;
use crate::impls::worker::state::DiceWorkerStateFinishedAndCached;
use crate::result::CancellableResult;
use crate::result::Cancelled;

pub(crate) mod state;

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
                        .do_work(DiceWorkerStateAwaitingPrevious::new(handle))
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
            previous.previous.await_termination().await;
            // old task actually finished, so just use that result if it wasn't
            // cancelled

            match previous
                .previous
                .get_finished_value()
                .expect("Terminated task must have finished value")
            {
                Ok(res) => {
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

        self.incremental
            .eval_entry_versioned(
                self.k,
                &self.eval,
                self.cycles,
                self.events_dispatcher,
                state,
            )
            .await
    }
}
