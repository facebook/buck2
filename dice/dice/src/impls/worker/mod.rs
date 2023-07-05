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
use crate::impls::task::handle::DiceTaskHandle;
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

        let spawner = eval.user_data.spawner.dupe();
        let spawner_ctx = eval.user_data.dupe();

        let worker = DiceTaskWorker::new(
            k,
            eval,
            cycles,
            events_dispatcher,
            previously_cancelled_task,
            incremental,
        );

        spawn_dice_task(k, &*spawner, &spawner_ctx, move |handle| {
            async move {
                // we hold onto the handle and drop it last after consuming the `worker`. This
                // ensures any data being held for the actual evaluation is dropped before we
                // notify the future as done.
                let _ignored = worker.do_work(handle).await;

                Box::new(()) as Box<dyn Any + Send + 'static>
            }
            .instrument(span)
            .boxed()
        })
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
        handle: &mut DiceTaskHandle<'_>,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        let state = DiceWorkerStateAwaitingPrevious::new(self.k, self.cycles, handle);

        let state = if let Some(previous) = self.previously_cancelled_task {
            state
                .internals
                .cancellation_ctx()
                .critical_section(|| previous.previous.await_termination())
                .await;

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
            .eval_entry_versioned(self.k, &self.eval, self.events_dispatcher, state)
            .await
    }
}
