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
use itertools::Either;
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
use crate::impls::worker::state::DiceWorkerStateLookupNode;
use crate::result::CancellableResult;

pub(crate) mod state;

/// The worker on the spawned dice task
pub(crate) struct DiceTaskWorker {
    k: DiceKey,
    eval: AsyncEvaluator,
    events_dispatcher: DiceEventDispatcher,
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

        let worker = DiceTaskWorker::new(k, eval, events_dispatcher, incremental);

        spawn_dice_task(k, &*spawner, &spawner_ctx, move |handle| {
            // NOTE: important to run prevent cancellation eagerly in the sync scope to prevent
            // cancellations so that we don't cancel the current task before we finish waiting
            // for the previously cancelled task
            let prevent_cancellation = handle.cancellation_ctx().begin_ignore_cancellation();
            let state =
                DiceWorkerStateAwaitingPrevious::new(k, cycles, handle, prevent_cancellation);

            async move {
                let previous_result = match previously_cancelled_task {
                    Some(v) => state.await_previous(v).await,
                    None => Either::Right(state.no_previous_task().await),
                };

                match previous_result {
                    Either::Left(_) => {
                        // previous result actually finished
                    }
                    Either::Right(state) => {
                        let _ignore = worker.do_work(state).await;
                    }
                }

                Box::new(()) as Box<dyn Any + Send + 'static>
            }
            .instrument(span)
            .boxed()
        })
    }

    fn new(
        k: DiceKey,
        eval: AsyncEvaluator,
        events_dispatcher: DiceEventDispatcher,
        incremental: IncrementalEngine,
    ) -> Self {
        Self {
            k,
            eval,
            events_dispatcher,
            incremental,
        }
    }

    pub(crate) async fn do_work(
        self,
        state: DiceWorkerStateLookupNode<'_, '_>,
    ) -> CancellableResult<DiceWorkerStateFinishedAndCached> {
        self.incremental
            .eval_entry_versioned(self.k, &self.eval, self.events_dispatcher, state)
            .await
    }
}
