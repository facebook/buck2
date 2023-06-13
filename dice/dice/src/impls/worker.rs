/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The main worker thread for the dice task

use more_futures::cancellable_future::DisableCancellationGuard;

use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::incremental::IncrementalEngine;
use crate::impls::key::DiceKey;
use crate::impls::task::handle::DiceTaskHandle;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::result::CancellableResult;

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
    pub(crate) fn new(
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
        handle: &DiceTaskHandle<'_>,
    ) -> CancellableResult<(DiceComputedValue, Option<DisableCancellationGuard>)> {
        if let Some(previous) = self.previously_cancelled_task {
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

                    return Ok((res, None));
                }
                Err(_err) => {
                    // actually was cancelled, so just continue re-evaluating
                }
            }
        }

        let result = self
            .incremental
            .eval_entry_versioned(
                self.k,
                self.eval,
                self.cycles,
                self.events_dispatcher,
                handle,
            )
            .await;

        debug!("finished versioned evaluation");

        result
    }
}
