/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Handle to the DiceTask as seen by the thread responsible for completing the task

use dice_futures::cancellation::CancellationContext;

use crate::epoch::cache::TransactionResult;
use crate::epoch::task::dice::DiceTaskCompletionHandle;
use crate::epoch::worker::WorkerCancelled;
use crate::epoch::worker::WorkerResult;
use crate::value::DiceComputedValue;

/// The handle to the 'DiceTask' owned by the spawned thread that is responsible for completing
/// the task.
pub(crate) struct DiceTaskHandle<'a> {
    pub(super) cancellations: &'a CancellationContext,
    completion_handle: Option<DiceTaskCompletionHandle>,
    // holds the result while `DiceTaskHandle` is not dropped, then upon drop, stores it into
    // `DiceTaskInternal` so that the result is always reported consistently at the very end of the task.
    result: Option<WorkerResult<TransactionResult<DiceComputedValue>>>,
}

impl<'a> DiceTaskHandle<'a> {
    pub(super) fn new(
        completion_handle: DiceTaskCompletionHandle,
        cancellations: &'a CancellationContext,
    ) -> Self {
        Self {
            cancellations,
            completion_handle: Some(completion_handle),
            result: None,
        }
    }

    pub(crate) fn cancellation_ctx(&self) -> &'a CancellationContext {
        self.cancellations
    }

    pub(crate) fn finished(&mut self, value: WorkerResult<TransactionResult<DiceComputedValue>>) {
        self.result = Some(value);
    }
}

unsafe impl Send for DiceTaskHandle<'_> {}

impl Drop for DiceTaskHandle<'_> {
    fn drop(&mut self) {
        let completion_handle = self.completion_handle.take().unwrap();
        match self.result.take() {
            Some(Ok(v)) => {
                completion_handle.completed(v);
            }
            Some(Err(token)) => {
                completion_handle.cancelled(token);
            }
            None => {
                completion_handle.cancelled(WorkerCancelled);
            }
        }
    }
}
