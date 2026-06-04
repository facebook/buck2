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

use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dice_futures::cancellation::CancellationContext;

use crate::impls::task::dice::DiceTask;
use crate::impls::value::DiceComputedValue;

/// The handle to the 'DiceTask' owned by the spawned thread that is responsible for completing
/// the task.
pub(crate) struct DiceTaskHandle<'a> {
    pub(super) task: DiceTask,
    pub(super) cancellations: &'a CancellationContext,
    // holds the result while `DiceTaskHandle` is not dropped, then upon drop, stores it into
    // `DiceTaskInternal` so that the result is always reported consistently at the very end of the task.
    result: Option<CancellableResult<DiceComputedValue>>,
}

/// After reporting that we are about to transition to a state, should we continue processing or
/// should we terminate
pub(crate) enum TaskState {
    /// continue processing as normal
    Continue,
    /// task was finished already
    Finished,
}

impl<'a> DiceTaskHandle<'a> {
    pub(super) fn new(task: DiceTask, cancellations: &'a CancellationContext) -> Self {
        Self {
            task,
            cancellations,
            result: None,
        }
    }

    pub(crate) fn cancellation_ctx(&self) -> &'a CancellationContext {
        self.cancellations
    }

    pub(crate) fn finished(&mut self, value: DiceComputedValue) {
        self.result = Some(Ok(value));
    }

    pub(crate) fn cancelled(&mut self, reason: CancellationReason) {
        self.result = Some(Err(reason));
    }
}

unsafe impl Send for DiceTaskHandle<'_> {}

impl Drop for DiceTaskHandle<'_> {
    fn drop(&mut self) {
        match self.result.take() {
            Some(Ok(v)) => {
                debug!("{:?} finished. Notifying result", self.task.key());
                let _ignore = self.task.set_value(v);
            }
            Some(Err(reason)) => {
                debug!("{:?} cancelled. Notifying cancellation", self.task.key());
                self.task.report_terminated(reason);
            }
            None => {
                debug!(
                    "{:?} dropped without result. Notifying cancellation",
                    self.task.key()
                );
                self.task
                    .report_terminated(CancellationReason::HandleDropped);
            }
        }
    }
}
