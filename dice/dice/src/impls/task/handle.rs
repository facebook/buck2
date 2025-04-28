/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Handle to the DiceTask as seen by the thread responsible for completing the task

use buck2_futures::cancellation::CancellationContext;
use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dupe::Dupe;

use crate::arc::Arc;
use crate::impls::task::dice::DiceTaskInternal;
use crate::impls::value::DiceComputedValue;

/// The handle to the 'DiceTask' owned by the spawned thread that is responsible for completing
/// the task.
pub(crate) struct DiceTaskHandle<'a> {
    pub(super) internal: Arc<DiceTaskInternal>,
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
    pub(super) fn new(
        internal: Arc<DiceTaskInternal>,
        cancellations: &'a CancellationContext,
    ) -> Self {
        Self {
            internal: internal.dupe(),
            cancellations,
            result: None,
        }
    }

    pub(crate) fn report_initial_lookup(&self) -> TaskState {
        self.internal.state.report_initial_lookup()
    }

    pub(crate) fn checking_deps(&self) -> TaskState {
        self.internal.state.report_checking_deps()
    }

    pub(crate) fn computing(&self) -> TaskState {
        self.internal.state.report_computing()
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

unsafe impl<'a> Send for DiceTaskHandle<'a> {}

impl Drop for DiceTaskHandle<'_> {
    fn drop(&mut self) {
        match self.result.take() {
            Some(Ok(v)) => {
                debug!("{:?} finished. Notifying result", self.internal.key);
                let _ignore = self.internal.set_value(v);
            }
            Some(Err(reason)) => {
                debug!("{:?} cancelled. Notifying cancellation", self.internal.key);
                self.internal.report_terminated(reason);
            }
            None => {
                debug!(
                    "{:?} dropped without result. Notifying cancellation",
                    self.internal.key
                );
                self.internal
                    .report_terminated(CancellationReason::HandleDropped);
            }
        }
    }
}
