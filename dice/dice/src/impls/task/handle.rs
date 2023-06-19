/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Handle to the DiceTask as seen by the thread responsible for completing the task

use more_futures::cancellation::CancellationContext;

use crate::arc::Arc;
use crate::impls::task::dice::DiceTaskInternal;
use crate::impls::value::DiceComputedValue;

/// The handle to the 'DiceTask' owned by the spawned thread that is responsible for completing
/// the task.
pub(crate) struct DiceTaskHandle<'a> {
    pub(super) internal: Arc<DiceTaskInternal>,
    pub(super) cancellations: &'a CancellationContext,
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
    pub(crate) fn report_initial_lookup(&self) -> TaskState {
        self.internal.state.report_initial_lookup()
    }

    pub(crate) fn checking_deps(&self) -> TaskState {
        self.internal.state.report_checking_deps()
    }

    pub(crate) fn computing(&self) -> TaskState {
        self.internal.state.report_computing()
    }

    pub(crate) fn finished(self, value: DiceComputedValue) {
        let _ignore = self.internal.set_value(value);
    }

    pub(crate) fn cancellation_ctx(&self) -> &CancellationContext {
        &self.cancellations
    }

    #[cfg(test)]
    pub(crate) fn testing_new() -> &'static DiceTaskHandle<'static> {
        static TEST: once_cell::sync::Lazy<DiceTaskHandle> =
            once_cell::sync::Lazy::new(|| DiceTaskHandle::<'static> {
                internal: DiceTaskInternal::new(crate::impls::key::DiceKey { index: 99999 }),
                cancellations: CancellationContext::testing(),
            });

        &TEST
    }
}

impl<'a> Drop for DiceTaskHandle<'a> {
    fn drop(&mut self) {
        if self.internal.read_value().is_none() {
            debug!("task is terminated");

            // This is only owned by the main worker task. If this was dropped, and no result was
            // ever recorded, then we must have been terminated.
            self.internal.report_terminated()
        }
    }
}

unsafe impl<'a> Send for DiceTaskHandle<'a> {}
