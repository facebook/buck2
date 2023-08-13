/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Handle to the DiceTask as seen by the thread responsible for completing the task

use dupe::Dupe;
use more_futures::cancellation::ExplicitCancellationContext;

use crate::arc::Arc;
use crate::impls::task::dice::DiceTaskInternal;
use crate::impls::value::DiceComputedValue;

/// The handle to the 'DiceTask' owned by the spawned thread that is responsible for completing
/// the task.
pub(crate) struct DiceTaskHandle<'a> {
    pub(super) internal: Arc<DiceTaskInternal>,
    pub(super) cancellations: &'a ExplicitCancellationContext,
    completion_handle: TaskCompletionHandle,
}

pub(crate) struct TaskCompletionHandle {
    internal: Arc<DiceTaskInternal>,
    // holds the result while `DiceTaskHandle` is not dropped, then upon drop, stores it into
    // `DiceTaskInternal`. So the result is never held in two spots at the same time.
    result: Option<DiceComputedValue>,
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
        cancellations: &'a ExplicitCancellationContext,
    ) -> Self {
        Self {
            internal: internal.dupe(),
            cancellations,
            completion_handle: TaskCompletionHandle {
                internal,
                result: None,
            },
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

    pub(crate) fn finished(&mut self, value: DiceComputedValue) {
        self.completion_handle.finished(value)
    }

    pub(crate) fn cancellation_ctx(&self) -> &'a ExplicitCancellationContext {
        self.cancellations
    }

    #[cfg(test)]
    pub(crate) fn testing_new() -> DiceTaskHandle<'static> {
        let internal = DiceTaskInternal::new(crate::impls::key::DiceKey { index: 99999 });
        DiceTaskHandle::<'static> {
            internal: internal.dupe(),
            cancellations: ExplicitCancellationContext::testing(),
            completion_handle: TaskCompletionHandle {
                internal,
                result: None,
            },
        }
    }
}

impl TaskCompletionHandle {
    pub(crate) fn finished(&mut self, value: DiceComputedValue) {
        let _ignore = self.result.insert(value);
    }
}

impl Drop for TaskCompletionHandle {
    fn drop(&mut self) {
        if let Some(value) = self.result.take() {
            // okay to ignore as it only errors on cancelled, in which case we don't care to set
            // the result successfully.
            debug!("{:?} finished. Notifying result", self.internal.key);
            let _ignore = self.internal.set_value(value);
        } else {
            debug!("{:?} cancelled. Notifying cancellation", self.internal.key);

            // This is only owned by the main worker task. If this was dropped, and no result was
            // ever recorded, then we must have been terminated.
            self.internal.report_terminated()
        }
    }
}

unsafe impl<'a> Send for DiceTaskHandle<'a> {}
