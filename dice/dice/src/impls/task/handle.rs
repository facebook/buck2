/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Handle to the DiceTask as seen by the thread responsible for completing the task

use futures::task::waker;
use futures::task::AtomicWaker;
use triomphe::Arc;

use crate::impls::task::dice::DiceTaskInternal;
use crate::impls::value::DiceValue;

/// The handle to the 'DiceTask' owned by the spawned thread that is responsible for completing
/// the task.
pub(crate) struct DiceTaskHandle {
    pub(super) internal: Arc<DiceTaskInternal>,
}

impl DiceTaskHandle {
    pub(crate) fn checking_deps(&self) {
        self.internal.state.report_checking_deps();
    }

    pub(crate) fn computing(&self) {
        self.internal.state.report_computing();
    }

    pub(crate) fn finished(self, value: DiceValue) {
        let prev_exist = unsafe {
            // SAFETY: no tasks read the value unless state is converted to `READY`
            &mut *self.internal.maybe_value.get()
        }
        .replace(value)
        .is_some();
        assert!(
            !prev_exist,
            "invalid state where somehow value was already written"
        );

        self.internal.state.report_ready();

        let mut deps = self
            .internal
            .dependants
            .lock()
            .take()
            .expect("Invalid state where deps where taken already");

        deps.drain().for_each(|(_k, waker)| waker.wake())
    }
}

unsafe impl Send for DiceTaskHandle {}
