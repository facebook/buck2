/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

pub(crate) mod critical;
pub(crate) mod dice;
pub(crate) mod handle;
pub(crate) mod promise;
mod state;
#[cfg(test)]
pub(crate) use dice::spawn_dice_task;
pub(crate) use dice::sync_dice_task;
pub(crate) use state::DiceTaskState;

#[cfg(test)]
mod tests;

pub(crate) struct PreviouslyCancelledTask {
    previous: dice::DiceTask,
}

impl PreviouslyCancelledTask {
    pub(crate) fn new(previous: dice::DiceTask) -> Self {
        Self { previous }
    }

    pub(crate) fn await_termination(&self) -> crate::impls::task::dice::TerminationObserver {
        self.previous.await_termination()
    }

    pub(crate) fn get_finished_value(
        &self,
    ) -> Option<dice_error::result::CancellableResult<crate::impls::value::DiceComputedValue>> {
        self.previous.get_finished_value()
    }
}
