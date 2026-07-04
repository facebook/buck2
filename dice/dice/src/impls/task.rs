/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

pub(crate) mod dice;
pub(crate) mod handle;
pub(crate) mod projections;
pub(crate) mod promise;
#[cfg(test)]
pub(crate) use dice::spawn_dice_task;

use crate::impls::task::dice::TerminationObserver;

#[cfg(test)]
mod tests;

pub(crate) struct PreviouslyCancelledTask {
    previous: TerminationObserver,
}

impl PreviouslyCancelledTask {
    pub(crate) fn new(previous: TerminationObserver) -> Self {
        Self { previous }
    }

    pub(crate) fn await_termination(self) -> crate::impls::task::dice::TerminationObserver {
        self.previous
    }
}
