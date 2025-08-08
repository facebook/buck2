/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use dupe::Dupe;

use crate::DiceComputations;
use crate::DiceTransactionUpdater;
use crate::impls::ctx::BaseComputeCtx;
use crate::versions::VersionNumber;

#[derive(Allocative)]
pub(crate) struct DiceTransactionImpl(pub(crate) BaseComputeCtx);

impl Clone for DiceTransactionImpl {
    fn clone(&self) -> Self {
        DiceTransactionImpl(self.0.clone())
    }
}

impl Dupe for DiceTransactionImpl {}

impl DiceTransactionImpl {
    pub(crate) fn get_version(&self) -> VersionNumber {
        self.0.get_version()
    }

    pub(crate) fn into_updater(self) -> DiceTransactionUpdater {
        self.0.into_updater()
    }

    pub(crate) fn as_computations(&self) -> &DiceComputations<'static> {
        self.0.as_computations()
    }

    pub(crate) fn as_computations_mut(&mut self) -> &mut DiceComputations<'static> {
        self.0.as_computations_mut()
    }
}
