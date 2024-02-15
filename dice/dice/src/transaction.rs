/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use dupe::Dupe;

use crate::ctx::DiceComputationsImpl;
use crate::impls::ctx::BaseComputeCtx;
use crate::transaction_update::DiceTransactionUpdaterImpl;
use crate::versions::VersionNumber;
use crate::DiceComputations;
use crate::DiceTransactionUpdater;

#[derive(Allocative)]
pub(crate) enum DiceTransactionImpl<'a> {
    Legacy(DiceComputations<'a>),
    Modern(BaseComputeCtx<'a>),
}

impl Clone for DiceTransactionImpl<'_> {
    fn clone(&self) -> Self {
        match self {
            DiceTransactionImpl::Legacy(ctx) => match &ctx.inner() {
                // since 'DiceComputations' should not be clone or dupe, we manually implement
                // clone and dupe for the 'DiceTransaction'
                DiceComputationsImpl::Legacy(ctx) => DiceTransactionImpl::Legacy(
                    DiceComputations::new(DiceComputationsImpl::Legacy(ctx.dupe())),
                ),
                DiceComputationsImpl::Modern(_) => {
                    unreachable!("wrong dice")
                }
            },
            DiceTransactionImpl::Modern(ctx) => DiceTransactionImpl::Modern(ctx.clone()),
        }
    }
}

impl Dupe for DiceTransactionImpl<'_> {}

impl<'a> DiceTransactionImpl<'a> {
    pub(crate) fn get_version(&self) -> VersionNumber {
        match self {
            DiceTransactionImpl::Legacy(ctx) => ctx.inner().get_version(),
            DiceTransactionImpl::Modern(ctx) => ctx.get_version(),
        }
    }

    pub(crate) fn into_updater(self) -> DiceTransactionUpdater {
        match self {
            DiceTransactionImpl::Legacy(delegate) => match delegate.inner() {
                DiceComputationsImpl::Legacy(ctx) => {
                    DiceTransactionUpdater(DiceTransactionUpdaterImpl::Legacy(ctx.dupe()))
                }
                DiceComputationsImpl::Modern(_) => {
                    unreachable!("legacy dice")
                }
            },
            DiceTransactionImpl::Modern(delegate) => delegate.into_updater(),
        }
    }

    pub(crate) fn as_computations(&self) -> &DiceComputations<'a> {
        match self {
            DiceTransactionImpl::Legacy(ctx) => ctx,
            DiceTransactionImpl::Modern(ctx) => ctx.as_computations(),
        }
    }

    pub(crate) fn as_computations_mut(&mut self) -> &mut DiceComputations<'a> {
        match self {
            DiceTransactionImpl::Legacy(ctx) => ctx,
            DiceTransactionImpl::Modern(ctx) => ctx.as_computations_mut(),
        }
    }
}
