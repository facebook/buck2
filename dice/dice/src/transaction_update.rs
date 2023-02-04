/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;

use crate::api::computations::DiceComputations;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationsImpl;
use crate::legacy::ctx::DiceComputationsImplLegacy;
use crate::DiceTransaction;

/// The struct for which we build transactions. This is where changes are recorded, and committed
/// to DICE, which returns the Transaction where we spawn computations.
#[derive(Allocative)]
pub(crate) enum DiceTransactionUpdaterImpl {
    Legacy(Arc<DiceComputationsImplLegacy>),
}

impl DiceTransactionUpdaterImpl {
    pub(crate) fn existing_state(&self) -> DiceComputations {
        match self {
            DiceTransactionUpdaterImpl::Legacy(ctx) => {
                DiceComputations(DiceComputationsImpl::Legacy(ctx.dupe()))
            }
        }
    }

    /// Records a set of `Key`s as changed so that they, and any dependents will
    /// be recomputed on the next set of requests at the next version.
    pub(crate) fn changed<K, I>(&self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = K> + Send + Sync + 'static,
    {
        match self {
            DiceTransactionUpdaterImpl::Legacy(ctx) => ctx.changed(changed),
        }
    }

    /// Records a set of `Key`s as changed to a particular value so that any
    /// dependents will be recomputed on the next set of requests. The
    /// `Key`s themselves will be update to the new value such that they
    /// will not need to be recomputed as long as they aren't recorded to be
    /// `changed` again (or invalidated by other means). Calling this method
    /// does not in anyway alter the types of the key such that they
    /// permanently becomes a special "inject value only" key.
    pub(crate) fn changed_to<K, I>(&self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = (K, K::Value)> + Send + Sync + 'static,
    {
        match self {
            DiceTransactionUpdaterImpl::Legacy(ctx) => ctx.changed_to(changed),
        }
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version.
    pub(crate) fn commit(self) -> DiceTransaction {
        match self {
            DiceTransactionUpdaterImpl::Legacy(ctx) => {
                DiceTransaction(DiceComputations(DiceComputationsImpl::Legacy(ctx.commit())))
            }
        }
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version,
    /// replacing the user data with the given set
    pub(crate) fn commit_with_data(self, extra: UserComputationData) -> DiceTransaction {
        match self {
            DiceTransactionUpdaterImpl::Legacy(ctx) => DiceTransaction(DiceComputations(
                DiceComputationsImpl::Legacy(ctx.commit_with_data(extra)),
            )),
        }
    }
}
