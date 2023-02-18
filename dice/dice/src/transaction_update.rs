/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use futures::FutureExt;

use crate::api::computations::DiceComputations;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationsImpl;
use crate::impls::transaction::TransactionUpdater;
use crate::legacy::ctx::DiceComputationsImplLegacy;
use crate::DiceTransaction;

/// The struct for which we build transactions. This is where changes are recorded, and committed
/// to DICE, which returns the Transaction where we spawn computations.
#[derive(Allocative)]
pub(crate) enum DiceTransactionUpdaterImpl {
    Legacy(Arc<DiceComputationsImplLegacy>),
    #[allow(unused)]
    Modern(TransactionUpdater),
}

impl DiceTransactionUpdaterImpl {
    pub(crate) fn existing_state(&self) -> impl Future<Output = DiceComputations> {
        match self {
            DiceTransactionUpdaterImpl::Legacy(ctx) => {
                futures::future::ready(DiceComputations(DiceComputationsImpl::Legacy(ctx.dupe())))
                    .left_future()
            }
            DiceTransactionUpdaterImpl::Modern(delegate) => delegate
                .existing_state()
                .map(|d| DiceComputations(DiceComputationsImpl::Modern(d)))
                .right_future(),
        }
    }

    /// Records a set of `Key`s as changed so that they, and any dependents will
    /// be recomputed on the next set of requests at the next version.
    pub(crate) fn changed<K, I>(&mut self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = K> + Send + Sync + 'static,
    {
        match self {
            DiceTransactionUpdaterImpl::Legacy(ctx) => ctx.changed(changed),
            DiceTransactionUpdaterImpl::Modern(delegate) => delegate.changed(changed),
        }
    }

    /// Records a set of `Key`s as changed to a particular value so that any
    /// dependents will be recomputed on the next set of requests. The
    /// `Key`s themselves will be update to the new value such that they
    /// will not need to be recomputed as long as they aren't recorded to be
    /// `changed` again (or invalidated by other means). Calling this method
    /// does not in anyway alter the types of the key such that they
    /// permanently becomes a special "inject value only" key.
    pub(crate) fn changed_to<K, I>(&mut self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = (K, K::Value)> + Send + Sync + 'static,
    {
        match self {
            DiceTransactionUpdaterImpl::Legacy(ctx) => ctx.changed_to(changed),
            DiceTransactionUpdaterImpl::Modern(delegate) => delegate.changed_to(changed),
        }
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version.
    pub(crate) fn commit(self) -> impl Future<Output = DiceTransaction> {
        match self {
            DiceTransactionUpdaterImpl::Legacy(ctx) => futures::future::ready(DiceTransaction(
                DiceComputations(DiceComputationsImpl::Legacy(ctx.commit())),
            ))
            .left_future(),
            DiceTransactionUpdaterImpl::Modern(delegate) => delegate
                .commit()
                .map(|x| DiceTransaction(DiceComputations(DiceComputationsImpl::Modern(x))))
                .right_future(),
        }
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version,
    /// replacing the user data with the given set
    pub(crate) fn commit_with_data(
        self,
        extra: UserComputationData,
    ) -> impl Future<Output = DiceTransaction> {
        match self {
            DiceTransactionUpdaterImpl::Legacy(ctx) => futures::future::ready(DiceTransaction(
                DiceComputations(DiceComputationsImpl::Legacy(ctx.commit_with_data(extra))),
            ))
            .left_future(),
            DiceTransactionUpdaterImpl::Modern(delegate) => delegate
                .commit_with_data(extra)
                .map(|x| DiceTransaction(DiceComputations(DiceComputationsImpl::Modern(x))))
                .right_future(),
        }
    }
}
