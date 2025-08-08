/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;

use allocative::Allocative;
use dice_error::DiceResult;
use futures::FutureExt;

use crate::DiceTransaction;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::impls::transaction::TransactionUpdater;
use crate::transaction::DiceTransactionImpl;

/// The struct for which we build transactions. This is where changes are recorded, and committed
/// to DICE, which returns the Transaction where we spawn computations.
#[derive(Allocative)]
pub(crate) struct DiceTransactionUpdaterImpl(pub(crate) TransactionUpdater);

impl DiceTransactionUpdaterImpl {
    pub(crate) fn existing_state(&self) -> impl Future<Output = DiceTransaction> + '_ {
        self.0
            .existing_state()
            .map(|d| DiceTransaction(DiceTransactionImpl(d)))
    }

    /// Records a set of `Key`s as changed so that they, and any dependents will
    /// be recomputed on the next set of requests at the next version.
    pub(crate) fn changed<K, I>(&mut self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = K> + Send + Sync + 'static,
    {
        self.0.changed(changed)
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
        self.0.changed_to(changed)
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version.
    pub(crate) fn commit(self) -> impl Future<Output = DiceTransaction> {
        self.0
            .commit()
            .map(|x| DiceTransaction(DiceTransactionImpl(x)))
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version,
    /// replacing the user data with the given set
    pub(crate) fn commit_with_data(
        self,
        extra: UserComputationData,
    ) -> impl Future<Output = DiceTransaction> {
        self.0
            .commit_with_data(extra)
            .map(|x| DiceTransaction(DiceTransactionImpl(x)))
    }

    /// Clears the entire DICE state. The dropping of values from memory happens asynchronously.
    ///
    /// Any currently running computations may receive cancellations as we may have dropped data
    /// needed to make progress.
    // TODO(cjhopman): Why is this named take when it doesn't return the taken data? It should be named clear.
    pub fn unstable_take(self) -> Self {
        self.0.unstable_take();
        self
    }
}
