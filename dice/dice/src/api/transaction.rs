/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Deref;
use std::thread;

use allocative::Allocative;
use dupe::Dupe;

use crate::api::computations::DiceComputations;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationsImpl;
use crate::incremental::versions::VersionNumber;

/// The struct for which we build transactions. This is where changes are recorded, and committed
/// to DICE, which returns the Transaction where we spawn computations.
#[derive(Allocative)]
pub struct DiceTransactionUpdater {
    pub(crate) existing_state: DiceComputationsImpl,
}

impl DiceTransactionUpdater {
    pub(crate) fn new(ctx: DiceComputationsImpl) -> Self {
        Self {
            existing_state: ctx,
        }
    }

    pub fn existing_state(&self) -> &DiceComputations {
        DiceComputations::ref_cast(&self.existing_state)
    }

    /// Records a set of `Key`s as changed so that they, and any dependents will
    /// be recomputed on the next set of requests at the next version.
    pub fn changed<K, I>(&self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = K> + Send + Sync + 'static,
    {
        self.existing_state.changed(changed)
    }

    /// Records a set of `Key`s as changed to a particular value so that any
    /// dependents will be recomputed on the next set of requests. The
    /// `Key`s themselves will be update to the new value such that they
    /// will not need to be recomputed as long as they aren't recorded to be
    /// `changed` again (or invalidated by other means). Calling this method
    /// does not in anyway alter the types of the key such that they
    /// permanently becomes a special "inject value only" key.
    pub fn changed_to<K, I>(&self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = (K, K::Value)> + Send + Sync + 'static,
    {
        self.existing_state.changed_to(changed)
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version.
    pub fn commit(self) -> DiceTransaction {
        DiceTransaction(DiceComputations(self.existing_state.commit()))
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version,
    /// replacing the user data with the given set
    pub fn commit_with_data(self, extra: UserComputationData) -> DiceTransaction {
        DiceTransaction(DiceComputations(
            self.existing_state.commit_with_data(extra),
        ))
    }
}

/// The base struct for which all computations start. This is clonable, and dupe, and can be
/// moved to different runtimes to start computations.
/// All computations on this transaction will see only changes at the most-up-to-date version at
/// the time of creation of this transaction.
///
/// This SHOULD NOT be ever stored by computations, or any results of computations.
#[derive(Allocative)]
pub struct DiceTransaction(pub(crate) DiceComputations);

impl DiceTransaction {
    pub fn unstable_take(self) -> Self {
        let map = self.0.0.unstable_take();
        // Destructors can be slow, so we do this in a separate thread.
        thread::spawn(|| drop(map));
        self
    }

    /// Returns whether the `DiceTransaction` is equivalent. Equivalent is defined as whether the
    /// two Transactions are based off the same underlying set of key states. That is, all
    /// injected keys are the same, and the same compute keys are dirtied, and that any computations
    /// that occur between the two transactions can be shared.
    pub fn equivalent<E>(&self, other: &E) -> bool
    where
        E: DiceEquivalent,
    {
        self.version_for_equivalence() == other.version_for_equivalence()
    }

    pub fn equality_token(&self) -> DiceEquality {
        DiceEquality(self.0.0.get_version())
    }

    /// Creates an Updater to record changes to DICE that upon committing, creates a new transaction
    /// that keeps the same set of user data. This is equivalent to `Dice::updater_with_user_data(data)`
    /// where the `data` is taken from the current Transaction.
    pub fn into_updater(self) -> DiceTransactionUpdater {
        DiceTransactionUpdater {
            existing_state: self.0.0,
        }
    }
}

#[derive(Allocative, Eq, PartialEq, Copy, Clone, derive_more::Display)]
#[repr(transparent)]
pub struct DiceEquality(VersionNumber);

mod private {
    use super::*;

    pub trait Sealed {}

    impl Sealed for DiceTransaction {}

    impl Sealed for DiceEquality {}
}

pub trait DiceEquivalent: private::Sealed {
    fn version_for_equivalence(&self) -> DiceEquality;
}

impl DiceEquivalent for DiceTransaction {
    fn version_for_equivalence(&self) -> DiceEquality {
        self.equality_token()
    }
}

impl DiceEquivalent for DiceEquality {
    fn version_for_equivalence(&self) -> DiceEquality {
        *self
    }
}

impl Deref for DiceTransaction {
    type Target = DiceComputations;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Clone for DiceTransaction {
    fn clone(&self) -> Self {
        Self(self.0.dupe())
    }
}

impl Dupe for DiceTransaction {}
