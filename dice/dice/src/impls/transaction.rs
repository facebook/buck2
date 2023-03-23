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
use derivative::Derivative;
use dupe::Dupe;
use tokio::sync::oneshot;

use crate::api::error::DiceError;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::storage_type::StorageType;
use crate::api::user_data::UserComputationData;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::core::state::StateRequest;
use crate::impls::ctx::PerComputeCtx;
use crate::impls::ctx::SharedLiveTransactionCtx;
use crate::impls::key::CowDiceKey;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErased;
use crate::impls::key::ParentKey;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceKeyValue;
use crate::impls::value::DiceValidValue;
use crate::impls::value::DiceValidity;
use crate::impls::value::MaybeValidDiceValue;
use crate::versions::VersionNumber;
use crate::DiceModern;
use crate::HashMap;

// TODO fill this more
#[derive(Allocative)]
pub(crate) struct TransactionUpdater {
    dice: Arc<DiceModern>,
    scheduled_changes: Changes,
    user_data: Arc<UserComputationData>,
}

impl TransactionUpdater {
    pub(crate) fn new(dice: Arc<DiceModern>, user_data: Arc<UserComputationData>) -> Self {
        Self {
            dice: dice.dupe(),
            scheduled_changes: Changes::new(dice),
            user_data,
        }
    }

    /// Records a set of `Key`s as changed so that they, and any dependents will
    /// be recomputed on the next set of requests at the next version.
    pub(crate) fn changed<K, I>(&mut self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = K> + Send + Sync + 'static,
    {
        changed
            .into_iter()
            .try_for_each(|k| self.scheduled_changes.change(k, ChangeType::Invalidate))
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
        changed.into_iter().try_for_each(|(k, new_value)| {
            match MaybeValidDiceValue::new(
                Arc::new(DiceKeyValue::<K>::new(new_value)),
                DiceValidity::Valid,
            )
            .into_valid_value()
            {
                Ok(validated_value) => self.scheduled_changes.change(
                    k,
                    ChangeType::UpdateValue(validated_value, K::storage_type()),
                ),
                Err(_) => Err(DiceError::invalid_change(Arc::new(k))),
            }
        })
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version.
    pub(crate) async fn commit(self) -> PerComputeCtx {
        let user_data = self.user_data.dupe();
        let dice = self.dice.dupe();

        let transaction = self.commit_to_state().await;

        let cycles = UserCycleDetectorData::new(user_data.cycle_detector.dupe(), dice.dupe());

        PerComputeCtx::new(ParentKey::None, transaction, user_data, dice, cycles)
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version,
    /// replacing the user data with the given set
    pub(crate) async fn commit_with_data(self, extra: UserComputationData) -> PerComputeCtx {
        let dice = self.dice.dupe();

        let transaction = self.commit_to_state().await;

        let cycles = UserCycleDetectorData::new(extra.cycle_detector.dupe(), dice.dupe());

        PerComputeCtx::new(ParentKey::None, transaction, Arc::new(extra), dice, cycles)
    }

    pub(crate) async fn existing_state(&self) -> PerComputeCtx {
        let (tx, rx) = oneshot::channel();
        self.dice
            .state_handle
            .request(StateRequest::CurrentVersion { resp: tx });

        let v = rx.await.unwrap();
        let guard = ActiveTransactionGuard::new(v, self.dice.state_handle.dupe());
        let (tx, rx) = oneshot::channel();
        self.dice.state_handle.request(StateRequest::CtxAtVersion {
            version: v,
            guard,
            resp: tx,
        });

        let transaction = rx.await.unwrap();
        PerComputeCtx::new(
            ParentKey::None,
            transaction,
            self.user_data.dupe(),
            self.dice.dupe(),
            UserCycleDetectorData::new(self.user_data.cycle_detector.dupe(), self.dice.dupe()),
        )
    }

    pub(crate) fn unstable_take(&self) {
        self.dice
            .state_handle
            .request(StateRequest::UnstableDropEverything)
    }

    async fn commit_to_state(self) -> SharedLiveTransactionCtx {
        let (tx, rx) = oneshot::channel();
        self.dice.state_handle.request(StateRequest::UpdateState {
            changes: self.scheduled_changes.changes.into_iter().collect(),
            resp: tx,
        });

        let v = rx.await.unwrap();
        let guard = ActiveTransactionGuard::new(v, self.dice.state_handle.dupe());
        let (tx, rx) = oneshot::channel();
        self.dice.state_handle.request(StateRequest::CtxAtVersion {
            guard,
            version: v,
            resp: tx,
        });

        rx.await.unwrap()
    }
}

#[derive(Allocative, Dupe, Clone, Derivative)]
#[derivative(Debug)]
pub(crate) struct ActiveTransactionGuard(Arc<ActiveTransactionGuardInner>);

impl ActiveTransactionGuard {
    pub(crate) fn new(v: VersionNumber, state_handle: CoreStateHandle) -> Self {
        Self(Arc::new(ActiveTransactionGuardInner { v, state_handle }))
    }
}

#[derive(Allocative, Derivative)]
#[derivative(Debug)]
pub(crate) struct ActiveTransactionGuardInner {
    v: VersionNumber,
    #[derivative(Debug = "ignore")]
    state_handle: CoreStateHandle,
}

impl Drop for ActiveTransactionGuardInner {
    fn drop(&mut self) {
        self.state_handle
            .request(StateRequest::DropCtxAtVersion { version: self.v })
    }
}

#[derive(Allocative)]
struct Changes {
    changes: HashMap<DiceKey, ChangeType>,
    dice: Arc<DiceModern>,
}

impl Changes {
    pub(crate) fn new(dice: Arc<DiceModern>) -> Self {
        Self {
            changes: HashMap::default(),
            dice,
        }
    }

    pub(crate) fn change<K: Key>(&mut self, key: K, change: ChangeType) -> DiceResult<()> {
        let key = self
            .dice
            .key_index
            .index(CowDiceKey::Owned(DiceKeyErased::key(key)));
        if self.changes.insert(key, change).is_some() {
            Err(DiceError::duplicate(
                self.dice.key_index.get(key).dupe().downcast::<K>().unwrap(),
            ))
        } else {
            Ok(())
        }
    }
}

#[derive(Allocative, Debug)]
pub(crate) enum ChangeType {
    /// Just invalidate the key
    Invalidate,
    /// Update the key to the given value
    UpdateValue(DiceValidValue, StorageType),
    #[cfg(test)]
    /// testing only, set as recheck but not required to rerun
    TestingSoftDirty,
}

#[cfg(test)]
mod tests {

    use allocative::Allocative;
    use assert_matches::assert_matches;
    use async_trait::async_trait;
    use derive_more::Display;

    use crate::api::computations::DiceComputations;
    use crate::api::data::DiceData;
    use crate::api::key::Key;
    use crate::impls::dice::DiceModern;
    use crate::impls::key::CowDiceKey;
    use crate::impls::key::DiceKeyErased;
    use crate::impls::transaction::ChangeType;
    use crate::versions::VersionNumber;

    #[derive(Allocative, Clone, PartialEq, Eq, Hash, Debug, Display)]
    struct K(usize);

    #[async_trait]
    impl Key for K {
        type Value = usize;

        async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
            unimplemented!("test")
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            unimplemented!("test")
        }
    }

    #[test]
    fn changes_are_recorded() -> anyhow::Result<()> {
        let dice = DiceModern::new(DiceData::new());
        let mut updater = dice.updater();

        updater.changed(vec![K(1), K(2)])?;

        updater.changed_to(vec![(K(3), 3), (K(4), 4)])?;

        assert_matches!(
            updater.scheduled_changes.changes.get(
                &dice
                    .key_index
                    .index(CowDiceKey::Owned(DiceKeyErased::key(K(1))))
            ),
            Some(ChangeType::Invalidate)
        );
        assert_matches!(
            updater.scheduled_changes.changes.get(
                &dice
                    .key_index
                    .index(CowDiceKey::Owned(DiceKeyErased::key(K(2))))
            ),
            Some(ChangeType::Invalidate)
        );

        assert_matches!(
        updater
            .scheduled_changes
            .changes
            .get(&dice.key_index.index(CowDiceKey::Owned(DiceKeyErased::key(K(3))))),
        Some(ChangeType::UpdateValue(x, _)) if *x.downcast_ref::<usize>().unwrap() == 3
            );

        assert_matches!(
        updater
            .scheduled_changes
            .changes
            .get(&dice.key_index.index(CowDiceKey::Owned(DiceKeyErased::key(K(4))))),
        Some(ChangeType::UpdateValue(x, _)) if *x.downcast_ref::<usize>().unwrap() == 4
            );

        assert!(updater.changed(vec![K(1)]).is_err());

        Ok(())
    }

    #[tokio::test]
    async fn transaction_versions() -> anyhow::Result<()> {
        let dice = DiceModern::new(DiceData::new());
        let mut updater = dice.updater();

        updater.changed(vec![K(1), K(2)])?;

        let ctx = updater.existing_state().await;
        assert_eq!(ctx.get_version(), VersionNumber::new(0));

        let ctx = updater.commit().await;
        assert_eq!(ctx.get_version(), VersionNumber::new(1));

        Ok(())
    }
}
