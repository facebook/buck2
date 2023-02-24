/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::marker::PhantomData;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use futures::FutureExt;

use crate::api::data::DiceData;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::impls::dice::DiceModern;
use crate::impls::key::CowDiceKey;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErasedRef;
use crate::impls::opaque::OpaqueValueModern;
use crate::impls::transaction::ActiveTransactionGuard;
use crate::impls::transaction::TransactionUpdater;
use crate::impls::value::DiceValue;
use crate::versions::VersionNumber;

/// Context given to the `compute` function of a `Key`.
#[derive(Allocative, Dupe, Clone)]
pub(crate) struct PerComputeCtx {
    per_live_version_ctx: Arc<SharedLiveTransactionCtx>,
    live_version_guard: ActiveTransactionGuard,
    user_data: Arc<UserComputationData>,
    dice: Arc<DiceModern>,
}

#[allow(clippy::manual_async_fn, unused)]
impl PerComputeCtx {
    pub(crate) fn new(
        per_live_version_ctx: Arc<SharedLiveTransactionCtx>,
        live_version_guard: ActiveTransactionGuard,
        user_data: Arc<UserComputationData>,
        dice: Arc<DiceModern>,
    ) -> Self {
        Self {
            per_live_version_ctx,
            live_version_guard,
            user_data,
            dice,
        }
    }

    /// Gets all the result of of the given computation key.
    /// recorded as dependencies of the current computation for which this
    /// context is for.
    pub(crate) fn compute<'a, K>(
        &'a self,
        key: &'a K,
    ) -> impl Future<Output = DiceResult<<K as Key>::Value>> + 'a
    where
        K: Key,
    {
        self.compute_opaque(key)
            .map(|r| r.map(|opaque| opaque.into_value()))
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque<'b, 'a: 'b, K>(
        &'a self,
        key: &'b K,
    ) -> impl Future<Output = DiceResult<OpaqueValueModern<K>>> + 'b
    where
        K: Key,
    {
        let dice_key = self
            .dice
            .key_index
            .index(CowDiceKey::Ref(DiceKeyErasedRef::new(key)));
        self.per_live_version_ctx
            .compute_opaque(dice_key)
            .map(|dice_result| {
                dice_result.map(|dice_value| {
                    // TODO properly create OpaqueValue
                    OpaqueValueModern(PhantomData)
                })
            })
    }

    /// temporarily here while we figure out why dice isn't paralleling computations so that we can
    /// use this in tokio spawn. otherwise, this shouldn't be here so that we don't need to clone
    /// the Arc, which makes lifetimes weird.
    pub(crate) fn temporary_spawn<F, FUT, R>(
        &self,
        f: F,
    ) -> impl Future<Output = R> + Send + 'static
    where
        F: FnOnce(PerComputeCtx) -> FUT + Send + 'static,
        FUT: Future<Output = R> + Send,
        R: Send + 'static,
    {
        async move { unimplemented!("todo") }
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub(crate) fn global_data(&self) -> &DiceData {
        &self.dice.global_data
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub(crate) fn per_transaction_data(&self) -> &UserComputationData {
        &self.user_data
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.per_live_version_ctx.get_version()
    }

    pub(crate) fn into_updater(self) -> TransactionUpdater {
        TransactionUpdater::new(self.dice.dupe(), self.user_data.dupe())
    }
}

/// Context that is shared for all current live computations of the same version.
#[derive(Allocative, Debug)]
pub(crate) struct SharedLiveTransactionCtx {
    version: VersionNumber,
}

#[allow(clippy::manual_async_fn, unused)]
impl SharedLiveTransactionCtx {
    pub(crate) fn new(v: VersionNumber) -> Arc<Self> {
        Arc::new(Self { version: v })
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque<'b, 'a: 'b>(
        self: &Arc<Self>,
        key: DiceKey,
    ) -> impl Future<Output = DiceResult<DiceValue>> + 'b {
        async move { unimplemented!("todo") }
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.version
    }
}

#[cfg(test)]
mod tests {

    use crate::impls::dice::DiceModernDataBuilder;
    use crate::DetectCycles;
    use crate::UserComputationData;

    #[tokio::test]
    async fn ctx_and_updater_with_user_data() {
        let dice = {
            let updater = DiceModernDataBuilder::new();

            updater.build(DetectCycles::Disabled)
        };

        let updater = dice.updater();
        let mut data = UserComputationData::new();
        data.data.set::<u32>(5);

        let ctx = updater.commit_with_data(data).await;
        let set_data = ctx
            .per_transaction_data()
            .data
            .get::<u32>()
            .expect("missing data");
        assert_eq!(set_data, &5);

        // check that into_updater keeps the data version
        let ctx = ctx.into_updater().commit().await;
        let set_data = ctx
            .per_transaction_data()
            .data
            .get::<u32>()
            .expect("missing data");
        assert_eq!(set_data, &5);

        let mut data = UserComputationData::new();
        data.data.set::<u32>(2);
        let updater = dice.updater_with_data(data);

        let ctx = updater.commit().await;
        let set_data = ctx
            .per_transaction_data()
            .data
            .get::<u32>()
            .expect("missing data");
        assert_eq!(set_data, &2);
    }
}
