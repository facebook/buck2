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
use std::sync::Arc;

use allocative::Allocative;
use dice_error::DiceResult;
use dice_futures::cancellation::CancellationContext;
use futures::FutureExt;
use futures::future::BoxFuture;

use crate::LinearRecomputeDiceComputations;
use crate::ProjectionKey;
use crate::api::computations::DiceComputations;
use crate::api::data::DiceData;
use crate::api::invalidation_tracking::DiceKeyTrackedInvalidationPaths;
use crate::api::key::Key;
use crate::api::opaque::OpaqueValue;
use crate::api::user_data::UserComputationData;
use crate::api::user_data::UserCycleDetectorGuard;
use crate::impls::ctx::LinearRecomputeModern;
use crate::impls::ctx::ModernComputeCtx;
use crate::versions::VersionNumber;

/// This is a wrapper around ModernComputeCtx.
///
/// It forwards calls to the underlying ModernComputeCtx implementation.
#[derive(Allocative)]
pub(crate) struct DiceComputationsImpl<'a>(pub(crate) ModernComputeCtx<'a>);

impl DiceComputationsImpl<'_> {
    /// Gets all the result of of the given computation key.
    /// recorded as dependencies of the current computation for which this
    /// context is for.
    pub(crate) fn compute<'a, K>(
        &'a mut self,
        key: &K,
    ) -> impl Future<Output = DiceResult<<K as Key>::Value>> + use<'a, K>
    where
        K: Key,
    {
        self.0.compute(key)
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque<'a, K>(
        &'a self,
        key: &K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<K>>> + use<'a, K>
    where
        K: Key,
    {
        self.0
            .compute_opaque(key)
            .map(|r| r.map(|x| OpaqueValue::new(x)))
    }

    pub fn projection<K: Key, P: ProjectionKey<DeriveFromKey = K>>(
        &mut self,
        derive_from: &OpaqueValue<K>,
        projection_key: &P,
    ) -> DiceResult<P::Value> {
        self.0
            .projection(&derive_from.implementation, projection_key)
    }

    pub fn opaque_into_value<K: Key>(
        &mut self,
        derive_from: OpaqueValue<K>,
    ) -> DiceResult<K::Value> {
        Ok(self.0.opaque_into_value(derive_from.implementation))
    }

    /// Computes all the given tasks in parallel, returning an unordered Stream
    pub(crate) fn compute_many<'a, Computes, F, T>(
        &'a mut self,
        computes: Computes,
    ) -> Vec<impl Future<Output = T> + use<'a, Computes, F, T>>
    where
        Computes: IntoIterator<Item = F>,
        F: for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
    {
        self.0.compute_many(computes)
    }

    pub(crate) fn compute2<'a, Compute1, T, Compute2, U>(
        &'a mut self,
        compute1: Compute1,
        compute2: Compute2,
    ) -> (
        impl Future<Output = T> + use<'a, Compute1, T, Compute2, U>,
        impl Future<Output = U> + use<'a, Compute1, T, Compute2, U>,
    )
    where
        Compute1: for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
        Compute2: for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, U> + Send,
    {
        self.0.compute2(compute1, compute2)
    }

    pub(crate) fn compute3<'a, Compute1, T, Compute2, U, Compute3, V>(
        &'a mut self,
        compute1: Compute1,
        compute2: Compute2,
        compute3: Compute3,
    ) -> (
        impl Future<Output = T> + use<'a, Compute1, T, Compute2, U, Compute3, V>,
        impl Future<Output = U> + use<'a, Compute1, T, Compute2, U, Compute3, V>,
        impl Future<Output = V> + use<'a, Compute1, T, Compute2, U, Compute3, V>,
    )
    where
        Compute1: for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
        Compute2: for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, U> + Send,
        Compute3: for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, V> + Send,
    {
        self.0.compute3(compute1, compute2, compute3)
    }

    pub(crate) fn with_linear_recompute<'a, Func, Fut, T>(
        &'a mut self,
        func: Func,
    ) -> impl Future<Output = T> + use<'a, Func, Fut, T>
    where
        Func: FnOnce(LinearRecomputeDiceComputations<'a>) -> Fut,
        Fut: Future<Output = T>,
    {
        self.0.with_linear_recompute(func)
    }

    /// Spawn a computation on a new tokio task.
    ///
    /// See [`DiceComputations::spawned`](crate::api::computations::DiceComputations::spawned) for details.
    pub fn spawned<'a, T, Compute>(
        &'a mut self,
        closure: Compute,
    ) -> impl Future<Output = T> + use<'a, Compute, T>
    where
        T: Send + 'static,
        Compute: (for<'x> FnOnce(
                &'x mut DiceComputations<'_>,
                &'x CancellationContext,
            ) -> BoxFuture<'x, T>)
            + Send
            + 'static,
    {
        self.0.spawned(closure)
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub(crate) fn global_data(&self) -> &DiceData {
        self.0.global_data()
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub(crate) fn per_transaction_data(&self) -> &UserComputationData {
        self.0.per_transaction_data()
    }

    pub(crate) fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<Arc<T>>> {
        self.0.cycle_guard()
    }

    pub fn store_evaluation_data<T: Send + Sync + 'static>(&self, value: T) -> DiceResult<()> {
        self.0.store_evaluation_data(value)
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.0.get_version()
    }

    pub fn get_invalidation_paths(&mut self) -> DiceKeyTrackedInvalidationPaths {
        self.0.get_invalidation_paths()
    }
}

pub(crate) struct LinearRecomputeDiceComputationsImpl<'a>(pub(crate) LinearRecomputeModern<'a>);

impl LinearRecomputeDiceComputationsImpl<'_> {
    pub(crate) fn get(&self) -> DiceComputations<'_> {
        self.0.get()
    }
}
