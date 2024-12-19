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
use dice_error::DiceResult;
use futures::future::BoxFuture;
use futures::FutureExt;
use gazebo::variants::UnpackVariants;

use crate::api::computations::DiceComputations;
use crate::api::data::DiceData;
use crate::api::invalidation_tracking::DiceKeyTrackedInvalidationPaths;
use crate::api::key::Key;
use crate::api::opaque::OpaqueValue;
use crate::api::user_data::UserComputationData;
use crate::api::user_data::UserCycleDetectorGuard;
use crate::impls::ctx::LinearRecomputeModern;
use crate::impls::ctx::ModernComputeCtx;
use crate::opaque::OpaqueValueImpl;
use crate::versions::VersionNumber;
use crate::LinearRecomputeDiceComputations;
use crate::ProjectionKey;

/// This is just a dispatcher to either of Legacy or Modern Dice.
///
/// It converts their impl futures into a common impl (via left/right_future()) and does some
/// minor packing/unpacking of types (like OpaqueValue to/from OpaqueValueLegacy). Otherwise it
/// just forwards calls along.
#[derive(Allocative, UnpackVariants)]
pub(crate) enum DiceComputationsImpl<'a> {
    Modern(ModernComputeCtx<'a>),
}

impl<'d> DiceComputationsImpl<'d> {
    /// Gets all the result of of the given computation key.
    /// recorded as dependencies of the current computation for which this
    /// context is for.
    pub(crate) fn compute<'a, K>(
        &'a mut self,
        key: &K,
    ) -> impl Future<Output = DiceResult<<K as Key>::Value>> + 'a
    where
        K: Key,
    {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.compute(key),
        }
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque<'a, K>(
        &'a self,
        key: &K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<K>>> + 'a
    where
        K: Key,
    {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate
                .compute_opaque(key)
                .map(|r| r.map(|x| OpaqueValue::new(OpaqueValueImpl::Modern(x)))),
        }
    }

    pub fn projection<'a, K: Key, P: ProjectionKey<DeriveFromKey = K>>(
        &'a mut self,
        derive_from: &OpaqueValue<K>,
        projection_key: &P,
    ) -> DiceResult<P::Value> {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.projection(
                derive_from.unpack_modern().expect("engine type mismatch"),
                projection_key,
            ),
        }
    }

    pub fn opaque_into_value<'a, K: Key>(
        &'a mut self,
        derive_from: OpaqueValue<K>,
    ) -> DiceResult<K::Value> {
        match self {
            DiceComputationsImpl::Modern(delegate) => Ok(delegate
                .opaque_into_value(derive_from.into_modern().expect("engine type mismatch"))),
        }
    }

    /// Computes all the given tasks in parallel, returning an unordered Stream
    pub(crate) fn compute_many<'a, T: 'a>(
        &'a mut self,
        computes: impl IntoIterator<
            Item = impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
        >,
    ) -> Vec<impl Future<Output = T> + 'a> {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.compute_many(computes),
        }
    }

    pub(crate) fn compute2<'a, T: 'a, U: 'a>(
        &'a mut self,
        compute1: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
        compute2: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, U> + Send,
    ) -> (impl Future<Output = T> + 'a, impl Future<Output = U> + 'a) {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.compute2(compute1, compute2),
        }
    }

    pub(crate) fn compute3<'a, T: 'a, U: 'a, V: 'a>(
        &'a mut self,
        compute1: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
        compute2: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, U> + Send,
        compute3: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, V> + Send,
    ) -> (
        impl Future<Output = T> + 'a,
        impl Future<Output = U> + 'a,
        impl Future<Output = V> + 'a,
    ) {
        match self {
            DiceComputationsImpl::Modern(delegate) => {
                delegate.compute3(compute1, compute2, compute3)
            }
        }
    }

    pub(crate) fn with_linear_recompute<'a, T, Fut: Future<Output = T> + 'a>(
        &'a mut self,
        func: impl FnOnce(LinearRecomputeDiceComputations<'a>) -> Fut + 'a,
    ) -> impl Future<Output = T> + 'a {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.with_linear_recompute(func),
        }
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub(crate) fn global_data(&self) -> &DiceData {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.global_data(),
        }
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub(crate) fn per_transaction_data(&self) -> &UserComputationData {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.per_transaction_data(),
        }
    }

    pub(crate) fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<Arc<T>>> {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.cycle_guard(),
        }
    }

    pub fn store_evaluation_data<T: Send + Sync + 'static>(&self, value: T) -> DiceResult<()> {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.store_evaluation_data(value),
        }
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.get_version(),
        }
    }

    pub fn get_invalidation_paths(&mut self) -> DiceKeyTrackedInvalidationPaths {
        match self {
            DiceComputationsImpl::Modern(delegate) => delegate.get_invalidation_paths(),
        }
    }
}

pub(crate) enum LinearRecomputeDiceComputationsImpl<'a> {
    Modern(LinearRecomputeModern<'a>),
}

impl LinearRecomputeDiceComputationsImpl<'_> {
    pub(crate) fn get(&self) -> DiceComputations<'_> {
        match self {
            LinearRecomputeDiceComputationsImpl::Modern(delegate) => delegate.get(),
        }
    }
}
