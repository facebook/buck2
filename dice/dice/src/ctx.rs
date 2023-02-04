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

use crate::api::data::DiceData;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::opaque::OpaqueValue;
use crate::api::user_data::UserComputationData;
use crate::legacy::ctx::DiceComputationsImplLegacy;
use crate::legacy::map::DiceMap;
use crate::opaque::OpaqueValueImpl;
use crate::versions::VersionNumber;

#[derive(Allocative, Dupe, Clone)]
pub(crate) enum DiceComputationsImpl {
    Legacy(Arc<DiceComputationsImplLegacy>),
}

impl DiceComputationsImpl {
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
        match self {
            DiceComputationsImpl::Legacy(delegate) => delegate
                .compute_opaque(key)
                .map(|r| r.map(|x| x.into_value())),
        }
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque<'b, 'a: 'b, K>(
        &'a self,
        key: &'b K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<'a, K>>> + 'b
    where
        K: Key,
    {
        match self {
            DiceComputationsImpl::Legacy(delegate) => delegate
                .compute_opaque(key)
                .map(|r| r.map(|x| OpaqueValue::new(OpaqueValueImpl::Legacy(x)))),
        }
    }

    /// temporarily here while we figure out why dice isn't paralleling computations so that we can
    /// use this in tokio spawn. otherwise, this shouldn't be here so that we don't need to clone
    /// the Arc, which makes lifetimes weird.
    pub(crate) fn temporary_spawn<F, FUT, R>(
        &self,
        f: F,
    ) -> impl Future<Output = R> + Send + 'static
    where
        F: FnOnce(DiceComputationsImpl) -> FUT + Send + 'static,
        FUT: Future<Output = R> + Send,
        R: Send + 'static,
    {
        match self {
            DiceComputationsImpl::Legacy(delegate) => delegate
                .temporary_spawn(async move |ctx| f(DiceComputationsImpl::Legacy(ctx)).await),
        }
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub(crate) fn global_data(&self) -> &DiceData {
        match self {
            DiceComputationsImpl::Legacy(delegate) => delegate.global_data(),
        }
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub(crate) fn per_transaction_data(&self) -> &UserComputationData {
        match self {
            DiceComputationsImpl::Legacy(delegate) => delegate.per_transaction_data(),
        }
    }

    pub(crate) fn changed<K, I>(&self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = K> + Send + Sync + 'static,
    {
        match self {
            DiceComputationsImpl::Legacy(delegate) => delegate.changed(changed),
        }
    }

    pub(crate) fn changed_to<K, I>(&self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = (K, K::Value)> + Send + Sync + 'static,
    {
        match self {
            DiceComputationsImpl::Legacy(delegate) => delegate.changed_to(changed),
        }
    }

    pub(crate) fn unstable_take(&self) -> DiceMap {
        match self {
            DiceComputationsImpl::Legacy(delegate) => delegate.unstable_take(),
        }
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version.
    /// This can only be called when the this is the only node remaining in the computation graph
    pub(super) fn commit(self) -> DiceComputationsImpl {
        match self {
            DiceComputationsImpl::Legacy(delegate) => {
                DiceComputationsImpl::Legacy(delegate.commit())
            }
        }
    }

    /// Same as `commit`, but replacing the user data with the given
    pub(super) fn commit_with_data(self, extra: UserComputationData) -> DiceComputationsImpl {
        match self {
            DiceComputationsImpl::Legacy(delegate) => {
                DiceComputationsImpl::Legacy(delegate.commit_with_data(extra))
            }
        }
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        match self {
            DiceComputationsImpl::Legacy(delegate) => delegate.get_version(),
        }
    }
}
