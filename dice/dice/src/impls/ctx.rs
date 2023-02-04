/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;

use allocative::Allocative;
use dupe::Dupe;

use crate::api::data::DiceData;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::transaction::DiceTransactionUpdater;
use crate::api::user_data::UserComputationData;
use crate::impls::opaque::OpaqueValueModern;
use crate::versions::VersionNumber;

#[derive(Allocative, Dupe, Clone)]
pub(crate) struct ComputationCtx {}

#[allow(clippy::manual_async_fn)]
impl ComputationCtx {
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
        async move { unimplemented!("todo") }
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
        async move { unimplemented!("todo") }
    }

    /// temporarily here while we figure out why dice isn't paralleling computations so that we can
    /// use this in tokio spawn. otherwise, this shouldn't be here so that we don't need to clone
    /// the Arc, which makes lifetimes weird.
    pub(crate) fn temporary_spawn<F, FUT, R>(
        &self,
        f: F,
    ) -> impl Future<Output = R> + Send + 'static
    where
        F: FnOnce(ComputationCtx) -> FUT + Send + 'static,
        FUT: Future<Output = R> + Send,
        R: Send + 'static,
    {
        async move { unimplemented!("todo") }
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub(crate) fn global_data(&self) -> &DiceData {
        unimplemented!("todo")
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub(crate) fn per_transaction_data(&self) -> &UserComputationData {
        unimplemented!("todo")
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        unimplemented!("todo")
    }

    pub(crate) fn into_updater(self) -> DiceTransactionUpdater {
        unimplemented!("todo")
    }
}
