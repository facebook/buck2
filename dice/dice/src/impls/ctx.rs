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
use dashmap::mapref::entry::Entry;
use derivative::Derivative;
use dupe::Dupe;
use futures::FutureExt;
use more_futures::spawn::spawn_dropcancel;
use parking_lot::Mutex;
use parking_lot::MutexGuard;

use crate::api::data::DiceData;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::impls::cache::SharedCache;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::dep_trackers::RecordingDepsTracker;
use crate::impls::dice::DiceModern;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::incremental::IncrementalEngine;
use crate::impls::key::CowDiceKey;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErasedRef;
use crate::impls::opaque::OpaqueValueModern;
use crate::impls::transaction::ActiveTransactionGuard;
use crate::impls::transaction::TransactionUpdater;
use crate::impls::value::DiceValue;
use crate::versions::VersionNumber;
use crate::HashSet;

/// Context given to the `compute` function of a `Key`.
#[derive(Allocative, Dupe, Clone)]
pub(crate) struct PerComputeCtx {
    data: Arc<PerComputeCtxData>,
}

#[derive(Allocative)]
pub(crate) struct PerComputeCtxData {
    per_live_version_ctx: SharedLiveTransactionCtx,
    user_data: Arc<UserComputationData>,
    dep_trackers: Mutex<RecordingDepsTracker>, // If we make PerComputeCtx &mut, we can get rid of this mutex after some refactoring
    dice: Arc<DiceModern>,
}

#[allow(clippy::manual_async_fn, unused)]
impl PerComputeCtx {
    pub(crate) fn new(
        per_live_version_ctx: SharedLiveTransactionCtx,
        user_data: Arc<UserComputationData>,
        dice: Arc<DiceModern>,
    ) -> Self {
        Self {
            data: Arc::new(PerComputeCtxData {
                per_live_version_ctx,
                user_data,
                dep_trackers: Mutex::new(RecordingDepsTracker::new()),
                dice,
            }),
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
            .data
            .dice
            .key_index
            .index(CowDiceKey::Ref(DiceKeyErasedRef::key(key)));

        self.data
            .per_live_version_ctx
            .compute_opaque(
                dice_key,
                self.data.dice.state_handle.dupe(),
                AsyncEvaluator::new(
                    self.data.per_live_version_ctx.dupe(),
                    self.data.user_data.dupe(),
                    self.data.dice.dupe(),
                ),
                &Arc::new(Default::default()),
                DiceEventDispatcher::new(self.data.user_data.tracker.dupe(), self.data.dice.dupe()),
            )
            .map(move |dice_result| {
                dice_result.map(move |dice_value| {
                    OpaqueValueModern::new(
                        self,
                        dice_key,
                        dice_value
                            .downcast_ref::<K::Value>()
                            .expect("Type mismatch when computing key")
                            .dupe(),
                    )
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
        let duped = self.dupe();

        spawn_dropcancel(
            async move { f(duped).await },
            self.data.user_data.spawner.as_ref(),
            &self.data.user_data,
            debug_span!(parent: None, "spawned_task",),
        )
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub(crate) fn global_data(&self) -> &DiceData {
        &self.data.dice.global_data
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub(crate) fn per_transaction_data(&self) -> &UserComputationData {
        &self.data.user_data
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.data.per_live_version_ctx.get_version()
    }

    pub(crate) fn into_updater(self) -> TransactionUpdater {
        TransactionUpdater::new(self.data.dice.dupe(), self.data.user_data.dupe())
    }

    pub(super) fn dep_trackers(&self) -> MutexGuard<'_, RecordingDepsTracker> {
        self.data.dep_trackers.lock()
    }

    pub(crate) fn finalize_deps(self) -> HashSet<DiceKey> {
        // TODO need to clean up these ctxs so we have less runtime errors from Arc references
        let data = Arc::try_unwrap(self.data)
            .map_err(|_| "Error: tried to finalize when there are more references")
            .unwrap();
        data.dep_trackers.into_inner().collect_deps()
    }
}

/// Context that is shared for all current live computations of the same version.
#[derive(Allocative, Derivative, Dupe, Clone)]
#[derivative(Debug)]
pub(crate) struct SharedLiveTransactionCtx {
    version: VersionNumber,
    #[derivative(Debug = "ignore")]
    live_version_guard: ActiveTransactionGuard,
    #[derivative(Debug = "ignore")]
    cache: SharedCache,
}

#[allow(clippy::manual_async_fn, unused)]
impl SharedLiveTransactionCtx {
    pub(crate) fn new(
        v: VersionNumber,
        live_version_guard: ActiveTransactionGuard,
        cache: SharedCache,
    ) -> Self {
        Self {
            version: v,
            live_version_guard,
            cache,
        }
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque(
        &self,
        key: DiceKey,
        state: CoreStateHandle,
        eval: AsyncEvaluator,
        extra: &Arc<UserComputationData>,
        events: DiceEventDispatcher,
    ) -> impl Future<Output = DiceResult<DiceValue>> {
        match self.cache.get(key) {
            Entry::Occupied(occupied) => occupied.get().depended_on_by(key),
            Entry::Vacant(vacant) => {
                let task = IncrementalEngine::spawn_for_key(
                    state,
                    extra.spawner.dupe(),
                    extra,
                    key,
                    eval,
                    self.dupe(),
                    events,
                );

                let fut = task.depended_on_by(key);

                vacant.insert(task);

                fut
            }
        }
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.version
    }
}
