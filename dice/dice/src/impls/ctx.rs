/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::ops::Deref;
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
use crate::api::projection::ProjectionKey;
use crate::api::user_data::UserComputationData;
use crate::impls::cache::SharedCache;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::dep_trackers::RecordingDepsTracker;
use crate::impls::dice::DiceModern;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::evaluator::SyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::incremental::IncrementalEngine;
use crate::impls::key::CowDiceKey;
use crate::impls::key::DiceKey;
use crate::impls::key::DiceKeyErasedRef;
use crate::impls::key::ParentKey;
use crate::impls::opaque::OpaqueValueModern;
use crate::impls::task::sync_dice_task;
use crate::impls::transaction::ActiveTransactionGuard;
use crate::impls::transaction::TransactionUpdater;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValidity;
use crate::impls::value::MaybeValidDiceValue;
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
    parent_key: ParentKey,
    dice: Arc<DiceModern>,
}

#[allow(clippy::manual_async_fn, unused)]
impl PerComputeCtx {
    pub(crate) fn new(
        parent_key: ParentKey,
        per_live_version_ctx: SharedLiveTransactionCtx,
        user_data: Arc<UserComputationData>,
        dice: Arc<DiceModern>,
    ) -> Self {
        Self {
            data: Arc::new(PerComputeCtxData {
                per_live_version_ctx,
                user_data,
                dep_trackers: Mutex::new(RecordingDepsTracker::new()),
                parent_key,
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
                self.data.parent_key,
                self.data.dice.state_handle.dupe(),
                AsyncEvaluator::new(
                    self.data.per_live_version_ctx.dupe(),
                    self.data.user_data.dupe(),
                    self.data.dice.dupe(),
                ),
                &self.data.user_data,
                DiceEventDispatcher::new(self.data.user_data.tracker.dupe(), self.data.dice.dupe()),
            )
            .map(move |dice_result| {
                dice_result.map(move |dice_value| {
                    OpaqueValueModern::new(self, dice_key, dice_value.value().dupe())
                })
            })
    }

    /// Compute "projection" based on deriving value
    pub(crate) fn project<K>(
        &self,
        key: &K,
        base_key: DiceKey,
        base: MaybeValidDiceValue,
    ) -> DiceResult<K::Value>
    where
        K: ProjectionKey,
    {
        let dice_key = self
            .data
            .dice
            .key_index
            .index(CowDiceKey::Ref(DiceKeyErasedRef::proj(base_key, key)));

        self.data
            .per_live_version_ctx
            .compute_projection(
                dice_key,
                self.data.dice.state_handle.dupe(),
                SyncEvaluator::new(
                    self.data.per_live_version_ctx.dupe(),
                    self.data.user_data.dupe(),
                    self.data.dice.dupe(),
                    base,
                ),
                DiceEventDispatcher::new(self.data.user_data.tracker.dupe(), self.data.dice.dupe()),
            )
            .map(|r| {
                self.data
                    .dep_trackers
                    .lock()
                    .record(dice_key, r.value().validity());

                r.value()
                    .downcast_maybe_transient::<K::Value>()
                    .expect("Type mismatch when computing key")
                    .dupe()
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

    pub(crate) fn finalize_deps(self) -> (HashSet<DiceKey>, DiceValidity) {
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
        parent_key: ParentKey,
        state: CoreStateHandle,
        eval: AsyncEvaluator,
        extra: &Arc<UserComputationData>,
        events: DiceEventDispatcher,
    ) -> impl Future<Output = DiceResult<DiceComputedValue>> {
        match self.cache.get(key) {
            Entry::Occupied(occupied) => occupied.get().depended_on_by(parent_key),
            Entry::Vacant(vacant) => {
                let task =
                    IncrementalEngine::spawn_for_key(state, extra, key, eval, self.dupe(), events);

                let fut = task.depended_on_by(parent_key);

                vacant.insert(task);

                fut
            }
        }
    }

    /// Compute "projection" based on deriving value
    pub(crate) fn compute_projection(
        &self,
        key: DiceKey,
        state: CoreStateHandle,
        eval: SyncEvaluator,
        events: DiceEventDispatcher,
    ) -> DiceResult<DiceComputedValue> {
        let task = match self.cache.get(key) {
            Entry::Occupied(occupied) => occupied.into_ref(),
            Entry::Vacant(vacant) => {
                let task = unsafe {
                    // SAFETY: task completed below by `IncrementalEngine::project_for_key`
                    sync_dice_task()
                };

                vacant.insert(task)
            }
        };

        IncrementalEngine::project_for_key(state, task.deref(), key, eval, self.dupe(), events)
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.version
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use dashmap::mapref::entry::Entry;

    use crate::impls::ctx::SharedLiveTransactionCtx;
    use crate::impls::key::DiceKey;
    use crate::impls::task::sync_dice_task;
    use crate::impls::value::DiceComputedValue;
    use crate::DiceResult;

    impl SharedLiveTransactionCtx {
        pub(crate) fn inject(&self, k: DiceKey, v: DiceResult<DiceComputedValue>) {
            let task = unsafe {
                // SAFETY: completed immediately below
                sync_dice_task()
            };
            let _r = task.get_or_complete(|| v);

            match self.cache.get(k) {
                Entry::Occupied(o) => {
                    o.replace_entry(task);
                }
                Entry::Vacant(v) => {
                    v.insert(task);
                }
            }
        }
    }
}
