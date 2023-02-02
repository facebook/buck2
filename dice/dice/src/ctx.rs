/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::future::Future;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use futures::FutureExt;

use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::error::DiceResult;
use crate::api::events::DiceEvent;
use crate::api::events::DiceEventListener;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::cycles::CycleDetector;
use crate::incremental::dep_trackers::BothDepTrackers;
use crate::incremental::dep_trackers::BothDeps;
use crate::incremental::transaction_ctx::ActiveTransactionCountGuard;
use crate::incremental::transaction_ctx::Changes;
use crate::incremental::transaction_ctx::TransactionCtx;
use crate::incremental::versions::VersionForWrites;
use crate::incremental::versions::VersionGuard;
use crate::legacy::DiceLegacy;
use crate::map::DiceMap;
use crate::opaque::OpaqueValue;
use crate::projection::ProjectionKeyAsKey;
use crate::ProjectionKey;

/// A context for the duration of a top-level compute request.
///
/// This contains both user-visible and dice-internal computation-specific data.
#[derive(Allocative)]
pub(crate) struct ComputationData {
    pub(crate) user_data: Arc<UserComputationData>,
    cycle_detector: Option<Box<CycleDetector>>,
    // TODO(bobyf): this seems a natural place to gather some stats about the compute too
}

#[derive(Allocative)]
pub(crate) struct NoOpTracker;

impl DiceEventListener for NoOpTracker {
    fn event(&self, _ev: DiceEvent) {}
}

impl ComputationData {
    pub(crate) fn new(data: UserComputationData, detect_cycles: DetectCycles) -> Self {
        Self {
            user_data: Arc::new(data),
            cycle_detector: match detect_cycles {
                DetectCycles::Enabled => Some(box CycleDetector::new()),
                DetectCycles::Disabled => None,
            },
        }
    }

    /// records that we are entering the computation of another key as part of this main request
    /// i.e. computing key a, which during its evaluation requests key b, enters a new subrequest.
    pub(crate) fn subrequest<K>(&self, key: &K) -> DiceResult<Self>
    where
        K: Allocative + Clone + Display + Debug + Eq + Hash + Send + Sync + 'static,
    {
        Ok(Self {
            user_data: self.user_data.dupe(),
            cycle_detector: self
                .cycle_detector
                .as_ref()
                .map(|detector| Ok(box CycleDetector::visit(detector, key)?))
                .transpose()?,
        })
    }
}

/// A context for computations to request for additional dependencies. The
/// dependencies accessed are tracked for caching, if enabled based on
/// 'Strategy'.
///
/// The context is valid only for the duration of the computation of a single
/// key.
///
/// When marking values as changed on the ctx, the changes are part of the next
/// version. The next version is only committed when the current context is
/// dropped. context, which means that the "current" context will not see
/// updated values.
#[derive(Allocative)]
pub(crate) struct DiceComputationImpl {
    pub(crate) transaction_ctx: Arc<TransactionCtx>,
    pub(crate) dice: Arc<DiceLegacy>,
    pub(crate) dep_trackers: BothDepTrackers,
    pub(crate) extra: ComputationData,
}

impl DiceComputationImpl {
    pub(super) fn new_transaction(
        dice: Arc<DiceLegacy>,
        version: VersionGuard,
        version_for_writes: VersionForWrites,
        extra: ComputationData,
    ) -> Self {
        Self {
            transaction_ctx: Arc::new(TransactionCtx::new(
                version,
                version_for_writes,
                Changes::new(),
                ActiveTransactionCountGuard::new(&dice),
            )),
            dep_trackers: BothDepTrackers::noop(),
            dice: dice.dupe(),
            extra,
        }
    }

    pub(super) fn new_for_key_evaluation(
        dice: Arc<DiceLegacy>,
        transaction_ctx: Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> Arc<Self> {
        // TODO(bobyf): for memory, handle cases where we don't want explicit tracking
        Arc::new(Self {
            transaction_ctx,
            dice: dice.dupe(),
            dep_trackers: BothDepTrackers::recording(),
            extra,
        })
    }

    pub(super) fn finalize(self: Arc<Self>) -> BothDeps {
        // TODO express this via lifetimes
        let this = Arc::try_unwrap(self).map_err(|_| "The computation lifetime of the `ctx` has ended and there should be no further references to the `Arc`").unwrap();

        this.dep_trackers.collect_deps()
    }

    pub(super) fn compute_opaque<'b, 'a: 'b, K>(
        self: &'a Arc<Self>,
        key: &'b K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<'a, K>>> + 'b
    where
        K: Key,
    {
        async move {
            let cache = self.dice.find_cache::<K>();
            let extra = self.extra.subrequest(key)?;
            let value = cache
                .eval_for_opaque(key, &self.transaction_ctx, extra)
                .await?;
            Ok(OpaqueValue::new(value, self, cache))
        }
        .boxed()
    }

    pub(super) fn compute_projection_sync<P>(
        self: &Arc<Self>,
        derive_from: &OpaqueValue<P::DeriveFromKey>,
        projection_key: &P,
    ) -> DiceResult<P::Value>
    where
        P: ProjectionKey,
    {
        assert!(Arc::ptr_eq(self, derive_from.parent_computations));

        let cache = self.dice.find_projection_cache::<P>();

        let projection_key_as_key = ProjectionKeyAsKey {
            derive_from_key: derive_from.key().clone(),
            k: projection_key.clone(),
        };

        let extra = self.extra.subrequest(&projection_key_as_key)?;

        Ok(cache.eval_projection(
            &projection_key_as_key,
            derive_from,
            &self.transaction_ctx,
            extra,
        ))
    }

    pub(super) fn changed<K, I>(&self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = K> + Send + Sync + 'static,
    {
        let mut changes = self.transaction_ctx.changes();

        changed.into_iter().try_for_each(|k| {
            let dice = self.dice.dupe();
            changes.change(
                k.clone(),
                box (move |version| {
                    debug!(msg = "marking value as changed", version = %version, key = %k);
                    let cache = dice.find_cache::<K>();
                    cache.dirty(k, version, true);

                    true
                }),
            )
        })
    }

    pub(super) fn changed_to<K, I>(&self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = (K, K::Value)> + Send + Sync + 'static,
    {
        let mut changes = self.transaction_ctx.changes();

        changed.into_iter().try_for_each(|(k, v)| {
            let dice = self.dice.dupe();
            changes.change(
                k.clone(),
                box (move |version| {
                    let cache = dice.find_cache::<K>();
                    debug!(msg = "marking value as updated", version = %version, key = %k);
                    cache.update_injected_value(k, version, v)
                }),
            )
        })
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version.
    /// This can only be called when the this is the only node remaining in the computation graph
    pub(super) fn commit(self: Arc<Self>) -> DiceComputations {
        // TODO need to clean up these ctxs so we have less runtime errors from Arc references
        let this = Arc::try_unwrap(self)
            .map_err(|_| "Error: tried to commit when there are more references")
            .unwrap();
        let eval = Arc::try_unwrap(this.transaction_ctx)
            .map_err(|_| "Error: tried to commit when there are more references")
            .unwrap();

        // hold onto the prev version until we get the new one below so we don't increment minor
        // version needlessly.
        let _prev_v = eval.commit();

        this.dice.make_ctx(this.extra)
    }

    /// Same as `commit`, but replacing the user data with the given
    pub(super) fn commit_with_data(
        self: Arc<Self>,
        extra: UserComputationData,
    ) -> DiceComputations {
        // TODO need to clean up these ctxs so we have less runtime errors from Arc references
        let this = Arc::try_unwrap(self)
            .map_err(|_| "Error: tried to commit when there are more references")
            .unwrap();
        let eval = Arc::try_unwrap(this.transaction_ctx)
            .map_err(|_| "Error: tried to commit when there are more references")
            .unwrap();

        // hold onto the prev version until we get the new one below so we don't increment minor
        // version needlessly.
        let _prev_v = eval.commit();

        this.dice.make_ctx(ComputationData {
            user_data: Arc::new(extra),
            cycle_detector: this.extra.cycle_detector,
        })
    }

    pub(super) fn unstable_take(self: &Arc<Self>) -> DiceMap {
        self.dice.unstable_take()
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::api::cycles::DetectCycles;
    use crate::api::user_data::UserComputationData;
    use crate::ctx::ComputationData;

    pub(crate) trait ComputationDataExt {
        fn testing_new() -> Self;
    }

    impl ComputationDataExt for ComputationData {
        fn testing_new() -> Self {
            Self::new(UserComputationData::new(), DetectCycles::Enabled)
        }
    }
}
