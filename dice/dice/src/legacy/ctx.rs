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
use futures::future::BoxFuture;
use futures::future::Either;
use futures::FutureExt;
use more_futures::cancellation::CancellationContext;
use more_futures::spawn::spawn_cancellable;
use more_futures::spawn::spawn_dropcancel;
use more_futures::spawn::DropCancelAndTerminationObserver;
use more_futures::spawn::StrongJoinHandle;
use more_futures::spawn::WeakFutureError;

use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::data::DiceData;
use crate::api::error::DiceErrorImpl;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::projection::ProjectionKey;
use crate::api::transaction::DiceTransaction;
use crate::api::user_data::UserComputationData;
use crate::api::user_data::UserCycleDetectorGuard;
use crate::ctx::DiceComputationsImpl;
use crate::legacy::cycles::CycleDetector;
use crate::legacy::incremental::dep_trackers::BothDepTrackers;
use crate::legacy::incremental::dep_trackers::BothDeps;
use crate::legacy::incremental::graph::storage_properties::StorageProperties;
use crate::legacy::incremental::transaction_ctx::ActiveTransactionCountGuard;
use crate::legacy::incremental::transaction_ctx::Changes;
use crate::legacy::incremental::transaction_ctx::TransactionCtx;
use crate::legacy::incremental::versions::VersionForWrites;
use crate::legacy::incremental::versions::VersionGuard;
use crate::legacy::key::StoragePropertiesForKey;
use crate::legacy::map::DiceMap;
use crate::legacy::opaque::OpaqueValueImplLegacy;
use crate::legacy::projection::ProjectionKeyAsKey;
use crate::legacy::projection::ProjectionKeyProperties;
use crate::legacy::DiceLegacy;
use crate::versions::VersionNumber;
use crate::DiceError;
use crate::WhichSpawner;

/// A context for the duration of a top-level compute request.
///
/// This contains both user-visible and dice-internal computation-specific data.
#[derive(Allocative)]
pub(crate) struct ComputationData {
    pub(crate) user_data: Arc<UserComputationData>,
    cycle_detector: Option<Box<CycleDetector>>,
    // TODO(bobyf): this seems a natural place to gather some stats about the compute too
    #[allocative(skip)]
    pub(crate) user_cycle_detector_guard: Option<Box<dyn UserCycleDetectorGuard>>,
}

impl ComputationData {
    pub(crate) fn new(data: UserComputationData, detect_cycles: DetectCycles) -> Self {
        Self {
            user_data: Arc::new(data),
            cycle_detector: match detect_cycles {
                DetectCycles::Enabled => Some(Box::new(CycleDetector::new())),
                DetectCycles::Disabled => None,
            },
            user_cycle_detector_guard: None,
        }
    }

    /// records that we are entering the computation of another key as part of this main request
    /// i.e. computing key a, which during its evaluation requests key b, enters a new subrequest.
    pub(crate) fn subrequest<K>(&self, key: &K::Key) -> DiceResult<Self>
    where
        K: StorageProperties,
    {
        if let Some(v) = &self.user_cycle_detector_guard {
            v.add_edge(K::to_key_any(key));
        }
        Ok(Self {
            user_data: self.user_data.dupe(),
            cycle_detector: self
                .cycle_detector
                .as_ref()
                .map(|detector| Ok(Box::new(CycleDetector::visit(detector, key)?)))
                .transpose()?,
            user_cycle_detector_guard: None,
        })
    }

    pub(crate) fn start_computing_key<K: StorageProperties>(&mut self, k: &K::Key) {
        assert!(self.user_cycle_detector_guard.is_none());
        self.user_cycle_detector_guard = self
            .user_data
            .cycle_detector
            .as_ref()
            .and_then(|v| v.start_computing_key(K::to_key_any(k)));
    }

    pub(crate) fn finished_computing_key<K: StorageProperties>(
        user_data: &UserComputationData,
        k: &K::Key,
    ) {
        if let Some(v) = &user_data.cycle_detector {
            v.finished_computing_key(K::to_key_any(k))
        }
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
pub(crate) struct DiceComputationsImplLegacy {
    pub(crate) transaction_ctx: Arc<TransactionCtx>,
    pub(crate) dice: Arc<DiceLegacy>,
    pub(crate) dep_trackers: BothDepTrackers,
    pub(crate) extra: ComputationData,
}

impl DiceComputationsImplLegacy {
    pub(crate) fn new_transaction(
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

    pub(crate) fn new_for_key_evaluation(
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

    pub(crate) fn finalize(self: Arc<Self>) -> (BothDeps, ComputationData) {
        // TODO express this via lifetimes
        let this = Arc::try_unwrap(self).map_err(|_| "The computation lifetime of the `ctx` has ended and there should be no further references to the `Arc`").unwrap();

        (this.dep_trackers.collect_deps(), this.extra)
    }

    pub(crate) fn compute_opaque<'b, 'a: 'b, K>(
        self: &'a Arc<Self>,
        key: &'b K,
    ) -> impl Future<Output = DiceResult<OpaqueValueImplLegacy<'a, K>>> + 'b
    where
        K: Key,
    {
        // This would be simpler with an `async fn/async move {}`, but we create these for every edge in the computation
        // and many of those may be live at a time, and so we need to take more care and ensure this is fairly small.
        let cache = self.dice.find_cache::<K>();
        let extra = self.extra.subrequest::<StoragePropertiesForKey<K>>(key);
        match extra {
            Ok(extra) => cache
                .eval_for_opaque(key, &self.transaction_ctx, extra)
                .map(move |value| Ok(OpaqueValueImplLegacy::new(value?, self, cache)))
                .left_future(),
            Err(e) => futures::future::ready(Err(e)).right_future(),
        }
    }

    pub(crate) fn compute_projection_sync<P>(
        self: &Arc<Self>,
        derive_from: &OpaqueValueImplLegacy<P::DeriveFromKey>,
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

        let extra = self
            .extra
            .subrequest::<ProjectionKeyProperties<P>>(&projection_key_as_key)?;

        Ok(cache.eval_projection(
            &projection_key_as_key,
            derive_from,
            &self.transaction_ctx,
            &extra,
        ))
    }

    pub(crate) fn changed<K, I>(&self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = K> + Send + Sync + 'static,
    {
        let mut changes = self.transaction_ctx.changes();

        changed.into_iter().try_for_each(|k| {
            let dice = self.dice.dupe();
            changes.change(
                k.clone(),
                Box::new(move |version| {
                    debug!(msg = "marking value as changed", version = %version, key = %k);
                    let cache = dice.find_cache::<K>();
                    cache.dirty(k, version, true);

                    true
                }),
            )
        })
    }

    pub(crate) fn changed_to<K, I>(&self, changed: I) -> DiceResult<()>
    where
        K: Key,
        I: IntoIterator<Item = (K, K::Value)> + Send + Sync + 'static,
    {
        let mut changes = self.transaction_ctx.changes();

        changed.into_iter().try_for_each(|(k, v)| {
            if !K::validity(&v) {
                return Err(DiceError::invalid_change(Arc::new(k)));
            }
            let dice = self.dice.dupe();
            changes.change(
                k.clone(),
                Box::new(move |version| {
                    let cache = dice.find_cache::<K>();
                    debug!(msg = "marking value as updated", version = %version, key = %k);
                    cache.update_injected_value(k, version, v)
                }),
            )
        })
    }

    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version.
    /// This can only be called when the this is the only node remaining in the computation graph
    pub(crate) fn commit(self: Arc<Self>) -> Arc<DiceComputationsImplLegacy> {
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
    pub(crate) fn commit_with_data(
        self: Arc<Self>,
        extra: UserComputationData,
    ) -> Arc<DiceComputationsImplLegacy> {
        // TODO need to clean up these ctxs so we have less runtime errors from Arc references
        let mut this = Arc::try_unwrap(self)
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
            cycle_detector: this.extra.cycle_detector.take(),
            user_cycle_detector_guard: None,
        })
    }

    /// temporarily here while we figure out why dice isn't paralleling computations so that we can
    /// use this in tokio spawn. otherwise, this shouldn't be here so that we don't need to clone
    /// the Arc, which makes lifetimes weird.
    pub(crate) fn temporary_spawn<F, R>(
        self: &Arc<Self>,
        f: F,
    ) -> Either<
        StrongJoinHandle<BoxFuture<'static, Result<R, WeakFutureError>>>,
        DropCancelAndTerminationObserver<R>,
    >
    where
        F: for<'a> FnOnce(DiceTransaction, &'a CancellationContext) -> BoxFuture<'a, R>
            + Send
            + 'static,
        R: Send + 'static,
    {
        let duped = self.dupe();

        match self.dice.which_spawner {
            WhichSpawner::DropCancel => spawn_dropcancel(
                async move {
                    f(
                        DiceTransaction(DiceComputations(DiceComputationsImpl::Legacy(duped))),
                        &CancellationContext::todo(),
                    )
                    .await
                },
                self.extra.user_data.spawner.as_ref(),
                &self.extra.user_data,
                debug_span!(parent: None, "spawned_task",),
            )
            .left_future(),
            WhichSpawner::ExplicitCancel => spawn_cancellable(
                |cancellations| {
                    async move {
                        f(
                            DiceTransaction(DiceComputations(DiceComputationsImpl::Legacy(duped))),
                            cancellations,
                        )
                        .await
                    }
                    .boxed()
                },
                self.extra.user_data.spawner.as_ref(),
                &self.extra.user_data,
                debug_span!(parent: None, "spawned_task",),
            )
            .into_drop_cancel()
            .right_future(),
        }
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.transaction_ctx.get_version()
    }

    pub(crate) fn unstable_take(self: &Arc<Self>) -> DiceMap {
        self.dice.unstable_take()
    }

    pub(crate) fn global_data(&self) -> &DiceData {
        &self.dice.data
    }

    pub(crate) fn per_transaction_data(&self) -> &UserComputationData {
        &self.extra.user_data
    }

    pub(crate) fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<&T>> {
        match &self.extra.user_cycle_detector_guard {
            None => Ok(None),
            Some(guard) => match guard.as_any().downcast_ref() {
                Some(guard) => Ok(Some(guard)),
                None => Err(DiceError(Arc::new(
                    DiceErrorImpl::UnexpectedCycleGuardType {
                        expected_type_name: std::any::type_name::<T>().to_owned(),
                        actual_type_name: guard.type_name().to_owned(),
                    },
                ))),
            },
        }
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::api::cycles::DetectCycles;
    use crate::api::user_data::UserComputationData;
    use crate::ctx::DiceComputationsImpl;
    use crate::legacy::ctx::ComputationData;
    use crate::legacy::incremental::versions::MinorVersion;

    pub(crate) trait DiceCtxExt {
        fn get_minor_version(&self) -> MinorVersion;
    }

    impl DiceCtxExt for DiceComputationsImpl {
        fn get_minor_version(&self) -> MinorVersion {
            match self {
                DiceComputationsImpl::Legacy(delegate) => {
                    delegate.transaction_ctx.get_minor_version()
                }
                DiceComputationsImpl::Modern(_delegate) => {
                    unimplemented!("todo")
                }
            }
        }
    }

    pub(crate) trait ComputationDataExt {
        fn testing_new() -> Self;
    }

    impl ComputationDataExt for ComputationData {
        fn testing_new() -> Self {
            Self::new(UserComputationData::new(), DetectCycles::Enabled)
        }
    }
}
