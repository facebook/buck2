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
use std::ops::Deref;
use std::sync::Arc;

use futures::FutureExt;
use gazebo::prelude::*;
use more_futures::spawn::spawn_dropcancel;
use more_futures::spawner::Spawner;
use more_futures::spawner::TokioSpawner;

use crate::cycles::CycleDetector;
use crate::cycles::DetectCycles;
use crate::data::DiceData;
use crate::incremental::dep_trackers::BothDepTrackers;
use crate::incremental::dep_trackers::BothDeps;
use crate::incremental::transaction_ctx::TransactionCtx;
use crate::incremental::versions::MinorVersionGuard;
use crate::incremental::versions::VersionForWrites;
use crate::incremental::versions::VersionNumber;
use crate::map::DiceMap;
use crate::opaque::OpaqueValue;
use crate::projection::ProjectionKeyAsKey;
use crate::Dice;
use crate::DiceResult;
use crate::Key;
use crate::ProjectionKey;

/// Includes all user related computation-specific data.
pub struct UserComputationData {
    /// The DiceData provides a spot for users to attach whatever extra things they want.
    ///
    /// This can contain arbitrary data from users that will not be part of the dice graph.
    /// As an example, users may want to inject some form of event dispatcher to send events from their computations.
    pub data: DiceData,
    pub tracker: Arc<dyn DiceTracker>,
    pub spawner: Arc<dyn Spawner<Self>>,

    /// We require that UserComputationData always be constructed with `..Default::default()`
    pub _requires_default: RequireDefault,
}

pub struct RequireDefault(());

impl UserComputationData {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for UserComputationData {
    fn default() -> Self {
        Self {
            data: DiceData::new(),
            tracker: Arc::new(NoOpTracker),
            spawner: Arc::new(TokioSpawner::default()),
            _requires_default: RequireDefault(()),
        }
    }
}

/// A context for the duration of a top-level compute request.
///
/// This contains both user-visible and dice-internal computation-specific data.
pub(crate) struct ComputationData {
    pub(crate) user_data: Arc<UserComputationData>,
    cycle_detector: Option<Box<CycleDetector>>,
    // TODO(bobyf): this seems a natural place to gather some stats about the compute too
}

pub enum DiceEvent {
    Started { key_type: &'static str },
    Finished { key_type: &'static str },
}

pub trait DiceTracker: Send + Sync + 'static {
    fn event(&self, ev: DiceEvent);
}

struct NoOpTracker;

impl DiceTracker for NoOpTracker {
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
        K: Clone + Display + Debug + Eq + Hash + Send + Sync + 'static,
    {
        Ok(Self {
            user_data: self.user_data.dupe(),
            cycle_detector: self
                .cycle_detector
                .as_ref()
                .map(|detector| Ok(box detector.visit(key)?))
                .transpose()?,
        })
    }
}

/// The base struct for which all computations start. This is clonable, and dupe, and can be
/// moved to different runtimes to start computations.
/// All computations on this transaction will see only changes at the most-up-to-date version at
/// the time of creation of this transaction.
///
/// This SHOULD NOT be ever stored by computations, or any results of computations.
pub struct DiceTransaction(pub(super) DiceComputations);

impl DiceTransaction {
    /// Commit the changes registered via 'changed' and 'changed_to' to the current newest version.
    /// This can only be called when the this is the only node remaining in the computation graph
    pub fn commit(self) -> DiceTransaction {
        DiceTransaction(self.0.0.commit())
    }

    pub fn unstable_take(self) -> DiceMap {
        self.0.0.unstable_take()
    }
}

impl Deref for DiceTransaction {
    type Target = DiceComputations;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Clone for DiceTransaction {
    fn clone(&self) -> Self {
        Self(DiceComputations(self.0.0.dupe()))
    }
}

impl Dupe for DiceTransaction {}

/// The context for computations to register themselves, and request for additional dependencies.
/// The dependencies accessed are tracked for caching via the 'DiceCtx'.
///
/// The computations are registered onto 'DiceComputations' via implementing traits for the
/// 'DiceComputation'.
///
/// The context is valid only for the duration of the computation of a single key, and cannot be
/// owned.
pub struct DiceComputations(pub(super) Arc<DiceComputationImpl>);

impl DiceComputations {
    /// Gets all the result of of the given computation key.
    /// recorded as dependencies of the current computation for which this
    /// context is for.
    pub fn compute<'a, K>(
        &'a self,
        key: &'a K,
    ) -> impl Future<Output = DiceResult<<K as Key>::Value>> + 'a
    where
        K: Key,
    {
        self.compute_opaque(key)
            .map(|r| r.map(OpaqueValue::into_value))
    }

    /// same as `compute` but for a multiple keys. The returned results will be in order of the
    /// keys given
    pub fn compute_many<'a, K>(
        &'a self,
        keys: &[&'a K],
    ) -> Vec<impl Future<Output = DiceResult<<K as Key>::Value>> + 'a>
    where
        K: Key,
    {
        keys.map(|k| self.compute(*k))
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub fn compute_opaque<'a, K>(
        &'a self,
        key: &K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<'a, K>>> + 'a
    where
        K: Key,
    {
        self.0.compute_opaque(key)
    }

    /// records a set of 'Key's as changed so that they, and any dependents will
    /// be recomputed on the next set of requests at the next version.
    pub fn changed<K, I>(&self, changed: I)
    where
        K: Key,
        I: IntoIterator<Item = K> + Send + Sync + 'static,
    {
        self.0.changed(changed)
    }

    /// records a set of 'Key's as changed to a particular value so that any
    /// dependents will be recomputed on the next set of requests. The
    /// 'Key's themselves will be update to the new value such that they
    /// will not need to be recomputed as long as they aren't recorded to be
    /// `changed` again (or invalidated by other means). Calling this method
    /// does not in anyway alter the types of the key such that they
    /// permanently becomes a special "inject value only" key.
    pub fn changed_to<K, I>(&self, changed: I)
    where
        K: Key,
        I: IntoIterator<Item = (K, K::Value)> + Send + Sync + 'static,
    {
        self.0.changed_to(changed)
    }

    /// temporarily here while we figure out why dice isn't paralleling computations so that we can
    /// use this in tokio spawn. otherwise, this shouldn't be here so that we don't need to clone
    /// the Arc, which makes lifetimes weird.
    pub fn temporary_spawn<F, FUT, R>(&self, f: F) -> impl Future<Output = R> + Send + 'static
    where
        F: FnOnce(DiceTransaction) -> FUT + Send + 'static,
        FUT: Future<Output = R> + Send,
        R: Send + 'static,
    {
        // Don't make this function async. It should perform the spawn without needing to poll the returned future.
        let duped = DiceTransaction(DiceComputations(self.0.dupe()));

        spawn_dropcancel(
            async move { f(duped).await },
            self.0.extra.user_data.spawner.dupe(),
            &self.0.extra.user_data,
            debug_span!(parent: None, "spawned_task",),
        )
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub fn global_data(&self) -> &DiceData {
        &self.0.dice.data
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level 'DiceComputation' used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub fn per_transaction_data(&self) -> &UserComputationData {
        &self.0.extra.user_data
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
pub(crate) struct DiceComputationImpl {
    pub(crate) transaction_ctx: Arc<TransactionCtx>,
    pub(crate) dice: Arc<Dice>,
    pub(crate) dep_trackers: BothDepTrackers,
    pub(crate) extra: ComputationData,
}

impl DiceComputationImpl {
    pub(super) fn new_transaction(
        dice: Arc<Dice>,
        version: (VersionNumber, MinorVersionGuard),
        version_for_writes: VersionForWrites,
        extra: ComputationData,
    ) -> Self {
        Self {
            transaction_ctx: Arc::new(TransactionCtx::new(version, version_for_writes, Vec::new())),
            dep_trackers: BothDepTrackers::noop(),
            dice: dice.dupe(),
            extra,
        }
    }

    pub(super) fn from_transaction_ctx(
        dice: Arc<Dice>,
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

    pub(super) fn compute_opaque<'a, K>(
        self: &'a Arc<Self>,
        key: &K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<'a, K>>> + 'a
    where
        K: Key,
    {
        let key = key.clone();
        async move {
            let cache = self.dice.find_cache::<K>();
            let extra = self.extra.subrequest(&key)?;
            let value = cache
                .eval_for_opaque(&key, &self.transaction_ctx, extra)
                .await?;
            Ok(OpaqueValue::new(key, value, self, cache))
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
            derive_from_key: derive_from.key.clone(),
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

    pub(super) fn changed<K, I>(&self, changed: I)
    where
        K: Key,
        I: IntoIterator<Item = K> + Send + Sync + 'static,
    {
        let mut changes = self.transaction_ctx.changes();

        changed.into_iter().for_each(|k| {
            let dice = self.dice.dupe();
            changes.push(
                box (move |version| {
                    debug!(msg = "marking value as changed", version = %version, key = %k);
                    let cache = dice.find_cache::<K>();
                    cache.dirty(k, version, true);
                }),
            )
        });
    }

    pub(super) fn changed_to<K, I>(&self, changed: I)
    where
        K: Key,
        I: IntoIterator<Item = (K, K::Value)> + Send + Sync + 'static,
    {
        let mut changes = self.transaction_ctx.changes();

        changed.into_iter().for_each(|(k, v)| {
            let dice = self.dice.dupe();
            changes.push(
                box (move |version| {
                    let cache = dice.find_cache::<K>();
                    debug!(msg = "marking value as updated", version = %version, key = %k);
                    cache.update(k, version, v);
                }),
            )
        });
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

        eval.commit();

        this.dice.make_ctx(this.extra)
    }

    pub(super) fn unstable_take(self: Arc<Self>) -> DiceMap {
        self.dice.unstable_take()
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::ctx::ComputationData;
    use crate::cycles::DetectCycles;
    use crate::incremental::versions::MinorVersion;
    use crate::incremental::versions::VersionNumber;
    use crate::DiceComputations;
    use crate::UserComputationData;

    pub(crate) trait DiceCtxExt {
        fn get_version(&self) -> VersionNumber;
        fn get_minor_version(&self) -> MinorVersion;
    }

    impl DiceCtxExt for DiceComputations {
        fn get_version(&self) -> VersionNumber {
            self.0.transaction_ctx.get_version()
        }

        fn get_minor_version(&self) -> MinorVersion {
            self.0.transaction_ctx.get_minor_version()
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use derive_more::Display;
    use gazebo::prelude::*;
    use indexmap::indexset;

    use crate::ctx::ComputationData;
    use crate::cycles::DetectCycles;
    use crate::DiceErrorImpl;
    use crate::RequestedKey;
    use crate::UserComputationData;

    #[derive(Clone, Dupe, Display, Debug, PartialEq, Eq, Hash)]
    struct K(usize);

    #[test]
    fn cycle_detection_when_no_cycles() -> anyhow::Result<()> {
        let ctx = ComputationData::new(UserComputationData::new(), DetectCycles::Enabled);
        let ctx = ctx.subrequest(&K(1))?;
        let ctx = ctx.subrequest(&K(2))?;
        let ctx = ctx.subrequest(&K(3))?;
        let _ctx = ctx.subrequest(&K(4))?;

        Ok(())
    }

    #[test]
    fn cycle_detection_when_cycles() -> anyhow::Result<()> {
        let ctx = ComputationData::new(UserComputationData::new(), DetectCycles::Enabled);
        let ctx = ctx.subrequest(&K(1))?;
        let ctx = ctx.subrequest(&K(2))?;
        let ctx = ctx.subrequest(&K(3))?;
        let ctx = ctx.subrequest(&K(4))?;
        match ctx.subrequest(&K(1)) {
            Ok(_) => {
                panic!("should have cycle error")
            }
            Err(e) => match &*e.0 {
                DiceErrorImpl::Cycle {
                    trigger,
                    cyclic_keys,
                } => {
                    assert!(
                        (**trigger).get_key_equality() == K(1).get_key_equality(),
                        "expected trigger key to be `{}` but was `{}`",
                        K(1),
                        trigger
                    );
                    assert_eq!(
                        &*cyclic_keys,
                        &indexset![
                            Arc::new(K(1)) as Arc<dyn RequestedKey>,
                            Arc::new(K(2)) as Arc<dyn RequestedKey>,
                            Arc::new(K(3)) as Arc<dyn RequestedKey>,
                            Arc::new(K(4)) as Arc<dyn RequestedKey>
                        ]
                    )
                }
            },
        }

        Ok(())
    }
}
