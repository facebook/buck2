/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::any::Any;
use std::future::Future;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::Arc;

use allocative::Allocative;
use derivative::Derivative;
use dice_error::DiceError;
use dice_error::DiceResult;
use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dice_futures::cancellation::CancellationContext;
use dice_futures::owning_future::OwningFuture;
use dice_futures::spawn::spawn_dropcancel;
use dupe::Dupe;
use futures::FutureExt;
use futures::TryFutureExt;
use futures::future::BoxFuture;
use itertools::Either;
use parking_lot::Mutex;
use typed_arena::Arena;

use crate::DiceTransactionUpdater;
use crate::LinearRecomputeDiceComputations;
use crate::UserCycleDetectorGuard;
use crate::api::activation_tracker::ActivationData;
use crate::api::computations::DiceComputations;
use crate::api::computations::DiceComputationsData;
use crate::api::data::DiceData;
use crate::api::invalidation_tracking::DiceKeyTrackedInvalidationPaths;
use crate::api::key::Key;
use crate::api::projection::ProjectionKey;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationsImpl;
use crate::ctx::LinearRecomputeDiceComputationsImpl;
use crate::impls::cache::DiceTaskRef;
use crate::impls::cache::SharedCache;
use crate::impls::core::state::CoreStateHandle;
use crate::impls::core::versions::VersionEpoch;
use crate::impls::deps::RecordedDeps;
use crate::impls::deps::RecordingDepsTracker;
use crate::impls::dice::Dice;
use crate::impls::evaluator::AsyncEvaluator;
use crate::impls::evaluator::SyncEvaluator;
use crate::impls::events::DiceEventDispatcher;
use crate::impls::key::CowDiceKeyHashed;
use crate::impls::key::DiceKey;
use crate::impls::key::ParentKey;
use crate::impls::opaque::OpaqueValueModern;
use crate::impls::task::PreviouslyCancelledTask;
use crate::impls::task::promise::DicePromise;
use crate::impls::task::sync_dice_task;
use crate::impls::transaction::ActiveTransactionGuard;
use crate::impls::transaction::TransactionUpdater;
use crate::impls::user_cycle::KeyComputingUserCycleDetectorData;
use crate::impls::user_cycle::UserCycleDetectorData;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::TrackedInvalidationPaths;
use crate::impls::worker::DiceTaskWorker;
use crate::impls::worker::project_for_key;
use crate::transaction_update::DiceTransactionUpdaterImpl;
use crate::versions::VersionNumber;

/// Context that is the base for which all requests start from
#[derive(Allocative)]
pub(crate) struct BaseComputeCtx {
    // we need to give off references of `DiceComputation` so hold this for now, but really once we
    // get rid of the enum, we just hold onto the base data directly and do some ref casts
    data: DiceComputations<'static>,
    live_version_guard: ActiveTransactionGuard,
}

impl Clone for BaseComputeCtx {
    fn clone(&self) -> Self {
        BaseComputeCtx::clone_for(&self.data.0.0, self.live_version_guard.dupe())
    }
}

impl Dupe for BaseComputeCtx {}

impl BaseComputeCtx {
    pub(crate) fn new(
        per_live_version_ctx: SharedLiveTransactionCtx,
        user_data: Arc<UserComputationData>,
        dice: Arc<Dice>,
        live_version_guard: ActiveTransactionGuard,
    ) -> Self {
        Self {
            data: DiceComputations(DiceComputationsImpl(ModernComputeCtx::new(
                ParentKey::None,
                KeyComputingUserCycleDetectorData::Untracked,
                AsyncEvaluator {
                    per_live_version_ctx,
                    user_data,
                    dice,
                },
            ))),
            live_version_guard,
        }
    }

    fn clone_for(
        modern: &ModernComputeCtx<'_>,
        live_version_guard: ActiveTransactionGuard,
    ) -> BaseComputeCtx {
        Self {
            data: DiceComputations(DiceComputationsImpl(ModernComputeCtx::new(
                ParentKey::None,
                KeyComputingUserCycleDetectorData::Untracked,
                modern.ctx_data().async_evaluator.clone(),
            ))),
            live_version_guard,
        }
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.data.0.get_version()
    }

    pub(crate) fn into_updater(self) -> DiceTransactionUpdater {
        DiceTransactionUpdater(DiceTransactionUpdaterImpl(self.data.0.0.into_updater()))
    }

    pub(crate) fn as_computations(&self) -> &DiceComputations<'static> {
        &self.data
    }

    pub(crate) fn as_computations_mut(&mut self) -> &mut DiceComputations<'static> {
        &mut self.data
    }
}

impl Deref for BaseComputeCtx {
    type Target = ModernComputeCtx<'static>;

    fn deref(&self) -> &Self::Target {
        &self.data.0.0
    }
}

impl DerefMut for BaseComputeCtx {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data.0.0
    }
}

impl ModernComputeCtx<'_> {
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
        let (ctx_data, dep_trackers) = self.unpack();
        Self::compute_opaque_impl(ctx_data, key)
            .map(move |r| r.map(|opaque| Self::opaque_into_value_impl(dep_trackers, opaque)))
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque<'a, K>(
        &'a self,
        key: &K,
    ) -> impl Future<Output = DiceResult<OpaqueValueModern<K>>> + use<'a, K>
    where
        K: Key,
    {
        Self::compute_opaque_impl(self.ctx_data(), key)
    }

    fn compute_opaque_impl<K>(
        ctx_data: &CoreCtx,
        key: &K,
    ) -> impl Future<Output = DiceResult<OpaqueValueModern<K>>> + use<K>
    where
        K: Key,
    {
        ctx_data.compute_opaque(key).map(move |cancellable_result| {
            let cancellable = cancellable_result.map(move |(dice_key, dice_value)| {
                let (value, invalidation_paths) = dice_value.into_parts();
                OpaqueValueModern::new(dice_key, value, invalidation_paths)
            });

            cancellable.map_err(DiceError::cancelled)
        })
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
        let iter = computes.into_iter();
        let parallel = self.parallel_builder(iter.size_hint().0);
        iter.map(|func| parallel.compute(func)).collect()
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
        let parallel = self.parallel_builder(2);
        (parallel.compute(compute1), parallel.compute(compute2))
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
        let parallel = self.parallel_builder(3);

        (
            parallel.compute(compute1),
            parallel.compute(compute2),
            parallel.compute(compute3),
        )
    }

    pub(crate) fn with_linear_recompute<'a, Func, Fut, T>(
        &'a mut self,
        func: Func,
    ) -> impl Future<Output = T> + use<'a, Func, Fut, T>
    where
        Func: FnOnce(LinearRecomputeDiceComputations<'a>) -> Fut,
        Fut: Future<Output = T>,
    {
        let (ctx_data, self_dep_trackers) = self.unpack();
        let dep_trackers = Arc::new(Mutex::new(RecordingDepsTracker::new(
            // TODO(cjhopman): if inspected during the with_linear_recompute, this will be missing some invalidation paths.
            TrackedInvalidationPaths::clean(),
        )));
        let fut = func(LinearRecomputeDiceComputations(
            LinearRecomputeDiceComputationsImpl(LinearRecomputeModern {
                ctx_data,
                dep_trackers: dep_trackers.dupe(),
            }),
        ));

        fut.map(move |v| {
            let mut self_dep_trackers = self_dep_trackers.lock();
            let dep_trackers = Arc::into_inner(dep_trackers)
                .unwrap()
                .into_inner()
                .collect_deps();
            let validity = dep_trackers.deps_validity;
            for k in dep_trackers.deps.iter_keys() {
                self_dep_trackers.record(k, validity, TrackedInvalidationPaths::clean())
            }
            self_dep_trackers.update_invalidation_paths(dep_trackers.invalidation_paths.dupe());
            v
        })
    }

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
        let (ctx_data, self_dep_trackers) = self.unpack();
        let mut inner_ctx: DiceComputations<'static> =
            DiceComputations(DiceComputationsImpl(ModernComputeCtx::new(
                ctx_data.parent_key,
                ctx_data.cycles.clone(),
                ctx_data.async_evaluator.dupe(),
            )));

        let user_data = ctx_data.per_transaction_data();
        let spawner = user_data.spawner.dupe();
        let ctx_data = user_data.dupe();

        let task = spawn_dropcancel(
            |cancellation| {
                async move {
                    let res = closure(&mut inner_ctx, cancellation).await;
                    let dep_trackers = inner_ctx.0.0.into_owned().1;
                    (res, dep_trackers)
                }
                .boxed()
            },
            &*spawner,
            ctx_data,
        );

        task.map(move |(res, dep_trackers)| {
            let deps = dep_trackers.collect_deps();
            let validity = deps.deps_validity;
            let mut self_dep_trackers = self_dep_trackers.lock();
            for k in deps.deps.iter_keys() {
                self_dep_trackers.record(k, validity, TrackedInvalidationPaths::clean())
            }
            self_dep_trackers.update_invalidation_paths(deps.invalidation_paths.dupe());

            res
        })
    }

    pub(crate) fn opaque_into_value<K: Key>(&mut self, opaque: OpaqueValueModern<K>) -> K::Value {
        Self::opaque_into_value_impl(self.unpack().1, opaque)
    }

    fn opaque_into_value_impl<K: Key>(
        deps: DepsTrackerHolder,
        opaque: OpaqueValueModern<K>,
    ) -> K::Value {
        let OpaqueValueModern {
            derive_from_key,
            derive_from,
            invalidation_paths,
            ..
        } = opaque;

        deps.lock()
            .record(derive_from_key, derive_from.validity(), invalidation_paths);

        derive_from
            .downcast_maybe_transient::<K::Value>()
            .expect("type mismatch")
            .dupe()
    }

    pub(crate) fn get_invalidation_paths(&mut self) -> DiceKeyTrackedInvalidationPaths {
        let (normal, high) = {
            let mut dep_trackers = self.dep_trackers();
            let paths = dep_trackers.invalidation_paths();
            (paths.get_normal(), paths.get_high())
        };
        DiceKeyTrackedInvalidationPaths::new(
            self.ctx_data().async_evaluator.dice.dupe(),
            normal,
            high,
        )
    }

    pub(crate) fn data(&self) -> DiceComputationsData {
        DiceComputationsData(ModernDiceComputationsData(
            self.ctx_data().async_evaluator.dupe(),
        ))
    }
}

impl<'a> From<ModernComputeCtx<'a>> for DiceComputations<'a> {
    fn from(value: ModernComputeCtx<'a>) -> Self {
        DiceComputations(DiceComputationsImpl(value))
    }
}

pub(crate) struct LinearRecomputeModern<'a> {
    ctx_data: &'a CoreCtx,
    dep_trackers: Arc<Mutex<RecordingDepsTracker>>,
}

impl LinearRecomputeModern<'_> {
    pub(crate) fn get(&self) -> DiceComputations<'_> {
        DiceComputations(DiceComputationsImpl(ModernComputeCtx::Linear {
            ctx_data: self.ctx_data,
            dep_trackers: &self.dep_trackers,
        }))
    }
}

/// This is used to create the ctx for each individual parallel compute (from compute_many/compute_join/compute2/etc).
///
/// For the Normal case, each parallel ctx will be expected to record its deps into a RecordedDeps allocated in the arena.
///
/// For the Linear case, each parallel ctx will record deps into the shared RecordingDepsTracker.
pub(crate) enum ModernComputeCtxParallelBuilder<'a> {
    Normal {
        ctx_data: &'a CoreCtx,
        tracker_arena: &'a Arena<RecordedDeps>,
        invalidation_paths: &'a TrackedInvalidationPaths,
    },
    Linear {
        ctx_data: &'a CoreCtx,
        dep_trackers: &'a Mutex<RecordingDepsTracker>,
    },
}
impl<'a> ModernComputeCtxParallelBuilder<'a> {
    fn compute<F, T>(&self, func: F) -> impl Future<Output = T> + use<'a, F, T>
    where
        F: for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
    {
        match self {
            ModernComputeCtxParallelBuilder::Normal {
                ctx_data,
                tracker_arena,
                invalidation_paths,
            } => OwningFuture::new(
                (
                    tracker_arena.alloc(RecordedDeps::new()),
                    ModernComputeCtx::Parallel {
                        ctx_data,
                        dep_trackers: RecordingDepsTracker::new((*invalidation_paths).dupe()),
                    }
                    .into(),
                ),
                |(_, ctx)| func(ctx),
            )
            .map_taking_data(|v, (this_deps, ctx)| match ctx.0.0 {
                ModernComputeCtx::Parallel { dep_trackers, .. } => {
                    *this_deps = dep_trackers.collect_deps();
                    v
                }
                _ => unreachable!(),
            })
            .left_future(),
            ModernComputeCtxParallelBuilder::Linear {
                ctx_data,
                dep_trackers,
            } => OwningFuture::new(
                ModernComputeCtx::Linear {
                    ctx_data,
                    dep_trackers,
                }
                .into(),
                func,
            )
            .right_future(),
        }
    }
}

/// A holder for the user data attached to DICE.
#[derive(Clone, Dupe)]
pub struct ModernDiceComputationsData(AsyncEvaluator);

impl ModernDiceComputationsData {
    pub fn global_data(&self) -> &DiceData {
        &self.0.dice.global_data
    }

    pub fn per_transaction_data(&self) -> &UserComputationData {
        &self.0.user_data
    }
}

/// Context given to the `compute` function of a `Key`.
#[derive(Allocative)]
pub(crate) enum ModernComputeCtx<'a> {
    /// The initial ctx for a key computation.
    Owned {
        ctx_data: CoreCtx,
        dep_trackers: RecordingDepsTracker,
    },
    /// The ctx within a compute_many/compute_join/try_compute_join.
    Parallel {
        #[allocative(skip)]
        ctx_data: &'a CoreCtx,
        #[allocative(skip)]
        dep_trackers: RecordingDepsTracker,
    },
    /// The ctx within a with_linear_recompute.
    Linear {
        #[allocative(skip)]
        ctx_data: &'a CoreCtx,
        #[allocative(skip)]
        dep_trackers: &'a Mutex<RecordingDepsTracker>,
    },
}

#[derive(Allocative)]
pub(crate) struct CoreCtx {
    async_evaluator: AsyncEvaluator,
    parent_key: ParentKey,
    #[allocative(skip)]
    cycles: KeyComputingUserCycleDetectorData,
    // data for the entire compute of a Key, including parallel computes
    #[allocative(skip)]
    evaluation_data: Mutex<EvaluationData>,
}

impl ModernComputeCtx<'static> {
    fn into_owned(self) -> (CoreCtx, RecordingDepsTracker) {
        match self {
            ModernComputeCtx::Owned {
                ctx_data,
                dep_trackers,
            } => (ctx_data, dep_trackers),
            _ => unreachable!(),
        }
    }
    pub(crate) fn finalize(
        self,
    ) -> (
        RecordedDeps,
        EvaluationData,
        KeyComputingUserCycleDetectorData,
    ) {
        let (data, dep_trackers) = self.into_owned();
        (
            dep_trackers.collect_deps(),
            data.evaluation_data.into_inner(),
            data.cycles,
        )
    }

    pub(crate) fn into_updater(self) -> TransactionUpdater {
        self.into_owned().0.into_updater()
    }
}

struct DepsTrackerHolder<'a>(Either<&'a mut RecordingDepsTracker, &'a Mutex<RecordingDepsTracker>>);
impl<'a> DepsTrackerHolder<'a> {
    fn lock(self) -> impl DerefMut<Target = RecordingDepsTracker> {
        self.0.map_right(|v| v.lock())
    }
}

impl ModernComputeCtx<'_> {
    pub(crate) fn new(
        parent_key: ParentKey,
        cycles: KeyComputingUserCycleDetectorData,
        async_evaluator: AsyncEvaluator,
    ) -> ModernComputeCtx<'static> {
        ModernComputeCtx::Owned {
            dep_trackers: RecordingDepsTracker::new(TrackedInvalidationPaths::clean()),
            ctx_data: CoreCtx {
                async_evaluator,
                parent_key,
                cycles,
                evaluation_data: Mutex::new(EvaluationData::none()),
            },
        }
    }

    fn parallel_builder(&mut self, size_hint: usize) -> ModernComputeCtxParallelBuilder<'_> {
        match self {
            ModernComputeCtx::Owned {
                ctx_data,
                dep_trackers,
            } => {
                let (tracker_arena, invalidation_paths) = dep_trackers.push_parallel(size_hint);
                ModernComputeCtxParallelBuilder::Normal {
                    ctx_data,
                    tracker_arena,
                    invalidation_paths,
                }
            }
            ModernComputeCtx::Parallel {
                ctx_data,
                dep_trackers,
            } => {
                let (tracker_arena, invalidation_paths) = dep_trackers.push_parallel(size_hint);
                ModernComputeCtxParallelBuilder::Normal {
                    ctx_data,
                    tracker_arena,
                    invalidation_paths,
                }
            }
            ModernComputeCtx::Linear {
                ctx_data,
                dep_trackers,
            } => ModernComputeCtxParallelBuilder::Linear {
                ctx_data,
                dep_trackers,
            },
        }
    }

    fn ctx_data(&self) -> &CoreCtx {
        match self {
            ModernComputeCtx::Owned { ctx_data, .. } => ctx_data,
            ModernComputeCtx::Parallel { ctx_data, .. } => ctx_data,
            ModernComputeCtx::Linear { ctx_data, .. } => ctx_data,
        }
    }

    fn unpack(&mut self) -> (&CoreCtx, DepsTrackerHolder<'_>) {
        match self {
            ModernComputeCtx::Owned {
                ctx_data,
                dep_trackers,
            } => (ctx_data, DepsTrackerHolder(Either::Left(dep_trackers))),
            ModernComputeCtx::Parallel {
                ctx_data,
                dep_trackers,
            } => (
                ctx_data,
                DepsTrackerHolder(Either::Left(&mut *dep_trackers)),
            ),
            ModernComputeCtx::Linear {
                ctx_data,
                dep_trackers,
            } => (ctx_data, DepsTrackerHolder(Either::Right(dep_trackers))),
        }
    }

    /// Compute "projection" based on deriving value
    pub(crate) fn projection<K: Key, P: ProjectionKey<DeriveFromKey = K>>(
        &mut self,
        derive_from: &OpaqueValueModern<K>,
        key: &P,
    ) -> DiceResult<P::Value> {
        let (ctx_data, dep_trackers) = self.unpack();
        ctx_data.project(key, derive_from, dep_trackers)
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub(crate) fn global_data(&self) -> &DiceData {
        self.ctx_data().global_data()
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub(crate) fn per_transaction_data(&self) -> &UserComputationData {
        self.ctx_data().per_transaction_data()
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.ctx_data().get_version()
    }

    #[allow(unused)] // used in test
    pub(super) fn dep_trackers(&mut self) -> impl DerefMut<Target = RecordingDepsTracker> {
        self.unpack().1.lock()
    }

    pub(crate) fn store_evaluation_data<T: Send + Sync + 'static>(
        &self,
        value: T,
    ) -> DiceResult<()> {
        self.ctx_data().store_evaluation_data(value)
    }

    pub(crate) fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<Arc<T>>> {
        self.ctx_data().cycle_guard()
    }
}

impl CoreCtx {
    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque<K>(
        &self,
        key: &K,
    ) -> impl Future<Output = CancellableResult<(DiceKey, DiceComputedValue)>> + use<K>
    where
        K: Key,
    {
        let dice_key = self
            .async_evaluator
            .dice
            .key_index
            .index(CowDiceKeyHashed::key_ref(key));

        self.async_evaluator
            .per_live_version_ctx
            .compute_opaque(
                dice_key,
                self.parent_key,
                &self.async_evaluator,
                self.cycles
                    .subrequest(dice_key, &self.async_evaluator.dice.key_index),
            )
            .map_ok(move |res| (dice_key, res))
    }

    /// Compute "projection" based on deriving value
    fn project<B: Key, K: ProjectionKey<DeriveFromKey = B>>(
        &self,
        key: &K,
        base: &OpaqueValueModern<B>,
        dep_trackers: DepsTrackerHolder,
    ) -> DiceResult<K::Value> {
        let dice_key = self
            .async_evaluator
            .dice
            .key_index
            .index(CowDiceKeyHashed::proj_ref(base.derive_from_key, key));

        let r = self
            .async_evaluator
            .per_live_version_ctx
            .compute_projection(
                dice_key,
                self.parent_key,
                self.async_evaluator.dice.state_handle.dupe(),
                SyncEvaluator::new(
                    self.async_evaluator.user_data.dupe(),
                    self.async_evaluator.dice.dupe(),
                    base.derive_from.dupe(),
                    base.invalidation_paths.dupe(),
                ),
                DiceEventDispatcher::new(
                    self.async_evaluator.user_data.tracker.dupe(),
                    self.async_evaluator.dice.dupe(),
                ),
            );

        let r = match r {
            Ok(r) => r,
            Err(reason) => return Err(DiceError::cancelled(reason)),
        };

        dep_trackers.lock().record(
            dice_key,
            r.value().validity(),
            r.invalidation_paths().dupe(),
        );

        Ok(r.value()
            .downcast_maybe_transient::<K::Value>()
            .expect("Type mismatch when computing key")
            .dupe())
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub(crate) fn global_data(&self) -> &DiceData {
        &self.async_evaluator.dice.global_data
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub(crate) fn per_transaction_data(&self) -> &UserComputationData {
        &self.async_evaluator.user_data
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.async_evaluator.per_live_version_ctx.get_version()
    }

    pub(crate) fn into_updater(self) -> TransactionUpdater {
        TransactionUpdater::new(
            self.async_evaluator.dice.dupe(),
            self.async_evaluator.user_data.dupe(),
        )
    }

    pub(crate) fn store_evaluation_data<T: Send + Sync + 'static>(
        &self,
        value: T,
    ) -> DiceResult<()> {
        let mut evaluation_data = self.evaluation_data.lock();
        if evaluation_data.0.is_some() {
            return Err(DiceError::duplicate_activation_data());
        }
        evaluation_data.0 = Some(Box::new(value) as _);
        Ok(())
    }

    pub(crate) fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<Arc<T>>> {
        self.cycles.cycle_guard()
    }
}

/// Context that is shared for all current live computations of the same version.
#[derive(Allocative, Derivative, Dupe, Clone)]
#[derivative(Debug)]
pub(crate) struct SharedLiveTransactionCtx {
    version: VersionNumber,
    version_epoch: VersionEpoch,
    #[derivative(Debug = "ignore")]
    cache: SharedCache,
}

#[allow(clippy::manual_async_fn, unused)]
impl SharedLiveTransactionCtx {
    pub(crate) fn new(v: VersionNumber, version_epoch: VersionEpoch, cache: SharedCache) -> Self {
        Self {
            version: v,
            version_epoch,
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
        eval: &AsyncEvaluator,
        cycles: UserCycleDetectorData,
    ) -> impl Future<Output = CancellableResult<DiceComputedValue>> + use<> {
        let res: CancellableResult<DicePromise> = match self.cache.get(key) {
            DiceTaskRef::Computed(result) => Ok(DicePromise::ready(result)),
            DiceTaskRef::Occupied(mut occupied) => {
                match occupied.get().depended_on_by(parent_key) {
                    Ok(promise) => {
                        debug!(msg = "shared state is waiting on existing task", k = ?key, v = ?self.version, v_epoch = ?self.version_epoch);
                        Ok(promise)
                    }
                    Err(_reason) => {
                        debug!(msg = "shared state has a cancelled task, spawning new one", k = ?key, v = ?self.version, v_epoch = ?self.version_epoch);

                        let eval = eval.dupe();
                        let events = DiceEventDispatcher::new(
                            eval.user_data.tracker.dupe(),
                            eval.dice.dupe(),
                        );

                        take_mut::take(occupied.get_mut(), |previous| {
                            DiceTaskWorker::spawn(
                                key,
                                self.version_epoch,
                                eval,
                                cycles,
                                events,
                                Some(PreviouslyCancelledTask { previous }),
                            )
                        });

                        // While we wouldn't have canceled the task, it could've already finished with a canceled result.
                        occupied.get().depended_on_by(parent_key)
                    }
                }
            }
            DiceTaskRef::Vacant(vacant) => {
                debug!(msg = "shared state is empty, spawning new task", k = ?key, v = ?self.version, v_epoch = ?self.version_epoch);

                let eval = eval.dupe();
                let events =
                    DiceEventDispatcher::new(eval.user_data.tracker.dupe(), eval.dice.dupe());

                let task =
                    DiceTaskWorker::spawn(key, self.version_epoch, eval, cycles, events, None);

                // While we wouldn't have canceled the task, it could've already finished with a canceled result.
                let result = task.depended_on_by(parent_key);
                if result.is_ok() {
                    vacant.insert(task);
                }
                result
            }
            DiceTaskRef::TransactionCancelled => Err(CancellationReason::TransactionCancelled),
        };

        match res {
            Ok(v) => v.left_future(),
            Err(reason) => {
                let v = self.version;
                let v_epoch = self.version_epoch;
                async move {
                    debug!(msg = "computing shared state is cancelled", k = ?key, v = ?v, v_epoch = ?v_epoch);
                    tokio::task::yield_now().await;
                    Err(reason)
                }
                    .right_future()
            }
        }
    }

    /// Compute "projection" based on deriving value
    pub(crate) fn compute_projection(
        &self,
        key: DiceKey,
        parent_key: ParentKey,
        state: CoreStateHandle,
        eval: SyncEvaluator,
        events: DiceEventDispatcher,
    ) -> CancellableResult<DiceComputedValue> {
        let result = match self.cache.get(key) {
            DiceTaskRef::Computed(value) => DicePromise::ready(value),
            DiceTaskRef::Occupied(mut occupied) => {
                match occupied.get().depended_on_by(parent_key) {
                    Ok(promise) => promise,
                    Err(_reason) => {
                        let task = unsafe {
                            // SAFETY: task completed below by `IncrementalEngine::project_for_key`
                            sync_dice_task(key)
                        };

                        *occupied.get_mut() = task;

                        occupied.get().depended_on_by(parent_key)?
                    }
                }
            }
            DiceTaskRef::Vacant(vacant) => {
                let task = unsafe {
                    // SAFETY: task completed below by `IncrementalEngine::project_for_key`
                    sync_dice_task(key)
                };

                vacant.insert(task).value().depended_on_by(parent_key)?
            }
            DiceTaskRef::TransactionCancelled => {
                // for projection keys, these are cheap and synchronous computes that should never
                // be cancelled
                let task = unsafe {
                    // SAFETY: task completed below by `IncrementalEngine::project_for_key`
                    sync_dice_task(key)
                };

                task.depended_on_by(parent_key)?
            }
        };

        project_for_key(
            state,
            result,
            key,
            self.version,
            self.version_epoch,
            eval,
            events,
        )
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.version
    }
}

/// Opaque data that the key may have provided during evalution via store_evaluation_data.
pub(crate) struct EvaluationData(Option<Box<dyn Any + Send + Sync + 'static>>);

impl EvaluationData {
    pub(crate) fn none() -> Self {
        Self(None)
    }

    pub(crate) fn into_activation_data(self) -> ActivationData {
        ActivationData::Evaluated(self.0)
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::impls::cache::DiceTaskRef;
    use crate::impls::core::versions::VersionEpoch;
    use crate::impls::ctx::SharedLiveTransactionCtx;
    use crate::impls::key::DiceKey;
    use crate::impls::key::ParentKey;
    use crate::impls::task::promise::DiceSyncResult;
    use crate::impls::task::sync_dice_task;
    use crate::impls::value::DiceComputedValue;

    impl SharedLiveTransactionCtx {
        pub(crate) fn inject(&self, k: DiceKey, v: DiceComputedValue) {
            // TODO(cjhopman): We should delete this. tests using it are doing weird things and
            // causing the transaction cache to be out of sync with what is possible in real
            // execution and it makes things really difficult to reason about. These tests
            // should be constructing the states they want to test via valid interactions
            // with things.
            let task = unsafe {
                // SAFETY: completed immediately below
                sync_dice_task(k)
            };
            let _r = task
                .depended_on_by(ParentKey::None)
                .unwrap()
                .sync_get_or_complete(|| DiceSyncResult::testing(v));

            match self.cache.get(k) {
                DiceTaskRef::Computed(_) => panic!("cannot inject already computed task"),
                DiceTaskRef::Occupied(o) => {
                    o.replace_entry(task);
                }
                DiceTaskRef::Vacant(v) => {
                    v.insert(task);
                }
                DiceTaskRef::TransactionCancelled => panic!("transaction cancelled"),
            }
        }

        pub(crate) fn testing_get_epoch(&self) -> VersionEpoch {
            self.version_epoch
        }
    }
}
