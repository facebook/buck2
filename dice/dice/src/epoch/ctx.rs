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
use std::sync::Arc as StdArc;

use dice_error::DiceError;
use dice_error::DiceResult;
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

use crate::ActivationData;
use crate::LinearRecomputeDiceComputations;
use crate::UserCycleDetectorGuard;
use crate::api::computations::DiceComputations;
use crate::api::computations::DiceComputationsData;
use crate::api::data::DiceData;
use crate::api::invalidation_tracking::DiceKeyTrackedInvalidationPaths;
use crate::api::key::Key;
use crate::api::projection::ProjectionKey;
use crate::api::user_data::UserComputationData;
use crate::arc::Arc;
use crate::deps::RecordedDeps;
use crate::deps::RecordingDepsTracker;
use crate::dice::Dice;
use crate::epoch::evaluator::TransactionData;
use crate::epoch::evaluator::VersionEpochState;
use crate::key::CowDiceKeyHashed;
use crate::key::DiceKey;
use crate::key::ParentKey;
use crate::opaque::OpaqueValue;
use crate::updater::ActiveTransactionGuard;
use crate::user_cycle::KeyComputingUserCycleDetectorData;
use crate::value::DiceComputedValue;
use crate::value::TrackedInvalidationPaths;
use crate::versions::VersionNumber;

/// Provides a `DiceTransaction` access to key computations.
pub(crate) struct TransactionCtx {
    ctx: ComputeCtx,
    live_version_guard: ActiveTransactionGuard,
}

impl Clone for TransactionCtx {
    fn clone(&self) -> Self {
        Self {
            ctx: ComputeCtx {
                transaction_data: self.ctx.transaction_data.dupe(),
                parent_key: ParentKey::None,
                cycles: KeyComputingUserCycleDetectorData::Untracked,
                evaluation_data: Mutex::new(EvaluationData::none()),
            },
            live_version_guard: self.live_version_guard.dupe(),
        }
    }
}

impl Dupe for TransactionCtx {}

impl TransactionCtx {
    pub(crate) fn new(
        per_live_version_ctx: VersionEpochState,
        user_data: Arc<UserComputationData>,
        dice: StdArc<Dice>,
        live_version_guard: ActiveTransactionGuard,
    ) -> Self {
        Self {
            ctx: ComputeCtx {
                transaction_data: TransactionData {
                    epoch_state: per_live_version_ctx,
                    user_data,
                    dice,
                },
                parent_key: ParentKey::None,
                cycles: KeyComputingUserCycleDetectorData::Untracked,
                evaluation_data: Mutex::new(EvaluationData::none()),
            },
            live_version_guard,
        }
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.ctx.get_version()
    }

    pub(crate) fn compute<'a, K>(
        &'a self,
        key: &K,
    ) -> impl Future<Output = DiceResult<<K as Key>::Value>> + use<'a, K>
    where
        K: Key,
    {
        self.ctx.compute_opaque(key).map_ok(|opaque| {
            opaque
                .derive_from
                .downcast_maybe_transient::<K::Value>()
                .expect("type mismatch")
                .dupe()
        })
    }

    pub(crate) fn as_computations(&self) -> TrackedComputations<'_> {
        TrackedComputations::Normal {
            compute: &self.ctx,
            // Provide a dep tracker here because this type expects to track its deps, but in the
            // context of a `DiceTransaction` we don't actually need the data
            dep_trackers: RecordingDepsTracker::new(TrackedInvalidationPaths::clean()),
        }
    }
}

// Just for convenience
impl Deref for TransactionCtx {
    type Target = ComputeCtx;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

impl<'d> TrackedComputations<'d> {
    /// Gets all the result of the given computation key.
    /// recorded as dependencies of the current computation for which this
    /// context is for.
    pub(crate) fn compute<'a, K>(
        &'a mut self,
        key: &K,
    ) -> impl Future<Output = DiceResult<<K as Key>::Value>> + use<'a, 'd, K>
    where
        K: Key,
    {
        let (ctx_data, dep_trackers) = self.unpack();
        ctx_data
            .compute_opaque(key)
            .map(move |r| r.map(|opaque| Self::opaque_into_value_impl(dep_trackers, opaque).dupe()))
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque<'a, K>(
        &'a self,
        key: &K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<'d, K>>> + use<'a, 'd, K>
    where
        K: Key,
    {
        self.ctx_data().compute_opaque(key)
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
            LinearRecomputeComputations {
                ctx_data,
                dep_trackers: dep_trackers.dupe(),
            },
        ));

        fut.map(move |v| {
            let mut self_dep_trackers = self_dep_trackers.lock();
            let dep_trackers = Arc::into_inner(dep_trackers)
                .unwrap()
                .into_inner()
                .collect_deps();
            let validity = dep_trackers.deps_validity;
            for k in dep_trackers.deps.iter_keys() {
                self_dep_trackers.record(k, validity, &TrackedInvalidationPaths::clean())
            }
            self_dep_trackers.update_invalidation_paths(&dep_trackers.invalidation_paths);
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
        let inner_core_ctx = ComputeCtx {
            transaction_data: ctx_data.transaction_data.dupe(),
            parent_key: ctx_data.parent_key.dupe(),
            // FIXME(JakobDegen): These are never looked at again below, seems bad?
            cycles: ctx_data.cycles.clone(),
            evaluation_data: Mutex::new(EvaluationData::none()),
        };

        let user_data = ctx_data.per_transaction_data();
        let spawner = user_data.spawner.dupe();
        let ctx_data = user_data.dupe();

        let task = spawn_dropcancel(
            |cancellation| {
                async move {
                    let mut ctx = TrackedComputations::Normal {
                        compute: &inner_core_ctx,
                        dep_trackers: RecordingDepsTracker::new(TrackedInvalidationPaths::clean()),
                    }
                    .into();
                    let res = closure(&mut ctx, cancellation).await;
                    let deps = ctx.0.finalize();
                    (res, deps)
                }
                .boxed()
            },
            &*spawner,
            ctx_data,
        );

        task.map(move |(res, deps)| {
            let validity = deps.deps_validity;
            let mut self_dep_trackers = self_dep_trackers.lock();
            for k in deps.deps.iter_keys() {
                self_dep_trackers.record(k, validity, &TrackedInvalidationPaths::clean())
            }
            self_dep_trackers.update_invalidation_paths(&deps.invalidation_paths);

            res
        })
    }

    pub(crate) fn opaque_into_value<K: Key>(&mut self, opaque: OpaqueValue<'d, K>) -> &'d K::Value {
        Self::opaque_into_value_impl(self.unpack().1, opaque)
    }

    fn opaque_into_value_impl<K: Key>(
        deps: DepsTrackerHolder,
        opaque: OpaqueValue<'d, K>,
    ) -> &'d K::Value {
        let OpaqueValue {
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
    }

    pub(crate) fn get_invalidation_paths(&mut self) -> DiceKeyTrackedInvalidationPaths {
        let (normal, high) = {
            let mut dep_trackers = self.dep_trackers();
            let paths = dep_trackers.invalidation_paths();
            (paths.get_normal(), paths.get_high())
        };
        DiceKeyTrackedInvalidationPaths::new(
            self.ctx_data().transaction_data.dice.dupe(),
            normal,
            high,
        )
    }

    pub(crate) fn data(&self) -> DiceComputationsData {
        DiceComputationsData(ModernDiceComputationsData(
            self.ctx_data().transaction_data.dupe(),
        ))
    }
}

impl<'a> From<TrackedComputations<'a>> for DiceComputations<'a> {
    fn from(value: TrackedComputations<'a>) -> Self {
        DiceComputations(value)
    }
}

/// Like `TrackedComputations`, but for linear recompute.
pub(crate) struct LinearRecomputeComputations<'a> {
    ctx_data: &'a ComputeCtx,
    dep_trackers: Arc<Mutex<RecordingDepsTracker>>,
}

impl LinearRecomputeComputations<'_> {
    pub(crate) fn get(&self) -> DiceComputations<'_> {
        DiceComputations(TrackedComputations::Linear {
            compute: self.ctx_data,
            dep_trackers: &self.dep_trackers,
        })
    }
}

/// This is used to create the ctx for each individual parallel compute (from compute_many/compute_join/compute2/etc).
///
/// For the Normal case, each parallel ctx will be expected to record its deps into a RecordedDeps allocated in the arena.
///
/// For the Linear case, each parallel ctx will record deps into the shared RecordingDepsTracker.
pub(crate) enum ModernComputeCtxParallelBuilder<'a> {
    Normal {
        ctx_data: &'a ComputeCtx,
        tracker_arena: &'a Arena<RecordedDeps>,
        invalidation_paths: &'a TrackedInvalidationPaths,
    },
    Linear {
        ctx_data: &'a ComputeCtx,
        dep_trackers: &'a Mutex<RecordingDepsTracker>,
    },
}

impl<'a> ModernComputeCtxParallelBuilder<'a> {
    fn compute<F, T>(&self, func: F) -> impl Future<Output = T> + use<'a, T, F>
    where
        // We don't actually need this closure to be `Send` and so we don't require that here, but
        // all the public APIs still do. It's unclear what we should commit to.
        F: for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T>,
    {
        match self {
            ModernComputeCtxParallelBuilder::Normal {
                ctx_data,
                tracker_arena,
                invalidation_paths,
            } => OwningFuture::new(
                (
                    tracker_arena.alloc(RecordedDeps::new()),
                    TrackedComputations::Normal {
                        compute: ctx_data,
                        dep_trackers: RecordingDepsTracker::new((*invalidation_paths).dupe()),
                    }
                    .into(),
                ),
                // Note: We need to use `OwningFuture` here specifically because we want `func` to
                // be called immediately instead of on the first poll. It's unclear whether that's
                // needed for the API (someone should decide and commit either way) but it's
                // important as a memory optimization, since it prevents the future returned from
                // this function needing room to store `func`
                |(_, ctx)| func(ctx),
            )
            .map_taking_data(|v, (this_deps, ctx)| {
                *this_deps = ctx.0.finalize();
                v
            })
            .left_future(),
            ModernComputeCtxParallelBuilder::Linear {
                ctx_data,
                dep_trackers,
            } => OwningFuture::new(
                TrackedComputations::Linear {
                    compute: ctx_data,
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
pub struct ModernDiceComputationsData(TransactionData);

impl ModernDiceComputationsData {
    pub fn global_data(&self) -> &DiceData {
        &self.0.dice.global_data
    }

    pub fn per_transaction_data(&self) -> &UserComputationData {
        &self.0.user_data
    }
}

/// A handle to a `ComputeCtx`, augmented with all the machinery to track the keys that are actually
/// computed as dependencies.
pub(crate) enum TrackedComputations<'a> {
    Normal {
        compute: &'a ComputeCtx,
        dep_trackers: RecordingDepsTracker,
    },
    /// The ctx within a with_linear_recompute.
    Linear {
        compute: &'a ComputeCtx,
        dep_trackers: &'a Mutex<RecordingDepsTracker>,
    },
}

impl TrackedComputations<'_> {
    pub(crate) fn finalize(self) -> RecordedDeps {
        match self {
            TrackedComputations::Normal {
                compute: _,
                dep_trackers,
            } => dep_trackers.collect_deps(),
            _ => unreachable!(),
        }
    }
}

struct DepsTrackerHolder<'a>(Either<&'a mut RecordingDepsTracker, &'a Mutex<RecordingDepsTracker>>);

impl<'a> DepsTrackerHolder<'a> {
    fn lock(self) -> impl DerefMut<Target = RecordingDepsTracker> {
        self.0.map_right(|v| v.lock())
    }
}

impl<'d> TrackedComputations<'d> {
    fn parallel_builder(&mut self, size_hint: usize) -> ModernComputeCtxParallelBuilder<'_> {
        match self {
            TrackedComputations::Normal {
                compute: ctx_data,
                dep_trackers,
            } => {
                let (tracker_arena, invalidation_paths) = dep_trackers.push_parallel(size_hint);
                ModernComputeCtxParallelBuilder::Normal {
                    ctx_data,
                    tracker_arena,
                    invalidation_paths,
                }
            }
            TrackedComputations::Linear {
                compute: ctx_data,
                dep_trackers,
            } => ModernComputeCtxParallelBuilder::Linear {
                ctx_data,
                dep_trackers,
            },
        }
    }

    fn ctx_data(&self) -> &'d ComputeCtx {
        match self {
            TrackedComputations::Normal {
                compute: ctx_data, ..
            } => ctx_data,
            TrackedComputations::Linear {
                compute: ctx_data, ..
            } => ctx_data,
        }
    }

    fn unpack(&mut self) -> (&'d ComputeCtx, DepsTrackerHolder<'_>) {
        match self {
            TrackedComputations::Normal {
                compute: ctx_data,
                dep_trackers,
            } => (
                ctx_data,
                DepsTrackerHolder(Either::Left(&mut *dep_trackers)),
            ),
            TrackedComputations::Linear {
                compute: ctx_data,
                dep_trackers,
            } => (ctx_data, DepsTrackerHolder(Either::Right(dep_trackers))),
        }
    }

    /// Compute "projection" based on deriving value
    pub(crate) fn projection<K: Key, P: ProjectionKey<DeriveFromKey = K>>(
        &mut self,
        derive_from: &OpaqueValue<K>,
        key: &P,
    ) -> DiceResult<P::Value> {
        let (ctx_data, dep_trackers) = self.unpack();
        let (dice_key, res) = ctx_data.project(key, derive_from)?;
        dep_trackers
            .lock()
            .record(dice_key, res.value().validity(), res.invalidation_paths());

        Ok(res
            .value()
            .downcast_maybe_transient::<P::Value>()
            .expect("Type mismatch when computing key")
            .dupe())
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

    #[allow(unused)] // used in test
    pub(crate) fn dep_trackers(&mut self) -> impl DerefMut<Target = RecordingDepsTracker> {
        self.unpack().1.lock()
    }

    pub(crate) fn store_evaluation_data<T: Send + Sync + 'static>(
        &self,
        value: T,
    ) -> DiceResult<()> {
        self.ctx_data().store_evaluation_data(value)
    }

    pub(crate) fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<StdArc<T>>> {
        self.ctx_data().cycle_guard()
    }
}

/// The base type that provides access to `.compute()` operations and stores the state needed to
/// perform those operations.
///
/// This type does not *track* those dependencies.
pub(crate) struct ComputeCtx {
    pub(crate) transaction_data: TransactionData,
    pub(crate) parent_key: ParentKey,
    pub(crate) cycles: KeyComputingUserCycleDetectorData,
    // data for the entire compute of a Key, including parallel computes
    pub(crate) evaluation_data: Mutex<EvaluationData>,
}

impl ComputeCtx {
    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub(crate) fn compute_opaque<'d, K>(
        &'d self,
        key: &K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<'d, K>>> + use<'d, K>
    where
        K: Key,
    {
        let dice_key = self
            .transaction_data
            .dice
            .key_index
            .index(CowDiceKeyHashed::key_ref(key));

        self.transaction_data
            .epoch_state
            .compute_opaque(
                dice_key,
                self.parent_key,
                &self.transaction_data,
                self.cycles
                    .subrequest(dice_key, &self.transaction_data.dice.key_index),
            )
            .map_ok(move |dice_value| {
                OpaqueValue::new(
                    dice_key,
                    dice_value.value(),
                    dice_value.invalidation_paths(),
                )
            })
            .map_err(DiceError::cancelled)
    }

    /// Compute "projection" based on deriving value
    pub(super) fn project<B: Key, K: ProjectionKey<DeriveFromKey = B>>(
        &self,
        key: &K,
        base: &OpaqueValue<B>,
    ) -> DiceResult<(DiceKey, DiceComputedValue)> {
        let dice_key = self
            .transaction_data
            .dice
            .key_index
            .index(CowDiceKeyHashed::proj_ref(base.derive_from_key, key));

        self.transaction_data
            .epoch_state
            .compute_projection(
                dice_key,
                base.derive_from,
                base.invalidation_paths,
                &self.transaction_data,
            )
            .map(|r| (dice_key, r))
            .map_err(DiceError::cancelled)
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub(crate) fn global_data(&self) -> &DiceData {
        &self.transaction_data.dice.global_data
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub(crate) fn per_transaction_data(&self) -> &UserComputationData {
        &self.transaction_data.user_data
    }

    pub(crate) fn get_version(&self) -> VersionNumber {
        self.transaction_data.epoch_state.get_version()
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

    pub(crate) fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<StdArc<T>>> {
        self.cycles.cycle_guard()
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
