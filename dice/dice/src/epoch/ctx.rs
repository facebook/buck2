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
use std::sync::Arc as StdArc;

use dice_error::DiceError;
use dice_error::DiceResult;
use dice_futures::cancellation::CancellationContext;
use dice_futures::spawn::spawn_dropcancel;
use dupe::Dupe;
use futures::FutureExt;
use futures::TryFutureExt;
use futures::future::BoxFuture;
use itertools::Either;
use parking_lot::Mutex;

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
use crate::deps::LinearDepsTracker;
use crate::deps::RecordedDeps;
use crate::deps::RecordingDepsTracker;
use crate::dice::Dice;
use crate::epoch::branches::BranchEntry;
use crate::epoch::branches::ParallelGroup;
use crate::epoch::branches::ParallelBranchFuture;
use crate::epoch::evaluator::TransactionData;
use crate::epoch::linear_branches::LinearBranches;
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
    ) -> impl Future<Output = DiceResult<&'a <K as Key>::Value>> + use<'a, K>
    where
        K: Key,
    {
        self.ctx.compute_opaque(key).map_ok(|opaque| {
            opaque
                .derive_from
                .downcast_maybe_transient::<K::Value>()
                .expect("type mismatch")
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
    ) -> impl Future<Output = DiceResult<&'d <K as Key>::Value>> + use<'a, 'd, K>
    where
        K: Key,
    {
        self.ctx_data().compute_opaque(key).map(move |r| {
            r.map(|opaque| Self::opaque_into_value_impl(self.dep_trackers_holder(), opaque))
        })
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

    /// Starts one parallel branch per element, invoking each closure eagerly on a fresh branch
    /// ctx. This is the primitive that all the parallel compute APIs bottom out in; the future
    /// type is generic so that both the boxed (`BoxFuture`) and unboxed (async closure) public
    /// APIs go through here.
    pub(crate) fn compute_many_inner<'a, Starts, G, Fut>(
        &'a mut self,
        starts: Starts,
    ) -> Vec<ParallelBranchFuture<'a, Fut>>
    where
        Starts: IntoIterator<Item = G>,
        Starts::IntoIter: ExactSizeIterator,
        G: FnOnce(&'a mut DiceComputations<'d>) -> Fut,
        Fut: Future,
    {
        let iter = starts.into_iter();
        let mut parallel = self.parallel_builder(iter.len());
        iter.map(|func| parallel.compute(func)).collect()
    }

    /// Computes all the given tasks in parallel, returning an unordered Stream
    pub(crate) fn compute_many<'a, Computes, F, T>(
        &'a mut self,
        computes: Computes,
    ) -> Vec<impl Future<Output = T> + use<'a, 'd, Computes, F, T>>
    where
        Computes: IntoIterator<Item = F>,
        Computes::IntoIter: ExactSizeIterator,
        F: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, T> + Send,
    {
        self.compute_many_inner(computes)
    }

    pub(crate) fn compute2<'a, G1, Fut1, G2, Fut2>(
        &'a mut self,
        compute1: G1,
        compute2: G2,
    ) -> (
        ParallelBranchFuture<'a, Fut1>,
        ParallelBranchFuture<'a, Fut2>,
    )
    where
        G1: FnOnce(&'a mut DiceComputations<'d>) -> Fut1,
        G2: FnOnce(&'a mut DiceComputations<'d>) -> Fut2,
        Fut1: Future,
        Fut2: Future,
    {
        let mut parallel = self.parallel_builder(2);
        (parallel.compute(compute1), parallel.compute(compute2))
    }

    pub(crate) fn compute3<'a, G1, Fut1, G2, Fut2, G3, Fut3>(
        &'a mut self,
        compute1: G1,
        compute2: G2,
        compute3: G3,
    ) -> (
        ParallelBranchFuture<'a, Fut1>,
        ParallelBranchFuture<'a, Fut2>,
        ParallelBranchFuture<'a, Fut3>,
    )
    where
        G1: FnOnce(&'a mut DiceComputations<'d>) -> Fut1,
        G2: FnOnce(&'a mut DiceComputations<'d>) -> Fut2,
        G3: FnOnce(&'a mut DiceComputations<'d>) -> Fut3,
        Fut1: Future,
        Fut2: Future,
        Fut3: Future,
    {
        let mut parallel = self.parallel_builder(3);

        (
            parallel.compute(compute1),
            parallel.compute(compute2),
            parallel.compute(compute3),
        )
    }

    pub(crate) fn with_linear_recompute<'a, Func, Fut, T>(
        &'a mut self,
        func: Func,
    ) -> impl Future<Output = T> + use<'a, 'd, Func, Fut, T>
    where
        Func: FnOnce(LinearRecomputeDiceComputations<'a>) -> Fut,
        Fut: Future<Output = T>,
    {
        let (ctx_data, self_dep_trackers) = self.unpack();
        let shared = Arc::new(LinearShared {
            dep_trackers: Mutex::new(LinearDepsTracker::new(
                // TODO(cjhopman): if inspected during the with_linear_recompute, this will be missing some invalidation paths.
                TrackedInvalidationPaths::clean(),
            )),
            branch_ctxs: LinearBranches::new(),
        });
        let fut = func(LinearRecomputeDiceComputations(
            LinearRecomputeComputations {
                ctx_data,
                shared: shared.dupe(),
            },
        ));

        fut.map(move |v| {
            let mut self_dep_trackers = self_dep_trackers;
            // The unwrap is fine because the `LinearRecomputeDiceComputations` holding the other
            // reference is dead by the time the inner future completes.
            let shared = Arc::into_inner(shared).unwrap();
            let dep_trackers = shared.dep_trackers.into_inner().collect_deps();
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
    ) -> impl Future<Output = T> + use<'a, 'd, Compute, T>
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
            let mut self_dep_trackers = self_dep_trackers;
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
        mut deps: DepsTrackerHolder<'_, 'd>,
        opaque: OpaqueValue<'d, K>,
    ) -> &'d K::Value {
        let OpaqueValue {
            derive_from_key,
            derive_from,
            invalidation_paths,
            ..
        } = opaque;

        deps.record(derive_from_key, derive_from.validity(), invalidation_paths);

        derive_from
            .downcast_maybe_transient::<K::Value>()
            .expect("type mismatch")
    }

    pub(crate) fn get_invalidation_paths(&mut self) -> DiceKeyTrackedInvalidationPaths {
        let (normal, high) = {
            let paths = self.dep_trackers_holder().invalidation_paths();
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
    shared: Arc<LinearShared>,
}

/// The state shared by all the ctxs of one `with_linear_recompute`.
pub(crate) struct LinearShared {
    dep_trackers: Mutex<LinearDepsTracker>,
    /// Owns the ctxs of parallel computes done through linear ctxs; see [`LinearBranches`].
    branch_ctxs: LinearBranches,
}

impl LinearRecomputeComputations<'_> {
    pub(crate) fn get(&self) -> DiceComputations<'_> {
        DiceComputations(TrackedComputations::Linear {
            compute: self.ctx_data,
            shared: &self.shared,
        })
    }
}

/// This is used to create the ctx for each individual parallel compute (from compute_many/compute_join/compute2/etc).
///
/// For the Normal case, each parallel ctx records its deps into its own tracker, which the group
/// owns and which the parent gathers up when the parallel compute is finished.
///
/// For the Linear case, each parallel ctx will record deps into the shared RecordingDepsTracker.
///
/// The two lifetimes here are deliberately separate: `'d` is the lifetime parameter of the
/// `DiceComputations<'d>` that the parallel closures receive (and hence the lifetime of values
/// computed through them), while `'a` is the borrow of the parent ctx. Only the returned futures
/// are tied to `'a`; the ctxs handed to the closures are not, which is what allows values they
/// compute to be held past the end of the parallel compute.
pub(crate) enum ModernComputeCtxParallelBuilder<'a, 'd> {
    Normal {
        /// The branches, pre-built by `parallel_builder`; `compute` claims them in order.
        handout: std::slice::IterMut<'a, BranchEntry<'d>>,
    },
    Linear {
        ctx_data: &'d ComputeCtx,
        shared: &'d LinearShared,
    },
}

impl<'a, 'd: 'a> ModernComputeCtxParallelBuilder<'a, 'd> {
    fn compute<F, Fut>(&mut self, func: F) -> ParallelBranchFuture<'a, Fut>
    where
        // We don't actually need this closure to be `Send` and so we don't require that here, but
        // all the public APIs still do. It's unclear what we should commit to.
        F: FnOnce(&'a mut DiceComputations<'d>) -> Fut,
        Fut: Future,
    {
        match self {
            ModernComputeCtxParallelBuilder::Normal { handout } => ParallelBranchFuture::launch(
                handout
                    .next()
                    .expect("more branches than the ExactSizeIterator promised")
                    .parts(),
                func,
            ),
            ModernComputeCtxParallelBuilder::Linear { ctx_data, shared } => {
                ParallelBranchFuture::launch(
                    shared.branch_ctxs.alloc(
                        TrackedComputations::Linear {
                            compute: ctx_data,
                            shared,
                        }
                        .into(),
                    ),
                    func,
                )
            }
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
        dep_trackers: RecordingDepsTracker<'a>,
    },
    /// The ctx within a with_linear_recompute.
    Linear {
        compute: &'a ComputeCtx,
        shared: &'a LinearShared,
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

/// Write access to whichever dep tracker a ctx records into: A normal ctx's own tracker, or the
/// mutex-shared one of a linear recompute.
struct DepsTrackerHolder<'a, 'd>(
    Either<&'a mut RecordingDepsTracker<'d>, &'a Mutex<LinearDepsTracker>>,
);

impl<'a, 'd> DepsTrackerHolder<'a, 'd> {
    fn record(
        &mut self,
        k: DiceKey,
        validity: crate::value::DiceValidity,
        invalidation_paths: &TrackedInvalidationPaths,
    ) {
        match &mut self.0 {
            Either::Left(t) => t.record(k, validity, invalidation_paths),
            Either::Right(m) => m.lock().record(k, validity, invalidation_paths),
        }
    }

    fn update_invalidation_paths(&mut self, invalidation_paths: &TrackedInvalidationPaths) {
        match &mut self.0 {
            Either::Left(t) => t.update_invalidation_paths(invalidation_paths),
            Either::Right(m) => m.lock().update_invalidation_paths(invalidation_paths),
        }
    }

    fn invalidation_paths(&mut self) -> TrackedInvalidationPaths {
        match &mut self.0 {
            Either::Left(t) => t.invalidation_paths().dupe(),
            Either::Right(m) => m.lock().invalidation_paths().dupe(),
        }
    }

    #[cfg(test)]
    pub(crate) fn recorded_deps(&mut self) -> crate::HashSet<DiceKey> {
        use crate::deps::testing::RecordingDepsTrackersExt;
        match &mut self.0 {
            Either::Left(t) => t.recorded_deps(),
            Either::Right(m) => m.lock().recorded_deps(),
        }
    }
}

impl<'d> TrackedComputations<'d> {
    fn parallel_builder(&mut self, len: usize) -> ModernComputeCtxParallelBuilder<'_, 'd> {
        match self {
            TrackedComputations::Normal {
                compute: ctx_data,
                dep_trackers,
            } => {
                let ctx_data = *ctx_data;
                let invalidation_paths = dep_trackers.invalidation_paths().dupe();
                let group = ParallelGroup::new((0..len).map(|_| {
                    TrackedComputations::Normal {
                        compute: ctx_data,
                        dep_trackers: RecordingDepsTracker::new(invalidation_paths.dupe()),
                    }
                    .into()
                }));
                ModernComputeCtxParallelBuilder::Normal {
                    handout: dep_trackers.push_parallel(group).handout(),
                }
            }
            TrackedComputations::Linear {
                compute: ctx_data,
                shared,
            } => ModernComputeCtxParallelBuilder::Linear { ctx_data, shared },
        }
    }

    pub(super) fn ctx_data(&self) -> &'d ComputeCtx {
        match self {
            TrackedComputations::Normal {
                compute: ctx_data, ..
            } => ctx_data,
            TrackedComputations::Linear {
                compute: ctx_data, ..
            } => ctx_data,
        }
    }

    fn dep_trackers_holder(&mut self) -> DepsTrackerHolder<'_, 'd> {
        self.unpack().1
    }

    fn unpack(&mut self) -> (&'d ComputeCtx, DepsTrackerHolder<'_, 'd>) {
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
                shared,
            } => (
                ctx_data,
                DepsTrackerHolder(Either::Right(&shared.dep_trackers)),
            ),
        }
    }

    /// Compute "projection" based on deriving value
    pub(crate) fn projection<K: Key, P: ProjectionKey<DeriveFromKey = K>>(
        &mut self,
        derive_from: &OpaqueValue<K>,
        key: &P,
    ) -> DiceResult<P::Value> {
        let (ctx_data, mut dep_trackers) = self.unpack();
        let (dice_key, res) = ctx_data.project(key, derive_from)?;
        dep_trackers.record(dice_key, res.value().validity(), res.invalidation_paths());

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
    #[cfg(test)]
    pub(crate) fn recorded_deps_for_test(&mut self) -> crate::HashSet<DiceKey> {
        self.unpack().1.recorded_deps()
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
            .map(move |result| {
                result.as_ref().into_dice_result().map(|dice_value| {
                    OpaqueValue::new(
                        dice_key,
                        dice_value.value(),
                        dice_value.invalidation_paths(),
                    )
                })
            })
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
            .into_dice_result()
            .map(|r| (dice_key, r))
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
