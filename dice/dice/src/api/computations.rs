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
use async_trait::async_trait;
use buck2_futures::cancellation::CancellationContext;
use dice_error::DiceResult;
use futures::future::BoxFuture;

use crate::api::data::DiceData;
use crate::api::key::Key;
use crate::api::opaque::OpaqueValue;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationsImpl;
use crate::ctx::LinearRecomputeDiceComputationsImpl;
use crate::DiceKeyTrackedInvalidationPaths;
use crate::ProjectionKey;
use crate::UserCycleDetectorGuard;

/// The context for computations to register themselves, and request for additional dependencies.
/// The dependencies accessed are tracked for caching via the `DiceCtx`.
///
/// The computations are registered onto `DiceComputations` via implementing traits for the
/// `DiceComputation`.
///
/// The context is valid only for the duration of the computation of a single key, and cannot be
/// owned.
#[derive(Allocative)]
pub struct DiceComputations<'a>(pub(crate) DiceComputationsImpl<'a>);

fn _test_computations_sync_send() {
    fn _assert_sync_send<T: Sync + Send>() {}
    _assert_sync_send::<DiceComputations>();
}

impl<'d> DiceComputations<'d> {
    /// Gets the result of the given computation key.
    /// Record dependencies of the current computation for which this
    /// context is for.
    pub fn compute<'a, K>(
        &'a mut self,
        key: &K,
    ) -> impl Future<Output = DiceResult<<K as Key>::Value>> + 'a
    where
        K: Key,
    {
        self.0.compute(key)
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub fn compute_opaque<'a, K>(
        &'a self,
        key: &K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<K>>> + 'a
    where
        K: Key,
    {
        self.0.compute_opaque(key)
    }

    pub fn projection<'a, K: Key, P: ProjectionKey<DeriveFromKey = K>>(
        &'a mut self,
        derive_from: &OpaqueValue<K>,
        projection_key: &P,
    ) -> DiceResult<P::Value> {
        self.0.projection(derive_from, projection_key)
    }

    pub fn opaque_into_value<'a, K: Key>(
        &'a mut self,
        derive_from: OpaqueValue<K>,
    ) -> DiceResult<K::Value> {
        self.0.opaque_into_value(derive_from)
    }

    /// DiceComputations &mut-based api can make some computations much more complex to express, but without it
    /// the data dependencies between different compute requests are impossible to track. with_linear_recompute()
    /// is an escape hatch in the case where we are willing to sacrifice recompute performance for easier expression
    /// of a computation. It should not be used lightly, it can be difficult to attribute performance regressions to its use.
    ///
    /// Withing the with_linear_recompute(), all deps will be recorded as accessed sequentially and so recomputation
    /// will not trigger them in parallel. This will apply to calls on DiceComputations returned by .ctx() and by the ones received
    /// in closures from compute_many and friends. It will not apply to the other deps from which the linear recompute was created.
    ///
    /// For example:
    ///
    /// ```ignore
    /// let mut ctx = something();
    /// let keys1 = vec![Key(10), Key(11)];
    /// let keys2 = vec![Key(20), Key(21)];
    /// let keys3 = vec![Key(30), Key(31)];
    /// let keys4 = vec![Key(40), Key(41)]
    /// ctx.compute_join(keys1, |ctx, key1| ctx.compute(key1).boxed()).await;
    /// ctx.with_linear_recompute(|mut linear| {
    ///     linear.ctx().compute_join(keys2, |ctx, key2| async move {
    ///         ctx.compute2(
    ///             |ctx| ctx.compute(key2).boxed(),
    ///             |ctx| ctx.compute_join(keys3, |ctx, key3| ctx.compute(key3).boxed()).boxed()
    ///         ).await;
    ///     }.boxed()
    /// ).await
    /// ctx.compute_join(keys4, |ctx, key4| ctx.compute(key4).boxed()).await;
    /// });
    /// ```
    ///
    /// In this example, the recomputation of all of keys2 and keys3 would be done linearly, but keys1 and keys4 would be recomputed in parallel.
    pub fn with_linear_recompute<'a, T, Fut: Future<Output = T> + 'a>(
        &'a mut self,
        func: impl FnOnce(LinearRecomputeDiceComputations<'a>) -> Fut + 'a,
    ) -> impl Future<Output = T> + 'a {
        self.0.with_linear_recompute(func)
    }

    /// Creates computation Futures for all the given tasks.
    ///
    /// ```ignore
    /// let mut ctx: &'a DiceComputations = ctx();
    /// let data: String = data();
    /// let keys : Vec<Key> = keys();
    /// let futs = ctx.compute_many(keys.into_iter().map(|k|
    ///   DiceComputations::declare_closure(
    ///     |dice: &mut DiceComputations| -> BoxFuture<String> {
    ///       async move {
    ///         dice.compute(k).await + data
    ///       }.boxed()
    ///     }
    ///   )
    /// ));
    /// futures::future::join_all(futs).await;
    /// ```
    pub fn compute_many<'a, T: 'a>(
        &'a mut self,
        computes: impl IntoIterator<
            Item = impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
        >,
    ) -> Vec<impl Future<Output = T> + 'a> {
        self.0.compute_many(computes)
    }

    /// Maps the items into computation futures and joins on them.
    ///
    /// ```ignore
    /// let mut ctx: &'a DiceComputations = ctx();
    /// let data: String = data();
    /// let keys : Vec<Key> = keys();
    /// // When defined inplance, there's no need to use a declare helper.
    /// ctx.compute_join(keys, |dice: &mut DiceComputations, k: &Key| {
    ///     async move {
    ///       dice.compute(k).await + data
    ///     }
    ///   }).await;
    ///
    /// // If the closure is going to be declared outside the compute_many itself, you need to use
    /// // declare_join_closure for it to get the right lifetime bounds.
    /// let compute_one = DiceComputations::declare_join_closure(
    ///   |dice: &mut DiceComputations, k: &Key| {
    ///     async move {
    ///       dice.compute(k).await + data
    ///     }
    ///   }
    /// );
    /// ctx.compute_join(keys, compute_one).await;
    /// ````
    pub fn compute_join<'a, T: Send, R: 'a>(
        &'a mut self,
        items: impl IntoIterator<Item = T>,
        mapper: (
            impl for<'x> FnOnce(&'x mut DiceComputations<'a>, T) -> BoxFuture<'x, R> + Send + Sync + Copy
        ),
    ) -> impl Future<Output = Vec<R>> + 'a {
        let futs = self.compute_many(items.into_iter().map(move |v| {
            DiceComputations::declare_closure(move |ctx: &mut DiceComputations| -> BoxFuture<R> {
                mapper(ctx, v)
            })
        }));
        futures::future::join_all(futs)
    }

    /// Maps the items into computations futures and then returns a future which represents either a
    /// collection of the results or an error.
    pub fn try_compute_join<'a, T: Send, R: 'a, E: 'a>(
        &'a mut self,
        items: impl IntoIterator<Item = T>,
        mapper: (
            impl for<'x> FnOnce(&'x mut DiceComputations<'a>, T) -> BoxFuture<'x, Result<R, E>>
            + Send
            + Sync
            + Copy
        ),
    ) -> impl Future<Output = Result<Vec<R>, E>> + 'a {
        let futs = self.compute_many(items.into_iter().map(move |v| {
            DiceComputations::declare_closure(
                move |ctx: &mut DiceComputations| -> BoxFuture<Result<R, E>> { mapper(ctx, v) },
            )
        }));
        crate::future::try_join_all(futs)
    }

    /// Computes all the given tasks in parallel.
    ///
    /// If the closures are defined out of the compute2 call, you need to use declare_closure() to get the right lifetimes.
    pub fn compute2<'a, T: 'a, U: 'a>(
        &'a mut self,
        compute1: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
        compute2: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, U> + Send,
    ) -> impl Future<Output = (T, U)> + 'a {
        let (t, u) = self.0.compute2(compute1, compute2);
        futures::future::join(t, u)
    }

    /// Compute all the given tasks in parallel.
    pub fn try_compute2<'a, T: 'a, U: 'a, E: 'a>(
        &'a mut self,
        compute1: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, Result<T, E>>
        + Send,
        compute2: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, Result<U, E>>
        + Send,
    ) -> impl Future<Output = Result<(T, U), E>> + 'a {
        let (t, u) = self.0.compute2(compute1, compute2);
        futures::future::try_join(t, u)
    }

    /// Computes all the given tasks in parallel.
    ///
    /// If the closures are defined out of the compute3 call, you need to use declare_closure() to get the right lifetimes.
    pub fn compute3<'a, T: 'a, U: 'a, V: 'a>(
        &'a mut self,
        compute1: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
        compute2: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, U> + Send,
        compute3: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, V> + Send,
    ) -> impl Future<Output = (T, U, V)> + 'a {
        let (t, u, v) = self.0.compute3(compute1, compute2, compute3);
        futures::future::join3(t, u, v)
    }

    /// Compute all the given tasks in parallel.
    pub fn try_compute3<'a, T: 'a, U: 'a, V: 'a, E: 'a>(
        &'a mut self,
        compute1: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, Result<T, E>>
        + Send,
        compute2: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, Result<U, E>>
        + Send,
        compute3: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, Result<V, E>>
        + Send,
    ) -> impl Future<Output = Result<(T, U, V), E>> + 'a {
        let (t, u, v) = self.0.compute3(compute1, compute2, compute3);
        futures::future::try_join3(t, u, v)
    }

    /// Used to declare a higher order closure for compute_join and try_compute_join.
    ///
    /// We need to use BoxFuture here to express that the future captures the 'x lifetime.
    pub fn declare_join_closure<'a, T, R, Closure>(closure: Closure) -> Closure
    where
        Closure: for<'x> FnOnce(&'x mut DiceComputations<'a>, T) -> BoxFuture<'x, R>
            + Send
            + Sync
            + Copy,
    {
        closure
    }

    /// Used to declare a higher order closure for compute2 and compute_many.
    ///
    /// We need to use BoxFuture here to express that the future captures the 'x lifetime.
    pub fn declare_closure<'a, R, Closure>(closure: Closure) -> Closure
    where
        Closure: for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, R>,
    {
        closure
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub fn global_data(&self) -> &DiceData {
        self.0.global_data()
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub fn per_transaction_data(&self) -> &UserComputationData {
        self.0.per_transaction_data()
    }

    /// Gets the current cycle guard if its set. If it's set but a different type, an error will be returned.
    pub fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<Arc<T>>> {
        self.0.cycle_guard()
    }

    /// Store some extra data that the ActivationTracker will receive if / when this key finishes
    /// executing.
    pub fn store_evaluation_data<T: Send + Sync + 'static>(&self, value: T) -> DiceResult<()> {
        self.0.store_evaluation_data(value)
    }

    /// Returns the current tracked invalidation paths for this computation node.
    pub fn get_invalidation_paths(&mut self) -> DiceKeyTrackedInvalidationPaths {
        self.0.get_invalidation_paths()
    }
}

pub struct LinearRecomputeDiceComputations<'a>(pub(crate) LinearRecomputeDiceComputationsImpl<'a>);

impl LinearRecomputeDiceComputations<'_> {
    pub fn get(&self) -> DiceComputations<'_> {
        self.0.get()
    }
}

// This assertion assures we don't unknowingly regress the size of this critical future.
// TODO(cjhopman): We should be able to wrap this in a convenient assertion macro.
#[allow(unused, clippy::diverging_sub_expression)]
fn _assert_dice_compute_future_sizes() {
    let ctx: DiceComputations = panic!();
    #[derive(Allocative, Debug, Clone, PartialEq, Eq, Hash)]
    struct K(u64);
    impl std::fmt::Display for K {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            panic!()
        }
    }
    #[async_trait]
    impl Key for K {
        type Value = Arc<String>;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            cancellations: &CancellationContext,
        ) -> Self::Value {
            panic!()
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            panic!()
        }
    }
    let k: K = panic!();
    let v = ctx.compute(&k);
    let e = [0u8; 704 / 8];
    static_assertions::assert_eq_size_ptr!(&v, &e);
}
