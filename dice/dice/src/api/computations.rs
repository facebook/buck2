/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use dice_error::DiceResult;
use dice_futures::cancellation::CancellationContext;
use dice_futures::owning_future::OwningFuture;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::BoxFuture;
use pagable::Pagable;
use pagable::pagable_typetag;

use crate::DiceKeyTrackedInvalidationPaths;
use crate::OpaqueValue;
use crate::ProjectionKey;
use crate::UserCycleDetectorGuard;
use crate::api::data::DiceData;
use crate::api::key::Key;
use crate::api::key::NoValueSerialize;
use crate::api::key::ValueSerialize;
use crate::api::user_data::UserComputationData;
use crate::epoch::ctx::LinearRecomputeComputations;
use crate::epoch::ctx::ModernDiceComputationsData;
use crate::epoch::ctx::TrackedComputations;
use crate::key::DiceKeyDyn;

/// The context for computations to register themselves, and request for additional dependencies.
/// The dependencies accessed are tracked for caching via the `DiceCtx`.
///
/// The computations are registered onto `DiceComputations` via implementing traits for the
/// `DiceComputation`.
///
/// The context is valid only for the duration of the computation of a single key, and cannot be
/// owned.
pub struct DiceComputations<'a>(pub(crate) TrackedComputations<'a>);

fn _test_computations_sync_send() {
    fn _assert_sync_send<T: Sync + Send>() {}
    _assert_sync_send::<DiceComputations>();
}

impl<'d> DiceComputations<'d> {
    /// Gets the result of the given computation key.
    pub fn compute<'a, K>(
        &'a mut self,
        key: &K,
    ) -> impl Future<Output = DiceResult<<K as Key>::Value>> + use<'a, 'd, K>
    where
        K: Key,
    {
        self.0.compute(key).map(|r| r.map(Dupe::dupe))
    }

    /// Gets the result of the given computation key.
    pub fn compute_ref<'a, K>(
        &'a mut self,
        key: &K,
    ) -> impl Future<Output = DiceResult<&'d <K as Key>::Value>> + use<'a, 'd, K>
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
    ) -> impl Future<Output = DiceResult<OpaqueValue<'d, K>>> + use<'a, 'd, K>
    where
        K: Key,
    {
        self.0.compute_opaque(key)
    }

    pub fn projection<K: Key, P: ProjectionKey<DeriveFromKey = K>>(
        &mut self,
        derive_from: &OpaqueValue<K>,
        projection_key: &P,
    ) -> DiceResult<P::Value> {
        self.0.projection(derive_from, projection_key)
    }

    pub fn opaque_into_value<K: Key>(
        &mut self,
        derive_from: OpaqueValue<'d, K>,
    ) -> DiceResult<&'d K::Value> {
        Ok(self.0.opaque_into_value(derive_from))
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
    pub fn with_linear_recompute<'a, Func, Fut, T>(
        &'a mut self,
        func: Func,
    ) -> impl Future<Output = T> + use<'a, Func, Fut, T>
    where
        Func: FnOnce(LinearRecomputeDiceComputations<'a>) -> Fut,
        Fut: Future<Output = T>,
    {
        self.0.with_linear_recompute(func)
    }

    /// Perform a number of computations in parallel.
    ///
    /// ## Example
    ///
    /// ```ignore
    /// let mut ctx: &'a DiceComputations = ctx();
    /// let data: String = data();
    /// let keys : Vec<Key> = keys();
    /// let futs = ctx.compute_many(keys.into_iter().map(|k|
    ///     |dice: &mut DiceComputations| -> BoxFuture<String> {
    ///       async move {
    ///         dice.compute(k).await + data
    ///       }.boxed()
    ///     }
    /// ));
    /// futures::future::join_all(futs).await;
    /// ```
    ///
    /// ## Why
    ///
    /// Dice records the dependencies of a key in structured form. Concretely, that means that if
    /// you have two dependencies, `K1` and `K2`, we track whether you depended on them
    /// sequentially:
    ///
    /// ```ignore
    /// let r1 = ctx.compute(&K1).await;
    /// let r2 = ctx.compute(&K2).await;
    /// ```
    ///
    /// Or in parallel:
    ///
    /// ```ignore
    /// let (r1, r2) = ctx.compute2(&K1, &K2).await; // Also `compute_many`, `compute_join`, etc.
    /// ```
    ///
    /// To understand why, consider that the sequential case might instead look like this:
    ///
    /// ```ignore
    /// let r = ctx.compute(&K1).await;
    /// if r == 42 {
    ///     r1 += ctx.compute(&K2).await;
    /// }
    /// ```
    ///
    /// When recomputing your deps to check if they changed, we will reproduce the
    /// parallel/sequential structure. That's important because - in the example above - if the
    /// value of `K1` did indeed change away from 42, initiating evaluation of `K2` may be
    /// inappropriate. At the very least it's wasted work, but it can violate preconditions for
    /// evaluating K2 (though we discourage such preconditions anyway, but they're not always easy
    /// to avoid).
    ///
    /// So: This API. You can still ask for things to be done in parallel, but must do so in a way
    /// that dice can understand.
    ///
    /// If you do not want to put up with this, you have an escape hatch in the form of
    /// `DiceComputations::with_linear_recompute`. `with_linear_recompute` gives you free access to
    /// unstructured dependencies, in exchange for pessimizing the recompute path by doing it in
    /// some linearized order.
    ///
    /// ## Enforcement
    ///
    /// Unfortunately, compliance with this is not strictly enforced. There's nothing that directly
    /// stops you from returning the `ctx` provided to each future from the future and initiating
    /// cross-talk between what ought to be parallel branches. Don't do that; you are subject to
    /// issues like the above if you do.
    // Note that dependencies are still correctly tracked even in the cross-talk case; only
    // recompute structure is violated.
    //
    // A previous design instead had closure signatures like `for<'x> FnOnce(&'x mut
    // DiceComputations<'a>) -> BoxFuture<'x>`. That makes this quite a bit better because you can
    // no longer return the `ctx` from the future, though code trying hard enough could still do bad
    // things with channels or other such constructs. The design encounters a lot of ergonomic and
    // memory disadvantages though, so it's not in use anymore.
    //
    // There may be some temptation to instead have closures like `for<'x> FnOnce(&'x mut
    // DiceComputations<'d>) -> BoxFuture<'x>` (notice `'d` switched for `'a`). This would marginally
    // but not drastically improves ergonomics (hrtbs are still painful), but actually on its own it
    // doesn't work at all; until Rust grows support for writing `for<'x> where 'a: 'x`, that API
    // would require you to return `BoxFuture<'x>` for any `'x` potentially as big as `'d`, which
    // means you could no longer capture references to locals.
    pub fn compute_many<'a, Computes, F, T>(
        &'a mut self,
        computes: Computes,
    ) -> Vec<impl Future<Output = T> + use<'a, 'd, Computes, F, T>>
    where
        Computes: IntoIterator<Item = F>,
        Computes::IntoIter: ExactSizeIterator,
        F: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, T> + Send,
    {
        self.0.compute_many(computes)
    }

    /// Maps the items into computation futures and joins on them.
    ///
    /// ```ignore
    /// let mut ctx: &'a DiceComputations = ctx();
    /// let data: String = data();
    /// let keys : Vec<Key> = keys();
    /// ctx.compute_join(keys, |dice: &mut DiceComputations, k: &Key| {
    ///     async move {
    ///       dice.compute(k).await + data
    ///     }
    ///   }).await;
    /// ```
    pub fn compute_join<'a, Items, Mapper, T, R>(
        &'a mut self,
        items: Items,
        mapper: Mapper,
    ) -> impl Future<Output = Vec<R>> + use<'a, 'd, Items, Mapper, T, R>
    where
        Items: IntoIterator<Item = T>,
        Items::IntoIter: ExactSizeIterator,
        Mapper: FnOnce(&'a mut DiceComputations<'d>, T) -> BoxFuture<'a, R> + Send + Sync + Copy,
        T: Send,
    {
        let futs = self.compute_many(items.into_iter().map(move |v| {
            move |ctx: &'a mut DiceComputations<'d>| -> BoxFuture<'a, R> { mapper(ctx, v) }
        }));
        futures::future::join_all(futs)
    }

    /// Maps the items into computations futures and then returns a future which represents either a
    /// collection of the results or an error.
    pub fn try_compute_join<'a, Items, Mapper, T, R, E>(
        &'a mut self,
        items: Items,
        mapper: Mapper,
    ) -> impl Future<Output = Result<Vec<R>, E>> + use<'a, 'd, Items, Mapper, T, R, E>
    where
        Items: IntoIterator<Item = T>,
        Items::IntoIter: ExactSizeIterator,
        Mapper: FnOnce(&'a mut DiceComputations<'d>, T) -> BoxFuture<'a, Result<R, E>>
            + Send
            + Sync
            + Copy,
        T: Send,
    {
        let futs = self.compute_many(items.into_iter().map(move |v| {
            move |ctx: &'a mut DiceComputations<'d>| -> BoxFuture<'a, Result<R, E>> {
                mapper(ctx, v)
            }
        }));
        crate::future::try_join_all(futs)
    }

    /// Computes all the given tasks in parallel.
    pub fn compute2<'a, Compute1, T, Compute2, U>(
        &'a mut self,
        compute1: Compute1,
        compute2: Compute2,
    ) -> impl Future<Output = (T, U)> + use<'a, 'd, Compute1, T, Compute2, U>
    where
        Compute1: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, T> + Send,
        Compute2: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, U> + Send,
    {
        let (t, u) = self.0.compute2(compute1, compute2);
        futures::future::join(t, u)
    }

    /// Compute all the given tasks in parallel.
    pub fn try_compute2<'a, Compute1, T, Compute2, U, E>(
        &'a mut self,
        compute1: Compute1,
        compute2: Compute2,
    ) -> impl Future<Output = Result<(T, U), E>> + use<'a, 'd, Compute1, T, Compute2, U, E>
    where
        Compute1: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, Result<T, E>> + Send,
        Compute2: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, Result<U, E>> + Send,
    {
        let (t, u) = self.0.compute2(compute1, compute2);
        futures::future::try_join(t, u)
    }

    /// Computes all the given tasks in parallel.
    pub fn compute3<'a, Compute1, T, Compute2, U, Compute3, V>(
        &'a mut self,
        compute1: Compute1,
        compute2: Compute2,
        compute3: Compute3,
    ) -> impl Future<Output = (T, U, V)> + use<'a, 'd, Compute1, T, Compute2, U, Compute3, V>
    where
        Compute1: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, T> + Send,
        Compute2: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, U> + Send,
        Compute3: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, V> + Send,
    {
        let (t, u, v) = self.0.compute3(compute1, compute2, compute3);
        futures::future::join3(t, u, v)
    }

    /// Compute all the given tasks in parallel.
    pub fn try_compute3<'a, Compute1, T, Compute2, U, Compute3, V, E>(
        &'a mut self,
        compute1: Compute1,
        compute2: Compute2,
        compute3: Compute3,
    ) -> impl Future<Output = Result<(T, U, V), E>> + use<'a, 'd, Compute1, T, Compute2, U, Compute3, V, E>
    where
        Compute1: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, Result<T, E>> + Send,
        Compute2: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, Result<U, E>> + Send,
        Compute3: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, Result<V, E>> + Send,
    {
        let (t, u, v) = self.0.compute3(compute1, compute2, compute3);
        futures::future::try_join3(t, u, v)
    }

    /// Used to declare a higher order closure for compute_join and try_compute_join.
    ///
    /// A closure that is created where a compute_join-shaped closure is expected infers its
    /// signature from that expected type and needs no help. This helper provides that same
    /// expected type for closures declared at a distance - stored in a variable, accumulated in
    /// a `Vec`, or created inside an iterator adapter for `compute_many` - where the compiler
    /// would otherwise have nothing to infer the ctx type from.
    pub fn declare_join_closure<'a, T, R, Closure>(closure: Closure) -> Closure
    where
        'd: 'a,
        Closure: FnOnce(&'a mut DiceComputations<'d>, T) -> BoxFuture<'a, R> + Send + Sync + Copy,
    {
        closure
    }

    /// Used to declare a higher order closure for compute2 and compute_many.
    ///
    /// See `declare_join_closure` for when this is needed.
    pub fn declare_closure<'a, R, Closure>(closure: Closure) -> Closure
    where
        'd: 'a,
        Closure: FnOnce(&'a mut DiceComputations<'d>) -> BoxFuture<'a, R>,
    {
        closure
    }

    /// Returns a handle to the DICE data that provides access to global and per-transaction
    /// data without the ability to request new keys.
    pub fn data(&self) -> DiceComputationsData {
        self.0.data()
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

    /// Spawn a computation on a new tokio task.
    ///
    /// This allows spawning work that requires access to the `DiceComputations` context
    /// without running into lifetime issues with `tokio::spawn` requiring `'static` futures.
    /// Dependencies accessed in the spawned task are tracked and merged back into the
    /// parent computation. If the returned future is dropped, the spawned task will be cancelled.
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
        self.0.spawned(closure)
    }
}

pub struct LinearRecomputeDiceComputations<'a>(pub(crate) LinearRecomputeComputations<'a>);

impl LinearRecomputeDiceComputations<'_> {
    pub fn get(&self) -> DiceComputations<'_> {
        self.0.get()
    }

    /// Spawn a computation on a new tokio task.
    ///
    /// See [`DiceComputations::spawned`] for details.
    pub fn spawned<'a, T, Compute>(
        &'a self,
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
        OwningFuture::new(self.get(), |ctx| ctx.spawned(closure).boxed())
    }
}

/// A holder for the user data attached to DICE. APIs that require access to data stored on DICE
/// but that won't request keys can indicate that by accepting a `DiceComputationsData`.
#[derive(Clone, Dupe)]
pub struct DiceComputationsData(pub(crate) ModernDiceComputationsData);

impl DiceComputationsData {
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
}

// This assertion assures we don't unknowingly regress the size of this critical future.
// TODO(cjhopman): We should be able to wrap this in a convenient assertion macro.
#[allow(unused, clippy::diverging_sub_expression)]
fn _assert_dice_compute_future_sizes() {
    #[derive(Allocative, Debug, Clone, PartialEq, Eq, Hash, Pagable)]
    #[pagable_typetag(DiceKeyDyn)]
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

        fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
            NoValueSerialize::<Self::Value>::new()
        }
    }

    mini_vec::size_assert::words_of_async_fn_future!(DiceComputations::compute::<K>, (_, _), 8);

    mini_vec::size_assert::words_of_async_fn_future!(
        DiceComputations::compute2::<_, u32, _, u32>,
        (
            _,
            DiceComputations::declare_closure(|ctx| panic!()),
            DiceComputations::declare_closure(|ctx| panic!())
        ),
        8
    );
}
