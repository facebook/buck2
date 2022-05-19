/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::{Duration, Instant};

/// The common module provides a macro that implements a set of benchmarks for you.
/// These are accessed through the `benchmark!` macro.
///
/// Example implementations can be found under `benches/`.
/// To quickly use the macro, implement the `BenchmarkComputationsPrerequisites` trait.
/// The methods to implement are roughly divided into providing routines to update and compute on the underlying DICE ctx
/// and methods to return changes to compute for a given benchmark.
///
/// After the trait is implemented, `benchmark!` can be called using the name of the implementee.
/// The implementee will not be instantiated inside the macro.
///
/// ```ignore
/// mod common;
/// use common::BenchmarkComputationsPrerequisites;
///
/// struct Foo;
/// impl BenchmarkComputationsPrerequisites for Foo {
/// ...
/// }
///
/// benchmark!(Foo);
/// ```
use async_trait::async_trait;
use criterion::{black_box, Criterion};
use dice::{cycles::DetectCycles, Dice, DiceTransaction};
use futures::{
    stream::{self, StreamExt},
    Future,
};

/// Implementers of this trait gain access to the `benchmark!` macro.
/// The type that this trait is implemented must not need to be constructed.
#[async_trait]
pub trait BenchmarkComputationsPrerequisites {
    /// The type that will be used to update the DICE state
    type Updater: Send + Sync;
    /// The type that will be used to perform a computation
    type Key: Clone + Send + Sync;
    /// The resulting value of a computation
    type Value;

    /// Generate a fresh computation state
    async fn fresh(ctx: DiceTransaction) -> DiceTransaction;

    /// Compute the value of the given key on the DICE state
    async fn compute(ctx: &DiceTransaction, key: Self::Key) -> Self::Value;

    /// Update the DICE state with the given set of changes
    async fn update<I>(ctx: DiceTransaction, keys: I) -> DiceTransaction
    where
        I::IntoIter: Send,
        I: IntoIterator<Item = Self::Updater> + Send + Sync;

    /// Provide a list of changes and a key to query to create a cache miss
    /// A good cache miss should require recomputation of large chunk of the graph, but not all of the graph
    fn invalidated_recompute() -> (Vec<Self::Updater>, Self::Key);

    /// Provide a list of changes and a key to query to create a scenario
    /// where we recompute a set of keys that evaluates to the same value as previously,
    /// which then avoids recomputing the portion of the graph that depends on these keys, known as the early-cutoff scenario
    fn early_cutoff_recompute() -> (Vec<Self::Updater>, Self::Key);

    /// Provide a list of sample updates to perform
    fn get_sample_updates() -> Vec<Self::Updater>;

    /// Return a key to compute on
    fn get_sample_key() -> Self::Key;
}

/// Auto-implemented trait for `benchmark!` functionality
#[async_trait]
pub(crate) trait Benchmarker: BenchmarkComputationsPrerequisites {
    /// Helper method to DRY Criterion boilerplate.
    /// - name: The name of the benchmark
    /// - c: A Criterion benchmarker
    /// - before: Actions to take before each benchmark
    /// - action: the action to be benchmarked
    fn benchmark_helper<InFut, OutFut, X, Input>(
        name: &str,
        c: &mut Criterion,
        before: impl Fn(DiceTransaction) -> InFut + Copy,
        action: impl Fn(Input) -> OutFut + Copy,
    ) where
        InFut: Future<Output = Input>,
        OutFut: Future<Output = X>,
    {
        let rt = tokio::runtime::Runtime::new().unwrap();
        c.bench_function(name, |bencher| {
            bencher.to_async(&rt).iter_custom(|iters| {
                let futs = (0..iters).map(|_| async move {
                    let ctx =
                        Self::fresh(Dice::builder().build(DetectCycles::Disabled).ctx()).await;
                    let input = before(ctx).await;
                    let start = Instant::now();
                    black_box(action(input).await);
                    start.elapsed()
                });

                // add up the time elapsed for each call
                stream::iter(futs).fold(Duration::default(), |acc, x| async move { acc + x.await })
            })
        });
    }

    /// Compute from scratch
    fn benchmark_fresh(c: &mut Criterion) {
        let before = |ctx| async { ctx };
        let action = |ctx| async move { Self::compute(&ctx, Self::get_sample_key()).await };
        Self::benchmark_helper("fresh", c, before, action);
    }

    /// Only recalculate one dependency for computation
    fn benchmark_cached(c: &mut Criterion) {
        let before = |ctx| async {
            let key = Self::get_sample_key();
            Self::compute(&ctx, key.clone()).await;

            //update the equation to mark it as dirty
            (ctx, key)
        };
        let action = |(ctx, key)| async move { Self::compute(&ctx, key).await };

        Self::benchmark_helper("cached", c, before, action);
    }

    /// Require recomputation of children.
    fn benchmark_invalidated_recompute(c: &mut Criterion) {
        let before = |ctx| {
            let (to_dirty, key) = Self::invalidated_recompute();
            async move {
                Self::compute(&ctx, key.clone()).await;
                (Self::update(ctx, to_dirty).await, key)
            }
        };
        let action = |(ctx, key)| async move { Self::compute(&ctx, key).await };

        Self::benchmark_helper("invalidated_recompute", c, before, action);
    }

    /// Do not require recomputation of children.
    fn benchmark_early_cutoff_recompute(c: &mut Criterion) {
        let before = |ctx| {
            let (to_dirty, key) = Self::early_cutoff_recompute();
            async move {
                Self::compute(&ctx, key.clone()).await;
                (Self::update(ctx, to_dirty).await, key)
            }
        };
        let action = |(ctx, key)| async move { Self::compute(&ctx, key).await };

        Self::benchmark_helper("early_cutoff_recompute", c, before, action);
    }

    fn benchmark_update(c: &mut Criterion) {
        let before = |ctx| async move { (ctx, Self::get_sample_updates()) };
        let action = |(ctx, updates)| async move { Self::update(ctx, updates).await };

        Self::benchmark_helper("update", c, before, action);
    }
}

impl<T: BenchmarkComputationsPrerequisites> Benchmarker for T {}

/// Automatically implement a main method with a set of benchmarks.
/// Callee must provide the identifier for a struct which implements `BenchmarkComputationsPrerequisites`
///
/// Provides the following benchmarks:
/// - Fresh
/// - Incremental
/// - Cache Miss
/// - Cache Hit
/// - Update
///
/// For more information about each computation, see trait docs for this module.
#[macro_export]
macro_rules! benchmark {
    ($benchmarkee: ident) => {
        use crate::common::Benchmarker;
        use criterion::{criterion_group, criterion_main, Criterion};
        type LocalT = paste::item! {[<$benchmarkee:camel>]};

        fn benchmark_fresh(c: &mut Criterion) {
            LocalT::benchmark_fresh(c);
        }

        fn benchmark_incremental(c: &mut Criterion) {
            LocalT::benchmark_cached(c);
        }

        fn benchmark_invalidated_recompute(c: &mut Criterion) {
            LocalT::benchmark_invalidated_recompute(c);
        }

        fn benchmark_early_cutoff_recompute(c: &mut Criterion) {
            LocalT::benchmark_early_cutoff_recompute(c);
        }

        fn benchmark_update(c: &mut Criterion) {
            LocalT::benchmark_update(c);
        }

        criterion_group![
            name = $benchmarkee;
            config = Criterion::default().sample_size(500);
            targets =
                benchmark_fresh,
                benchmark_incremental,
                benchmark_invalidated_recompute,
                benchmark_early_cutoff_recompute,
                benchmark_update
        ];
        criterion_main!($benchmarkee);
    };
}
