/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;

use crate::dice_task::DiceTask;
use crate::incremental::graph::GraphNode;
use crate::ComputationData;
use crate::IncrementalComputeProperties;
use crate::IncrementalEngine;
use crate::TransactionCtx;
use crate::ValueWithDeps;
use crate::WeakDiceFutureHandle;

#[async_trait]
pub(crate) trait Recompute: IncrementalComputeProperties {
    /// Dice task executed in the `IncrementalEngine` for compute or recompute.
    type DiceTask: DiceTask;

    async fn recompute(
        key: &Self::Key,
        engine: &Arc<IncrementalEngine<Self>>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
    ) -> GraphNode<Self>;
}

#[async_trait]
pub(crate) trait Evaluator:
    IncrementalComputeProperties<DiceTask = WeakDiceFutureHandle<Self>>
{
    async fn eval(
        &self,
        k: &Self::Key,
        transaction_ctx: Arc<TransactionCtx>,
        extra: ComputationData,
    ) -> ValueWithDeps<Self::Value>;
}

#[cfg(test)]
pub(crate) mod testing {
    use std::fmt;
    use std::fmt::Debug;
    use std::fmt::Display;
    use std::future::Future;
    use std::hash::Hash;
    use std::pin::Pin;
    use std::sync::Arc;

    use async_trait::async_trait;
    use gazebo::prelude::*;

    use crate::incremental::evaluator::Evaluator;
    use crate::incremental::graph::GraphNode;
    use crate::incremental::IncrementalComputeProperties;
    use crate::ComputationData;
    use crate::DiceResult;
    use crate::IncrementalEngine;
    use crate::StorageProperties;
    use crate::StorageType;
    use crate::TransactionCtx;
    use crate::ValueWithDeps;
    use crate::WeakDiceFutureHandle;

    /// Evaluator which panics on attempt to evaluate.
    #[derive(Default_)]
    pub(crate) struct EvaluatorUnreachable<K, V> {
        _kv: std::marker::PhantomData<fn(K) -> V>,
    }

    impl<K, V> fmt::Debug for EvaluatorUnreachable<K, V> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("EvaluatorUnreachable")
                .finish_non_exhaustive()
        }
    }

    impl<
        K: Send + Sync + Hash + Eq + Display + Debug + Clone + 'static,
        V: Send + Sync + PartialEq + Dupe + 'static,
    > StorageProperties for EvaluatorUnreachable<K, V>
    {
        type Key = K;
        type Value = V;

        fn key_type_name() -> &'static str {
            "TestingUnreachable"
        }

        fn storage_type(&self, _key: &Self::Key) -> StorageType {
            unreachable!()
        }

        fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }

        fn validity(&self, _x: &Self::Value) -> bool {
            true
        }
    }

    #[async_trait]
    impl<
        K: Send + Sync + Hash + Eq + Display + Debug + Clone + 'static,
        V: Send + Sync + PartialEq + Dupe + 'static,
    > IncrementalComputeProperties for EvaluatorUnreachable<K, V>
    {
        type DiceTask = WeakDiceFutureHandle<Self>;

        async fn recompute(
            _key: &Self::Key,
            _engine: &Arc<IncrementalEngine<Self>>,
            _transaction_ctx: &Arc<TransactionCtx>,
            _extra: &ComputationData,
        ) -> DiceResult<GraphNode<Self>> {
            unreachable!()
        }
    }

    #[async_trait]
    impl<
        K: Send + Sync + Hash + Eq + Display + Debug + Clone + 'static,
        V: Send + Sync + PartialEq + Dupe + 'static,
    > Evaluator for EvaluatorUnreachable<K, V>
    {
        async fn eval(
            &self,
            _k: &Self::Key,
            _transaction_ctx: Arc<TransactionCtx>,
            _extra: ComputationData,
        ) -> ValueWithDeps<Self::Value> {
            unreachable!()
        }
    }

    /// Evaluator which invokes boxed closure on evaluation.
    pub(crate) struct EvaluatorFn<K, V> {
        f: Pin<
            Box<
                dyn Fn(K) -> Pin<Box<dyn Future<Output = ValueWithDeps<V>> + Sync + Send + 'static>>
                    + Send
                    + Sync
                    + 'static,
            >,
        >,
    }

    impl<K, V> EvaluatorFn<K, V> {
        pub(crate) fn new<F, FUT>(f: F) -> Self
        where
            FUT: Future<Output = ValueWithDeps<V>> + Sync + Send + 'static,
            F: FnOnce(K) -> FUT + Clone + 'static + Sync + Send + 'static,
        {
            Self {
                f: Box::pin(move |k| {
                    let f = f.clone();
                    Box::pin(f(k))
                }),
            }
        }
    }

    impl<K, V> Debug for EvaluatorFn<K, V> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("EvaluatorFn").finish_non_exhaustive()
        }
    }

    impl<
        K: Clone + Eq + Hash + Display + Debug + Send + Sync + 'static,
        V: Dupe + PartialEq + Send + Sync + 'static,
    > StorageProperties for EvaluatorFn<K, V>
    {
        type Key = K;
        type Value = V;

        fn key_type_name() -> &'static str {
            "EvaluatorFn"
        }

        fn storage_type(&self, _key: &Self::Key) -> StorageType {
            StorageType::LastN(1)
        }

        fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }

        fn validity(&self, _x: &Self::Value) -> bool {
            true
        }
    }

    #[async_trait]
    impl<K, V> IncrementalComputeProperties for EvaluatorFn<K, V>
    where
        K: Clone + Eq + Hash + Display + Debug + Send + Sync + 'static,
        V: Dupe + PartialEq + Send + Sync + 'static,
    {
        type DiceTask = WeakDiceFutureHandle<Self>;

        async fn recompute(
            key: &K,
            engine: &Arc<IncrementalEngine<Self>>,
            transaction_ctx: &Arc<TransactionCtx>,
            extra: &ComputationData,
        ) -> DiceResult<GraphNode<Self>> {
            engine
                .eval_entry_versioned(key, transaction_ctx, extra.subrequest(key)?)
                .await
        }
    }

    #[async_trait]
    impl<K, V> Evaluator for EvaluatorFn<K, V>
    where
        K: Clone + Eq + Hash + Display + Debug + Send + Sync + 'static,
        V: Dupe + PartialEq + Send + Sync + 'static,
    {
        async fn eval(
            &self,
            k: &K,
            _: Arc<TransactionCtx>,
            _extra: ComputationData,
        ) -> ValueWithDeps<V> {
            (self.f)(k.clone()).await
        }
    }
}
