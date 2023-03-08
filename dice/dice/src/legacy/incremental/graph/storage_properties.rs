/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::fmt::Debug;

use allocative::Allocative;
use dupe::Dupe;

use crate::api::storage_type::StorageType;
use crate::legacy::incremental::Computable;

/// Defines key/value operations in the versioned graph storage.
pub(crate) trait StorageProperties:
    Allocative + Debug + Send + Sync + Sized + 'static
{
    /// Type of the key.
    type Key: Computable + 'static;
    /// Type of the value.
    type Value: Allocative + Dupe + Send + Sync + 'static;
    fn key_type_name() -> &'static str;

    /// Returns the Key as an Any. This should be an Any for the user-provided
    /// type (or ProjectionKeyAsKey for projections) so that users can potentially
    /// inspect its concrete type.
    fn to_key_any(key: &Self::Key) -> &dyn Any;

    /// How long the value should be stored.
    fn storage_type(&self) -> StorageType;
    /// Are values equal?
    fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool;
    /// Is computed value valid (or transient)?
    fn validity(&self, x: &Self::Value) -> bool;
}

#[cfg(test)]
pub(crate) mod testing {
    use std::fmt;
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use dupe::Dupe;

    use crate::api::error::DiceResult;
    use crate::api::storage_type::StorageType;
    use crate::legacy::ctx::ComputationData;
    use crate::legacy::incremental::graph::storage_properties::StorageProperties;
    use crate::legacy::incremental::Computable;
    use crate::legacy::incremental::IncrementalComputeProperties;
    use crate::legacy::incremental::IncrementalEngine;
    use crate::GraphNode;
    use crate::TransactionCtx;
    use crate::WeakDiceFutureHandle;

    /// Simple storage key for tests.
    ///
    /// * `PartialEq` is used for value equality
    /// * values are always considered valid
    /// * storage type is configured
    #[derive(Allocative)]
    #[allocative(bound = "")]
    pub(crate) struct StoragePropertiesLastN<T: Computable, V> {
        n: usize,
        _t: std::marker::PhantomData<fn(T)>,
        _v: std::marker::PhantomData<fn(V)>,
    }

    #[async_trait]
    impl<T: Computable, V: PartialEq + Dupe + Allocative + Send + Sync + 'static>
        IncrementalComputeProperties for StoragePropertiesLastN<T, V>
    {
        type DiceTask = WeakDiceFutureHandle<Self>;

        async fn recompute(
            _key: &Self::Key,
            _engine: &Arc<IncrementalEngine<Self>>,
            _transaction_ctx: &Arc<TransactionCtx>,
            _extra: &ComputationData,
        ) -> DiceResult<GraphNode<Self>> {
            unimplemented!("not needed for test")
        }
    }

    impl<T: Computable, V> fmt::Debug for StoragePropertiesLastN<T, V> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("StorageKeyLastN")
                .field("n", &self.n)
                .finish_non_exhaustive()
        }
    }

    impl<K: Computable, V> Default for StoragePropertiesLastN<K, V> {
        fn default() -> Self {
            StoragePropertiesLastN::new(1)
        }
    }

    impl<N: Computable, V> StoragePropertiesLastN<N, V> {
        pub(crate) fn new(n: usize) -> Self {
            Self {
                n,
                _t: std::marker::PhantomData,
                _v: std::marker::PhantomData,
            }
        }
    }

    impl<T: Computable, V: PartialEq + Dupe + Allocative + Send + Sync + 'static> StorageProperties
        for StoragePropertiesLastN<T, V>
    {
        type Key = T;
        type Value = V;

        fn key_type_name() -> &'static str {
            "TestingLastN"
        }

        fn storage_type(&self) -> StorageType {
            StorageType::LastN(self.n)
        }

        fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }

        fn validity(&self, x: &Self::Value) -> bool {
            let _ = x;
            true
        }

        fn to_key_any(key: &Self::Key) -> &dyn std::any::Any {
            key
        }
    }
}
