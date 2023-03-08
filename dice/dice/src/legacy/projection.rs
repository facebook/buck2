/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! "Projection" keys: synchronously computed keys from "opaque" values.

use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;
use std::sync::Weak;

use allocative::Allocative;
use async_trait::async_trait;

use crate::api::error::DiceResult;
use crate::api::projection::ProjectionKey;
use crate::api::storage_type::StorageType;
use crate::introspection::graph::short_type_name;
use crate::legacy::ctx::ComputationData;
use crate::legacy::dice_futures::sync_handle::SyncDiceTaskHandle;
use crate::legacy::incremental::graph::storage_properties::StorageProperties;
use crate::legacy::incremental::IncrementalComputeProperties;
use crate::legacy::incremental::IncrementalEngine;
use crate::legacy::DiceLegacy;
use crate::GraphNode;
use crate::TransactionCtx;

/// Actual key in the graph (well, actual key is `StorageKeyForKey<ProjectionKeyAsKey<P>>`).
/// `ProjectionKey` alone does not have information about what it is derived from.
/// So we record dependency on projection key, we depend on this key.
#[derive(Debug, derive_more::Display, Clone, PartialEq, Eq, Hash, Allocative)]
#[display(fmt = "({}, {})", derive_from_key, k)]
pub(crate) struct ProjectionKeyAsKey<P: ProjectionKey> {
    pub(crate) derive_from_key: P::DeriveFromKey,
    pub(crate) k: P,
}

#[derive(Debug, Allocative)]
#[allocative(bound = "")]
pub(crate) struct ProjectionKeyProperties<P: ProjectionKey> {
    _marker: std::marker::PhantomData<P>,
    pub(crate) dice: Weak<DiceLegacy>,
}

impl<P: ProjectionKey> ProjectionKeyProperties<P> {
    pub(crate) fn new(dice: &Arc<DiceLegacy>) -> Self {
        ProjectionKeyProperties {
            _marker: std::marker::PhantomData,
            dice: Arc::downgrade(dice),
        }
    }
}

impl<P: ProjectionKey> StorageProperties for ProjectionKeyProperties<P> {
    type Key = ProjectionKeyAsKey<P>;
    type Value = P::Value;

    fn storage_type(&self) -> StorageType {
        StorageType::LastN(1)
    }

    fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
        P::equality(x, y)
    }

    fn validity(&self, x: &Self::Value) -> bool {
        P::validity(x)
    }

    /// Provides a short informative name for this projection type.
    fn key_type_name() -> &'static str {
        short_type_name(std::any::type_name::<Self>())
    }

    fn to_key_any(key: &Self::Key) -> &dyn std::any::Any {
        &key.k
    }
}

#[async_trait]
impl<P: ProjectionKey> IncrementalComputeProperties for ProjectionKeyProperties<P> {
    type DiceTask = SyncDiceTaskHandle<Self>;

    async fn recompute(
        key: &Self::Key,
        engine: &Arc<IncrementalEngine<Self>>,
        transaction_ctx: &Arc<TransactionCtx>,
        extra: &ComputationData,
    ) -> DiceResult<GraphNode<Self>> {
        engine
            .recompute_projection(key, transaction_ctx, extra.subrequest::<Self>(key)?)
            .await
    }
}
