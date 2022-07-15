/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! "Projection" keys: synchronously computed keys from "opaque" values.

mod tests;

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;
use std::sync::Weak;

use async_trait::async_trait;
use gazebo::dupe::Dupe;

use crate::ctx::UserComputationData;
use crate::incremental::introspection::short_type_name;
use crate::incremental::IncrementalComputeProperties;
use crate::sync_handle::SyncDiceTaskHandle;
use crate::ComputationData;
use crate::Dice;
use crate::DiceData;
use crate::DiceResult;
use crate::GraphNode;
use crate::IncrementalEngine;
use crate::Key;
use crate::StorageProperties;
use crate::StorageType;
use crate::TransactionCtx;

/// Synchronously computed key from an "opaque" value.
pub trait ProjectionKey:
    Clone + PartialEq + Eq + Hash + Display + Debug + Send + Sync + 'static
{
    /// Key of the value that this projection key is computed from.
    type DeriveFromKey: Key;
    /// This projection key is mapped to this value.
    type Value: Dupe + Send + Sync;

    /// Compute the projection key value.
    // Implementation note:
    // This function is called from two places:
    // * `OpaqueValue::projection`, where the projection key is computed synchronously.
    // * `ProjectionKeyAsKey`, where the projection key is computed asynchronously
    //   together with opaque key this key is derived from.
    //   This operation cannot be triggered manually by user,
    //   but it happens implicitly during dependency recompute.
    fn compute(
        &self,
        derive_from: &<<Self as ProjectionKey>::DeriveFromKey as Key>::Value,
        ctx: &DiceProjectionComputations,
    ) -> Self::Value;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool;

    fn validity(x: &Self::Value) -> bool {
        let _ = x;
        true
    }

    fn storage_type(&self) -> StorageType {
        StorageType::LastN(1)
    }

    /// Provides a short informative name for this projection type.
    fn key_type_name() -> &'static str {
        "<unknown_projection>"
    }
}

/// Actual key in the graph (well, actual key is `StorageKeyForKey<ProjectionKeyAsKey<P>>`).
/// `ProjectionKey` alone does not have information about what it is derived from.
/// So we record dependency on projection key, we depend on this key.
#[derive(Debug, derive_more::Display, Clone, PartialEq, Eq, Hash)]
#[display(fmt = "({}, {})", derive_from_key, k)]
pub(crate) struct ProjectionKeyAsKey<P: ProjectionKey> {
    pub(crate) derive_from_key: P::DeriveFromKey,
    pub(crate) k: P,
}

#[derive(Debug)]
pub(crate) struct ProjectionKeyProperties<P: ProjectionKey> {
    _marker: std::marker::PhantomData<P>,
    pub(crate) dice: Weak<Dice>,
}

impl<P: ProjectionKey> ProjectionKeyProperties<P> {
    pub(crate) fn new(dice: &Arc<Dice>) -> Self {
        ProjectionKeyProperties {
            _marker: std::marker::PhantomData,
            dice: Arc::downgrade(dice),
        }
    }
}

impl<P: ProjectionKey> StorageProperties for ProjectionKeyProperties<P> {
    type Key = ProjectionKeyAsKey<P>;
    type Value = P::Value;

    fn storage_type(&self, _key: &Self::Key) -> StorageType {
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
            .recompute_projection(key, transaction_ctx, extra.subrequest(key))
            .await
    }
}

/// Context for projection key computation.
///
/// Only provide access to globals.
pub struct DiceProjectionComputations<'a> {
    pub(crate) dice: &'a Arc<Dice>,
    pub(crate) extra: ComputationData,
}

impl<'a> DiceProjectionComputations<'a> {
    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub fn global_data(&self) -> &DiceData {
        &self.dice.data
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level 'DiceComputation' used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub fn per_transaction_data(&self) -> &UserComputationData {
        &self.extra.user_data
    }
}
