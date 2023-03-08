/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use allocative::Allocative;
use async_trait::async_trait;
use dupe::Dupe;

use crate::api::computations::DiceComputations;
use crate::api::storage_type::StorageType;
use crate::introspection::graph::short_type_name;

/// The computation Key that maps to a value. The key will be used as an index
/// for caching the computed values.
///
/// ## Cloning
/// The Key will need to be cloned. This occurs whenever the key is recomputed
/// and needs to be stored. During normal lookups, only a reference is needed.
///
/// The result also needs to be cloned whenever it is requested from DICE.
/// Cloning therefore should be cheap.
#[async_trait]
pub trait Key: Allocative + Debug + Display + Clone + Eq + Hash + Send + Sync + 'static {
    type Value: Allocative + Dupe + Send + Sync + 'static;

    /// Provides a short informative name for this key type.
    fn key_type_name() -> &'static str {
        short_type_name(std::any::type_name::<Self>())
    }

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value;

    /// If computed value is equal to previously cached value,
    /// DICE won't invalidate graph nodes depending on this node.
    ///
    /// It is safe to return `false` from this function when values are equal,
    /// but returning `true` when values are not equal would result
    /// in inconsistent graph state.
    fn equality(x: &Self::Value, y: &Self::Value) -> bool;

    /// If the computed value is `false`, DICE will consider that result to be a transient value
    /// that won't be re-used on subsequent computations. It will, however, reuse that value for
    /// all on-going computations at the current version.
    ///
    /// The default here is true, but computations should override this if its expected that they
    /// may occasionally produce transient values.
    fn validity(_x: &Self::Value) -> bool {
        true
    }

    fn storage_type() -> StorageType {
        StorageType::LastN(1)
    }
}
