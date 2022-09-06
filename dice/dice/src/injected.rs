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

use async_trait::async_trait;
use gazebo::dupe::Dupe;

use crate::DiceComputations;
use crate::Key;
use crate::StorageType;

/// Specialized version of 'Key' above. This type of Key is never computed. It
/// should always be injected onto the graph before being requested via
/// `DiceCtx::changed_to(Key, Value)`. Therefore, the `compute` function on this
/// Key is never called.
///
/// TODO when rust gets smarter about negative trait bounds and specializations,
/// we can make injected and normal key types disjoint, and not have this clone issue nor
/// require a `panic!` implementation in `compute` function, both of which are
/// horrible and breaks semantics of traits.
pub trait InjectedKey: Clone + Debug + Display + Send + Sync + Eq + Hash + 'static {
    type Value: Dupe + Send + Sync + 'static;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool;
}

#[async_trait]
impl<K> Key for K
where
    K: InjectedKey,
{
    type Value = K::Value;

    async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
        panic!(
            "Injected Keys must be injected onto the graph before being requested, hence \
            computes should never be called; however, `compute` on `{:?}` was called.",
            self
        )
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        Self::compare(x, y)
    }

    fn storage_type(&self) -> StorageType {
        // if we store more than usize max value, we are in trouble.
        StorageType::LastN(usize::max_value())
    }
}
