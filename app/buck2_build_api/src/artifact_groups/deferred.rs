/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::deferred::key::DeferredHolderKey;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use starlark::values::OwnedFrozenValueTyped;

use crate::deferred::calculation::lookup_deferred_holder;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;

#[derive(Hash, Eq, PartialEq, Clone, Dupe, Display, Debug, Allocative)]
#[display("{:?}", self)]
pub struct TransitiveSetKey(DeferredHolderKey, TransitiveSetIndex);

impl TransitiveSetKey {
    pub fn new(key: DeferredHolderKey, id: TransitiveSetIndex) -> Self {
        Self(key, id)
    }

    pub fn holder_key(&self) -> &DeferredHolderKey {
        &self.0
    }

    pub fn index(&self) -> TransitiveSetIndex {
        self.1
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Dupe, Copy, Display, Debug, Allocative)]
#[display("{:?}", self)]
/// Index for the transitive set data in the analysis result
pub struct TransitiveSetIndex(pub(crate) u32);

impl TransitiveSetKey {
    pub async fn lookup(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<OwnedFrozenValueTyped<FrozenTransitiveSet>> {
        lookup_deferred_holder(ctx, &self.0)
            .await?
            .lookup_transitive_set(self)
    }
}

impl TransitiveSetIndex {
    pub fn testing_new(v: u32) -> Self {
        Self(v)
    }
}
