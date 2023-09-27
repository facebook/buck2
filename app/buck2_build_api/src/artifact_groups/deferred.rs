/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_artifact::deferred::data::DeferredData;
use dupe::Dupe;
use starlark::values::OwnedFrozenValue;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::Value;

use crate::deferred::types::AnyValue;
use crate::deferred::types::TrivialDeferred;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;

pub type TransitiveSetKey = DeferredData<DeferredTransitiveSetData>;

#[derive(Clone, Dupe, Debug, Allocative)]
pub struct DeferredTransitiveSetData(
    #[allocative(skip)] // TODO(nga): visit heap.
    pub(super)  OwnedFrozenValueTyped<FrozenTransitiveSet>,
);

impl TrivialDeferred for DeferredTransitiveSetData {
    fn as_any_value(&self) -> &dyn AnyValue {
        self
    }

    fn provide<'a>(&'a self, _demand: &mut provider::Demand<'a>) {}
}

impl DeferredTransitiveSetData {
    pub fn testing_new(value: OwnedFrozenValue) -> DeferredTransitiveSetData {
        DeferredTransitiveSetData(value.downcast_anyhow().unwrap())
    }

    pub fn as_transitive_set(&self) -> OwnedFrozenValueTyped<FrozenTransitiveSet> {
        self.0.dupe()
    }

    pub fn as_value(&self) -> Value {
        self.0.to_value()
    }
}
