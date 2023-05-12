/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Demand;

use allocative::Allocative;
use anyhow::Context;
use dupe::Dupe;
use starlark::values::OwnedFrozenValue;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::Value;

use crate::deferred::types::AnyValue;
use crate::deferred::types::DeferredData;
use crate::deferred::types::TrivialDeferred;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;

pub type TransitiveSetKey = DeferredData<DeferredTransitiveSetData>;

#[derive(Clone, Dupe, Debug, Allocative)]
pub struct DeferredTransitiveSetData(pub(super) OwnedFrozenValue);

impl TrivialDeferred for DeferredTransitiveSetData {
    fn as_any_value(&self) -> &dyn AnyValue {
        self
    }

    fn provide<'a>(&'a self, _demand: &mut Demand<'a>) {}
}

impl DeferredTransitiveSetData {
    pub fn testing_new(value: OwnedFrozenValue) -> DeferredTransitiveSetData {
        DeferredTransitiveSetData(value)
    }

    pub fn as_transitive_set(&self) -> anyhow::Result<OwnedFrozenValueTyped<FrozenTransitiveSet>> {
        self.0.dupe().downcast().ok().context("Invalid deferred")
    }

    pub fn as_value(&self) -> Value {
        self.0.value()
    }
}
