/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use gazebo::prelude::*;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValue;
use starlark::values::Value;

use crate::deferred::types::Deferred;
use crate::deferred::types::DeferredCtx;
use crate::deferred::types::DeferredData;
use crate::deferred::types::DeferredInput;
use crate::deferred::types::DeferredValue;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;

pub type TransitiveSetKey = DeferredData<DeferredTransitiveSetData>;

#[derive(Clone, Dupe, Debug)]
pub struct DeferredTransitiveSetData(pub(super) OwnedFrozenValue);

impl DeferredTransitiveSetData {
    pub fn as_transitive_set(&self) -> anyhow::Result<&TransitiveSet> {
        TransitiveSet::from_value(self.0.value()).context("Invalid deferred")
    }

    pub fn as_value(&self) -> Value {
        self.0.value()
    }
}

#[derive(Clone, Dupe)]
pub struct DeferredTransitiveSet(pub(super) DeferredTransitiveSetData);

impl Deferred for DeferredTransitiveSet {
    type Output = DeferredTransitiveSetData;

    fn inputs(&self) -> &IndexSet<DeferredInput> {
        static INPUTS: Lazy<IndexSet<DeferredInput>> = Lazy::new(IndexSet::new);
        &INPUTS
    }

    fn execute(&self, _ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValue<Self::Output>> {
        Ok(DeferredValue::Ready(self.0.dupe()))
    }
}
