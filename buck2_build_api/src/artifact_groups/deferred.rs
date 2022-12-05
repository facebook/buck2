/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use anyhow::Context;
use gazebo::prelude::*;
use starlark::values::OwnedFrozenValue;
use starlark::values::Value;

use crate::deferred::types::DeferredData;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;

pub type TransitiveSetKey = DeferredData<DeferredTransitiveSetData>;

#[derive(Clone, Dupe, Debug, Allocative)]
pub struct DeferredTransitiveSetData(pub(super) OwnedFrozenValue);

impl DeferredTransitiveSetData {
    pub fn as_transitive_set(&self) -> anyhow::Result<&TransitiveSet> {
        TransitiveSet::from_value(self.0.value()).context("Invalid deferred")
    }

    pub fn as_value(&self) -> Value {
        self.0.value()
    }
}
