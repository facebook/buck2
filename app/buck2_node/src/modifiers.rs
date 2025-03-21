/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;

pub const MODIFIER_METADATA_KEY: &str = "buck.cfg_modifiers";
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative)]
pub struct PackageCfgModifiersValue(Arc<serde_json::Value>);

impl PackageCfgModifiersValue {
    pub fn new(v: serde_json::Value) -> Self {
        Self(Arc::new(v))
    }

    pub fn to_value(&self) -> serde_json::Value {
        (*self.0).clone()
    }
}
