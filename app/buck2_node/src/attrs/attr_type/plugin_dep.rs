/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::plugins::PluginKind;
use dupe::Dupe;

#[derive(Debug, Eq, PartialEq, Hash, Allocative, Clone, Dupe)]
pub struct PluginDepAttrType {
    kind: PluginKind,
}

impl PluginDepAttrType {
    pub fn new(kind: PluginKind) -> Self {
        Self { kind }
    }

    pub fn kind(&self) -> &PluginKind {
        &self.kind
    }
}
