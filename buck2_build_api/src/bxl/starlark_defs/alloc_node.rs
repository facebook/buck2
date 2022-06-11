/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::values::{Heap, Value};

use crate::{
    bxl::starlark_defs::nodes::{
        configured::StarlarkConfiguredTargetNode, unconfigured::StarlarkTargetNode,
    },
    nodes::{configured::ConfiguredTargetNode, unconfigured::TargetNode},
};

/// BXL-specific node allocator.
pub trait AllocNode {
    fn alloc(self, heap: &Heap) -> Value;
}

impl AllocNode for TargetNode {
    fn alloc(self, heap: &Heap) -> Value {
        heap.alloc(StarlarkTargetNode(self))
    }
}

impl AllocNode for ConfiguredTargetNode {
    fn alloc(self, heap: &Heap) -> Value {
        heap.alloc(StarlarkConfiguredTargetNode(self))
    }
}
