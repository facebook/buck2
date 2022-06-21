/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::nodes::configured::ConfiguredTargetNode;
use buck2_build_api::nodes::unconfigured::TargetNode;
use starlark::values::Heap;
use starlark::values::Value;

use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;

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
