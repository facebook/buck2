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
use gazebo::dupe::Dupe;
use starlark_map::small_map::SmallMap;

use crate::compatibility::IncompatiblePlatformReason;
use crate::configuration::execution::ExecutionPlatform;

/// The constraint introduced on execution platform resolution by
/// a toolchain rule (reached via a toolchain_dep).
#[derive(Dupe, Clone, PartialEq, Eq, Allocative)]
pub struct ToolchainConstraints {
    // We know the set of execution platforms is fixed throughout the build,
    // so we can record just the ones we are incompatible with,
    // and assume all others we _are_ compatible with.
    incompatible: Arc<SmallMap<ExecutionPlatform, Arc<IncompatiblePlatformReason>>>,
}

impl ToolchainConstraints {
    pub fn new(incompatible: SmallMap<ExecutionPlatform, Arc<IncompatiblePlatformReason>>) -> Self {
        Self {
            incompatible: Arc::new(incompatible),
        }
    }

    pub fn allows(
        &self,
        exec_platform: &ExecutionPlatform,
    ) -> Result<(), Arc<IncompatiblePlatformReason>> {
        match self.incompatible.get(exec_platform) {
            None => Ok(()),
            Some(e) => Err(e.dupe()),
        }
    }
}
