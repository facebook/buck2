/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[cfg(not(unix))]
pub mod memory_tracker {
    use std::sync::Arc;

    use allocative::Allocative;
    use buck2_common::init::ResourceControlConfig;

    #[derive(Allocative)]
    pub struct MemoryTracker {}

    pub type MemoryTrackerHandle = Arc<MemoryTracker>;

    pub async fn create_memory_tracker(
        _resource_control_config: &ResourceControlConfig,
    ) -> buck2_error::Result<Option<MemoryTrackerHandle>> {
        Ok(None)
    }
}

#[cfg(unix)]
pub mod memory_tracker;
