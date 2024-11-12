/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(unix)]
pub(crate) mod memory_tracker {
    use std::sync::Arc;

    use allocative::Allocative;

    #[derive(Allocative)]
    pub struct MemoryTracker {}

    impl MemoryTracker {
        pub async fn start_tracking() -> anyhow::Result<Arc<Self>> {
            Ok(Arc::new(MemoryTracker {}))
        }
    }
}

#[cfg(not(unix))]
pub(crate) mod memory_tracker {
    use allocative::Allocative;

    #[derive(Allocative)]
    pub struct MemoryTracker {}
}

pub use memory_tracker::*;
