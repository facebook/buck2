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
use buck2_node::configuration::execution::ExecutionPlatform;
use buck2_node::configuration::resolved::ResolvedConfiguration;

pub mod calculation;

pub type ExecutionPlatforms = Arc<ExecutionPlatformsData>;

#[derive(Debug, Allocative)]
pub enum ExecutionPlatformFallback {
    Error,
    UseUnspecifiedExec,
    Platform(ExecutionPlatform),
}

#[derive(Debug, Allocative)]
pub struct ExecutionPlatformsData {
    platforms: Vec<ExecutionPlatform>,
    fallback: ExecutionPlatformFallback,
}

impl ExecutionPlatformsData {
    pub fn new(platforms: Vec<ExecutionPlatform>, fallback: ExecutionPlatformFallback) -> Self {
        Self {
            platforms,
            fallback,
        }
    }

    pub fn candidates(&self) -> impl Iterator<Item = &ExecutionPlatform> {
        self.platforms.iter()
    }

    pub fn fallback(&self) -> &ExecutionPlatformFallback {
        &self.fallback
    }
}
