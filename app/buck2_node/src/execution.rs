/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::execution_types::execution_platforms::ExecutionPlatforms;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

pub const EXECUTION_PLATFORMS_BUCKCONFIG: BuckconfigKeyRef = BuckconfigKeyRef {
    section: "build",
    property: "execution_platforms",
};

#[async_trait]
pub trait GetExecutionPlatformsImpl: 'static + Send + Sync {
    async fn get_execution_platforms_impl(
        &self,
        dice_computations: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Option<ExecutionPlatforms>>;
}

pub static GET_EXECUTION_PLATFORMS: LateBinding<&'static dyn GetExecutionPlatformsImpl> =
    LateBinding::new("EXECUTION_PLATFORMS");

#[allow(async_fn_in_trait)]
pub trait GetExecutionPlatforms: Send {
    /// Returns a list of the configured execution platforms. This looks up the providers on the target
    /// configured **in the root cell's buckconfig** with key `build.execution_platforms`. If there's no
    /// value configured, it will return `None` which indicates we should fallback to the legacy execution
    /// platform behavior.
    async fn get_execution_platforms(&mut self) -> buck2_error::Result<Option<ExecutionPlatforms>>;
}

impl GetExecutionPlatforms for DiceComputations<'_> {
    async fn get_execution_platforms(&mut self) -> buck2_error::Result<Option<ExecutionPlatforms>> {
        GET_EXECUTION_PLATFORMS
            .get()?
            .get_execution_platforms_impl(self)
            .await
    }
}
