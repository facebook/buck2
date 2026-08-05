/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::execution_types::execution::ExecutionPlatformResolutionPartial;
use buck2_core::execution_types::execution_platforms::ExecutionPlatforms;
use buck2_core::target::label::label::TargetLabel;
use buck2_core::target::target_configured_target_label::TargetConfiguredTargetLabel;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use futures::FutureExt;
use futures::future::BoxFuture;

use crate::configuration::calculation::CellNameForConfigurationResolution;
use crate::configuration::resolved::ConfigurationSettingKey;

pub const EXECUTION_PLATFORMS_BUCKCONFIG: BuckconfigKeyRef = BuckconfigKeyRef {
    section: "build",
    property: "execution_platforms",
};

#[async_trait]
pub trait GetExecutionPlatformsImpl: 'static + Send + Sync {
    fn get_execution_platforms_impl<'a, 'd>(
        &self,
        dice_computations: &'a mut DiceComputations<'d>,
    ) -> BoxFuture<'a, buck2_error::Result<&'d Option<ExecutionPlatforms>>>
    where
        'd: 'a;

    async fn execution_platform_resolution_one_for_cell(
        &self,
        dice: &mut DiceComputations<'_>,
        exec_deps: Arc<[TargetLabel]>,
        toolchain_deps: Arc<[TargetConfiguredTargetLabel]>,
        exec_compatible_with: Arc<[ConfigurationSettingKey]>,
        cell: CellNameForConfigurationResolution,
    ) -> buck2_error::Result<ExecutionPlatformResolutionPartial>;
}

pub static GET_EXECUTION_PLATFORMS: LateBinding<&'static dyn GetExecutionPlatformsImpl> =
    LateBinding::new("EXECUTION_PLATFORMS");

pub trait GetExecutionPlatforms<'d>: Send {
    /// Returns a list of the configured execution platforms. This looks up the providers on the target
    /// configured **in the root cell's buckconfig** with key `build.execution_platforms`. If there's no
    /// value configured, it will return `None` which indicates we should fallback to the legacy execution
    /// platform behavior.
    fn get_execution_platforms<'a>(
        &'a mut self,
    ) -> BoxFuture<'a, buck2_error::Result<&'d Option<ExecutionPlatforms>>>
    where
        'd: 'a;
}

impl<'d> GetExecutionPlatforms<'d> for DiceComputations<'d> {
    fn get_execution_platforms<'a>(
        &'a mut self,
    ) -> BoxFuture<'a, buck2_error::Result<&'d Option<ExecutionPlatforms>>>
    where
        'd: 'a,
    {
        match GET_EXECUTION_PLATFORMS.get() {
            Ok(i) => i.get_execution_platforms_impl(self),
            Err(e) => futures::future::ready(Err(e)).boxed(),
        }
    }
}
