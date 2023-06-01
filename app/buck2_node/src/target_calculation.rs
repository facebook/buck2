/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

#[async_trait]
pub trait ConfiguredTargetCalculationImpl: Send + Sync + 'static {
    async fn get_configured_target(
        &self,
        ctx: &DiceComputations,
        target: &TargetLabel,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<ConfiguredTargetLabel>;
}

pub static CONFIGURED_TARGET_CALCULATION: LateBinding<
    &'static dyn ConfiguredTargetCalculationImpl,
> = LateBinding::new("CONFIGURED_TARGET_CALCULATION");

#[async_trait]
pub trait ConfiguredTargetCalculation {
    /// Returns the Configuration for an unconfigured TargetLabel or ProvidersLabel.
    ///
    /// This performs "target platform resolution" on the provided target and returns the configured
    /// result.
    ///
    /// Normally, a TargetLabel and ProvidersLabel would
    /// get its Configuration based on the context it's being requested in (i.e configuration is
    /// passed down from higher nodes). For top-level requested things, though, we will have an
    /// unconfigured (or "lightly"-configured) thing and the Configuration will be determined as
    /// a mix of the global Configuration, the target's `default_target_platform` and
    /// (potentially) self-transitions on that node.
    async fn get_configured_target(
        &self,
        target: &TargetLabel,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<ConfiguredTargetLabel>;

    async fn get_configured_provider_label(
        &self,
        target: &ProvidersLabel,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<ConfiguredProvidersLabel>;

    async fn get_default_configured_target(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<ConfiguredTargetLabel>;
}

#[async_trait]
impl ConfiguredTargetCalculation for DiceComputations {
    async fn get_configured_target(
        &self,
        target: &TargetLabel,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<ConfiguredTargetLabel> {
        CONFIGURED_TARGET_CALCULATION
            .get()?
            .get_configured_target(self, target, global_target_platform)
            .await
    }

    async fn get_configured_provider_label(
        &self,
        target: &ProvidersLabel,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<ConfiguredProvidersLabel> {
        let configured_target_label = CONFIGURED_TARGET_CALCULATION
            .get()?
            .get_configured_target(self, target.target(), global_target_platform)
            .await?;
        Ok(ConfiguredProvidersLabel::new(
            configured_target_label,
            target.name().clone(),
        ))
    }

    async fn get_default_configured_target(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<ConfiguredTargetLabel> {
        CONFIGURED_TARGET_CALCULATION
            .get()?
            .get_configured_target(self, target, None)
            .await
    }
}
