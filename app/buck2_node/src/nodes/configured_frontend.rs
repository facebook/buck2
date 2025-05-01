/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

use crate::nodes::configured::ConfiguredTargetNode;

#[async_trait]
pub trait ConfiguredTargetNodeCalculationImpl: Send + Sync + 'static {
    /// Returns the ConfiguredTargetNode corresponding to a ConfiguredTargetLabel.
    async fn get_configured_target_node(
        &self,
        ctx: &mut DiceComputations<'_>,
        target: &ConfiguredTargetLabel,
        check_dependency_incompatibility: bool,
    ) -> buck2_error::Result<MaybeCompatible<ConfiguredTargetNode>>;
}

pub static CONFIGURED_TARGET_NODE_CALCULATION: LateBinding<
    &'static dyn ConfiguredTargetNodeCalculationImpl,
> = LateBinding::new("CONFIGURED_TARGET_NODE_CALCULATION");

#[async_trait]
pub trait ConfiguredTargetNodeCalculation {
    /// Returns the ConfiguredTargetNode corresponding to a ConfiguredTargetLabel.
    async fn get_configured_target_node(
        &mut self,
        target: &ConfiguredTargetLabel,
    ) -> buck2_error::Result<MaybeCompatible<ConfiguredTargetNode>>;

    /// Same as `get_configured_target_node` except it doesn't error/soft-error on
    /// configured target that is transitively incompatible. This should only be used
    /// to obtain any configured target node used as deps of other configured nodes,
    /// ex. recursively from `get_configured_target_node` function. All other use cases
    /// should use `get_configured_target_node` instead.
    async fn get_internal_configured_target_node(
        &mut self,
        target: &ConfiguredTargetLabel,
    ) -> buck2_error::Result<MaybeCompatible<ConfiguredTargetNode>>;
}

#[async_trait]
impl ConfiguredTargetNodeCalculation for DiceComputations<'_> {
    async fn get_configured_target_node(
        &mut self,
        target: &ConfiguredTargetLabel,
    ) -> buck2_error::Result<MaybeCompatible<ConfiguredTargetNode>> {
        CONFIGURED_TARGET_NODE_CALCULATION
            .get()?
            .get_configured_target_node(self, target, true)
            .await
    }

    async fn get_internal_configured_target_node(
        &mut self,
        target: &ConfiguredTargetLabel,
    ) -> buck2_error::Result<MaybeCompatible<ConfiguredTargetNode>> {
        CONFIGURED_TARGET_NODE_CALCULATION
            .get()?
            .get_configured_target_node(self, target, false)
            .await
    }
}
