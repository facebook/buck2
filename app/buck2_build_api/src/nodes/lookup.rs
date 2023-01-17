/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::traversal::AsyncNodeLookup;
use dice::DiceComputations;

use crate::nodes::calculation::NodeCalculation;

pub struct TargetNodeLookup<'c>(pub &'c DiceComputations);

#[async_trait]
impl<'c> AsyncNodeLookup<TargetNode> for TargetNodeLookup<'c> {
    async fn get(&self, label: &TargetLabel) -> anyhow::Result<TargetNode> {
        Ok(self.0.get_target_node(label).await?)
    }
}

pub struct ConfiguredTargetNodeLookup<'c>(pub &'c DiceComputations);

#[async_trait]
impl<'c> AsyncNodeLookup<ConfiguredTargetNode> for ConfiguredTargetNodeLookup<'c> {
    async fn get(&self, label: &ConfiguredTargetLabel) -> anyhow::Result<ConfiguredTargetNode> {
        self.0
            .get_configured_target_node(label)
            .await?
            .require_compatible()
    }
}
