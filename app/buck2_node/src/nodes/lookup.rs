/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_query::query::traversal::AsyncNodeLookup;
use dice::LinearRecomputeDiceComputations;

use crate::nodes::configured::ConfiguredTargetNode;
use crate::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use crate::nodes::frontend::TargetGraphCalculation;
use crate::nodes::unconfigured::TargetNode;

pub struct TargetNodeLookup<'c, 'd>(pub &'c LinearRecomputeDiceComputations<'d>);

#[async_trait]
impl AsyncNodeLookup<TargetNode> for TargetNodeLookup<'_, '_> {
    async fn get(&self, label: &TargetLabel) -> buck2_error::Result<TargetNode> {
        Ok(self.0.get().get_target_node(label).await?)
    }
}

pub struct ConfiguredTargetNodeLookup<'c, 'd>(pub &'c LinearRecomputeDiceComputations<'d>);

#[async_trait]
impl AsyncNodeLookup<ConfiguredTargetNode> for ConfiguredTargetNodeLookup<'_, '_> {
    async fn get(
        &self,
        label: &ConfiguredTargetLabel,
    ) -> buck2_error::Result<ConfiguredTargetNode> {
        Ok(self
            .0
            .get()
            .get_configured_target_node(label)
            .await?
            .require_compatible()?)
    }
}
