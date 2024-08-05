/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_util::late_binding::LateBinding;
use dice::LinearRecomputeDiceComputations;

use crate::materialize::MaterializationContext;

#[async_trait]
pub trait ValidationImpl: Send + Sync + 'static {
    /// Validate a given configured target node and any transitive nodes.
    async fn validate_target_node_transitively<'a>(
        &self,
        ctx: &'a LinearRecomputeDiceComputations<'_>,
        materialization_context: &'a MaterializationContext,
        target_node: ConfiguredTargetNode,
    ) -> Result<(), buck2_error::Error>;
}

pub static VALIDATION_IMPL: LateBinding<&'static dyn ValidationImpl> =
    LateBinding::new("VALIDATION_IMPL");
