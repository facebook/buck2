/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

#[async_trait]
pub trait TransitionCalculation: Send + Sync + 'static {
    /// Apply transition function to configuration and cache the result.
    async fn apply_transition(
        &self,
        ctx: &DiceComputations,
        target_node: &TargetNode,
        conf: &ConfigurationData,
        transition_id: &TransitionId,
    ) -> anyhow::Result<Arc<TransitionApplied>>;
}

pub static TRANSITION_CALCULATION: LateBinding<&'static dyn TransitionCalculation> =
    LateBinding::new("TRANSITION_CALCULATION");
