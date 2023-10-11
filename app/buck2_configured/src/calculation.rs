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
use buck2_common::dice::cycles::CycleAdapterDescriptor;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_error::shared_result::SharedResult;
use buck2_node::cfg_constructor::CFG_CONSTRUCTOR_CALCULATION_IMPL;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::RuleKind;
use buck2_node::target_calculation::ConfiguredTargetCalculationImpl;
use buck2_node::target_calculation::CONFIGURED_TARGET_CALCULATION;
use buck2_util::cycle_detector::CycleDescriptor;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use gazebo::prelude::*;
use thiserror::Error;

use crate::configuration::calculation::ConfigurationCalculation;
use crate::nodes::calculation::get_execution_platform_toolchain_dep;
use crate::nodes::calculation::ConfiguredTargetNodeKey;

struct ConfiguredTargetCalculationInstance;

pub(crate) fn init_configured_target_calculation() {
    CONFIGURED_TARGET_CALCULATION.init(&ConfiguredTargetCalculationInstance);
}

#[async_trait]
impl ConfiguredTargetCalculationImpl for ConfiguredTargetCalculationInstance {
    async fn get_configured_target(
        &self,
        ctx: &DiceComputations,
        target: &TargetLabel,
        global_target_platform: Option<&TargetLabel>,
    ) -> anyhow::Result<ConfiguredTargetLabel> {
        let node = ctx.get_target_node(target).await?;

        let get_platform_configuration = async || -> SharedResult<ConfigurationData> {
            let current_cfg = match global_target_platform {
                Some(global_target_platform) => {
                    ctx.get_platform_configuration(global_target_platform)
                        .await?
                }
                None => match node.get_default_target_platform() {
                    Some(target) => ctx.get_platform_configuration(target).await?,
                    None => ctx.get_default_platform(target).await?,
                },
            };

            Ok(CFG_CONSTRUCTOR_CALCULATION_IMPL
                .get()?
                .eval_cfg_constructor(ctx, current_cfg)
                .await?)
        };

        match node.rule_kind() {
            RuleKind::Configuration => Ok(target.configure(ConfigurationData::unbound())),
            RuleKind::Normal => Ok(target.configure(get_platform_configuration().await?)),
            RuleKind::Toolchain => {
                let cfg = get_platform_configuration().await?;
                let exec_cfg =
                    get_execution_platform_toolchain_dep(ctx, &target.configure(cfg.dupe()), &node)
                        .await?
                        .require_compatible()?
                        .cfg();
                Ok(target.configure_with_exec(cfg, exec_cfg.cfg().dupe()))
            }
        }
    }
}

#[derive(Error, Debug, Clone, Dupe)]
pub struct ConfiguredGraphCycleError {
    cycle: Arc<Vec<ConfiguredGraphCycleKeys>>,
}

impl std::fmt::Display for ConfiguredGraphCycleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Configured target cycle detected (`->` means \"depends on\"):"
        )?;
        for p in self.cycle.iter() {
            writeln!(f, "  {} ->", p)?;
        }
        // point back at the first item in the cycle.
        writeln!(f, "  {}", self.cycle.first().unwrap())?;
        Ok(())
    }
}

// TODO(cjhopman): There's other keys that could be involved in a cycle in the configured graph and they should probably also be tracked
// here. Would be good to check on things like transitions, toolchains, configuration nodes. Still, this will currently catch most
// configured graph cycles.
#[derive(Debug, Display, Clone, Eq, PartialEq, Hash)]
pub enum ConfiguredGraphCycleKeys {
    #[display(fmt = "{}", _0)]
    ConfiguredTargetNode(ConfiguredTargetNodeKey),
}

#[derive(Debug)]
pub struct ConfiguredGraphCycleDescriptor;

impl CycleDescriptor for ConfiguredGraphCycleDescriptor {
    type Key = ConfiguredGraphCycleKeys;

    type Error = ConfiguredGraphCycleError;

    fn cycle_error(cycle: Vec<&Self::Key>) -> Self::Error {
        ConfiguredGraphCycleError {
            cycle: Arc::new(cycle.cloned()),
        }
    }
}

impl CycleAdapterDescriptor for ConfiguredGraphCycleDescriptor {
    fn to_key(key: &dyn std::any::Any) -> Option<Self::Key> {
        if let Some(v) = key.downcast_ref::<ConfiguredTargetNodeKey>() {
            return Some(ConfiguredGraphCycleKeys::ConfiguredTargetNode(v.dupe()));
        }
        None
    }
}
