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
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_core::target::target_configured_target_label::TargetConfiguredTargetLabel;
use buck2_node::cfg_constructor::CFG_CONSTRUCTOR_CALCULATION_IMPL;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::RuleKind;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::super_package::SuperPackage;
use buck2_node::target_calculation::ConfiguredTargetCalculationImpl;
use buck2_node::target_calculation::CONFIGURED_TARGET_CALCULATION;
use buck2_util::cycle_detector::CycleDescriptor;
use derive_more::Display;
use dice::DiceComputations;
use dice::DynKey;
use dupe::Dupe;
use gazebo::prelude::*;

use crate::configuration::ConfigurationCalculation;
use crate::execution::get_execution_platform_toolchain_dep;
use crate::execution::ExecutionPlatformResolutionKey;
use crate::execution::ToolchainExecutionPlatformCompatibilityKey;
use crate::nodes::ConfiguredTargetNodeKey;

struct ConfiguredTargetCalculationInstance;

pub(crate) fn init_configured_target_calculation() {
    CONFIGURED_TARGET_CALCULATION.init(&ConfiguredTargetCalculationInstance);
}

#[async_trait]
impl ConfiguredTargetCalculationImpl for ConfiguredTargetCalculationInstance {
    async fn get_configured_target(
        &self,
        ctx: &mut DiceComputations<'_>,
        target: &TargetLabel,
        global_cfg_options: &GlobalCfgOptions,
    ) -> buck2_error::Result<ConfiguredTargetLabel> {
        let (node, super_package) = ctx.get_target_node_with_super_package(target).await?;

        async fn get_platform_configuration(
            ctx: &mut DiceComputations<'_>,
            global_cfg_options: &GlobalCfgOptions,
            target: &TargetLabel,
            node: &TargetNode,
            super_package: &SuperPackage,
        ) -> buck2_error::Result<ConfigurationData> {
            let current_cfg = match global_cfg_options.target_platform.as_ref() {
                Some(global_target_platform) => {
                    ctx.get_platform_configuration(global_target_platform)
                        .await?
                }
                None => match node.get_default_target_platform() {
                    Some(target) => ctx.get_platform_configuration(target).await?,
                    None => ctx.get_default_platform(target).await?,
                },
            };

            CFG_CONSTRUCTOR_CALCULATION_IMPL
                .get()?
                .eval_cfg_constructor(
                    ctx,
                    node.as_ref(),
                    super_package,
                    current_cfg,
                    &global_cfg_options.cli_modifiers,
                    node.rule_type(),
                )
                .await
        }

        match node.rule_kind() {
            RuleKind::Configuration => Ok(target.configure(ConfigurationData::unbound())),
            RuleKind::Normal => Ok(target.configure(
                get_platform_configuration(ctx, global_cfg_options, target, &node, &super_package)
                    .await?,
            )),
            RuleKind::Toolchain => {
                let cfg = get_platform_configuration(
                    ctx,
                    global_cfg_options,
                    target,
                    &node,
                    &super_package,
                )
                .await?;
                let exec_cfg = get_execution_platform_toolchain_dep(
                    ctx,
                    &TargetConfiguredTargetLabel::new_configure(target, cfg.dupe()),
                    node.as_ref(),
                )
                .await?
                .require_compatible()?
                .cfg();
                Ok(target.configure_with_exec(cfg, exec_cfg.cfg().dupe()))
            }
        }
    }
}

#[derive(Debug, buck2_error::Error, Clone, Dupe)]
#[buck2(input)]
#[error("{}", display_configured_graph_cycle_error(&.cycle[..]))]
pub struct ConfiguredGraphCycleError {
    cycle: Arc<Vec<ConfiguredGraphCycleKeys>>,
}

fn display_configured_graph_cycle_error(cycle: &[ConfiguredGraphCycleKeys]) -> String {
    use std::fmt::Write;

    let mut s = String::new();
    writeln!(
        s,
        "Configured target cycle detected (`->` means \"depends on\"):"
    )
    .unwrap();
    for p in cycle.iter() {
        writeln!(s, "  {} ->", p).unwrap();
    }
    // point back at the first item in the cycle.
    writeln!(s, "  {}", cycle.first().unwrap()).unwrap();
    s
}

// TODO(cjhopman): There's other keys that could be involved in a cycle in the configured graph and they should probably also be tracked
// here. Would be good to check on things like transitions, toolchains, configuration nodes. Still, this will currently catch most
// configured graph cycles.
#[derive(Debug, Display, Clone, Eq, PartialEq, Hash)]
#[allow(private_interfaces)]
pub enum ConfiguredGraphCycleKeys {
    #[display("{}", _0)]
    ConfiguredTargetNode(ConfiguredTargetNodeKey),
    #[display("{}", _0)]
    ToolchainExecutionPlatformCompatibility(ToolchainExecutionPlatformCompatibilityKey),
    #[display("{}", _0)]
    ExecutionPlatformResolution(ExecutionPlatformResolutionKey),
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
    fn to_key(key: &DynKey) -> Option<Self::Key> {
        if let Some(v) = key.downcast_ref::<ConfiguredTargetNodeKey>() {
            return Some(ConfiguredGraphCycleKeys::ConfiguredTargetNode(v.dupe()));
        }
        if let Some(v) = key.downcast_ref::<ExecutionPlatformResolutionKey>() {
            return Some(ConfiguredGraphCycleKeys::ExecutionPlatformResolution(
                v.dupe(),
            ));
        }
        if let Some(v) = key.downcast_ref::<ToolchainExecutionPlatformCompatibilityKey>() {
            return Some(
                ConfiguredGraphCycleKeys::ToolchainExecutionPlatformCompatibility(v.dupe()),
            );
        }

        None
    }
}
