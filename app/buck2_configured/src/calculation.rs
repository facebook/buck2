/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
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
use dice::DiceComputations;
use dupe::Dupe;

use crate::configuration::ConfigurationCalculation;
use crate::execution::get_execution_platform_toolchain_dep;

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
                // FIXME(JakobDegen): This is busted. Callers of this function expect to need to
                // subsequently actually configure the target, and handle any possible
                // incompatibilities at that time. Doing this here prevents them from handling those
                // as they would for non-toolchain targets.
                //
                // FIXME(JakobDegen): Write a test for the above.
                .require_compatible()?
                .cfg();
                Ok(target.configure_with_exec(cfg, exec_cfg.cfg().dupe()))
            }
        }
    }
}
