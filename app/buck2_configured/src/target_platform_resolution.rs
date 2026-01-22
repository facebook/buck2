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

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_core::target::target_configured_target_label::TargetConfiguredTargetLabel;
use buck2_error::BuckErrorContext;
use buck2_node::cfg_constructor::CFG_CONSTRUCTOR_CALCULATION_IMPL;
use buck2_node::configuration::target_platform_detector::TargetPlatformDetector;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::RuleKind;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::super_package::SuperPackage;
use buck2_node::target_calculation::CONFIGURED_TARGET_CALCULATION;
use buck2_node::target_calculation::ConfiguredTargetCalculationImpl;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;

use crate::configuration::get_platform_configuration;
use crate::execution::get_execution_platform_toolchain_dep;

async fn get_target_platform_detector(
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<Arc<TargetPlatformDetector>> {
    // This requires a bit of computation so cache it on the graph.
    // TODO(cjhopman): Should we construct this (and similar buckconfig-derived objects) as part of the buck config itself?
    #[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative)]
    #[display("TargetPlatformDetectorKey")]
    struct TargetPlatformDetectorKey;

    #[async_trait]
    impl Key for TargetPlatformDetectorKey {
        type Value = buck2_error::Result<Arc<TargetPlatformDetector>>;
        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellation: &CancellationContext,
        ) -> Self::Value {
            // We get this off the root cell's config. It's not clear that that's the appropriate way to do it, but its the easiest to get working at FB.
            // TODO(cjhopman): Consider revisiting that approach.
            let resolver = ctx.get_cell_resolver().await?;
            let root_cell = resolver.root_cell();
            let cell_alias_resolver = ctx.get_cell_alias_resolver(root_cell).await?;

            Ok(Arc::new(
                match ctx
                    .get_legacy_config_property(
                        root_cell,
                        BuckconfigKeyRef {
                            section: "parser",
                            property: "target_platform_detector_spec",
                        },
                    )
                    .await?
                {
                    None => TargetPlatformDetector::empty(),
                    Some(spec) => TargetPlatformDetector::parse_spec(
                        &spec,
                        root_cell,
                        &resolver,
                        &cell_alias_resolver,
                    )?,
                },
            ))
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            match (x, y) {
                (Ok(x), Ok(y)) => x == y,
                _ => false,
            }
        }
    }

    ctx.compute(&TargetPlatformDetectorKey).await?
}

async fn get_default_platform(
    ctx: &mut DiceComputations<'_>,
    target: &TargetLabel,
) -> buck2_error::Result<ConfigurationData> {
    let detector = get_target_platform_detector(ctx).await?;
    if let Some(target) = detector.detect(target) {
        return get_platform_configuration(ctx, target)
            .await
            .map_err(buck2_error::Error::from);
    }
    // TODO(cjhopman): This needs to implement buck1's approach to determining target platform, it's currently missing the fallback to buckconfig parser.target_platform.
    Ok(ConfigurationData::unspecified())
}

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

        async fn get_platform_configuration_from_options(
            ctx: &mut DiceComputations<'_>,
            global_cfg_options: &GlobalCfgOptions,
            target: &TargetLabel,
            node: &TargetNode,
            super_package: &SuperPackage,
        ) -> buck2_error::Result<ConfigurationData> {
            let current_cfg = match global_cfg_options.target_platform.as_ref() {
                Some(global_target_platform) => {
                    get_platform_configuration(ctx, global_target_platform).await?
                }
                None => match node.get_default_target_platform() {
                    Some(target) => get_platform_configuration(ctx, target).await?,
                    None => get_default_platform(ctx, target).await?,
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
                    // configuring_exec_dep: false because this is configuring a regular target,
                    // not an execution dependency.
                    false,
                )
                .await
                .with_buck_error_context(|| format!("Resolving modifiers for target `{target}`"))
        }

        match node.rule_kind() {
            RuleKind::Configuration => Ok(target.configure(ConfigurationData::unbound())),
            RuleKind::Normal => Ok(target.configure(
                get_platform_configuration_from_options(
                    ctx,
                    global_cfg_options,
                    target,
                    &node,
                    &super_package,
                )
                .await?,
            )),
            RuleKind::Toolchain => {
                let cfg = get_platform_configuration_from_options(
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
