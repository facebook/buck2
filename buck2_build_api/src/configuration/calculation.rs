/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::parse_config_section_and_key;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::cells::CellName;
use buck2_core::configuration::Configuration;
use buck2_core::configuration::ConfigurationData;
use buck2_core::pattern::ParsedPattern;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_node::compatibility::IncompatiblePlatformReason;
use buck2_node::compatibility::MaybeCompatible;
use buck2_node::configuration::execution::ExecutionPlatform;
use buck2_node::configuration::execution::ExecutionPlatformIncompatibleReason;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use buck2_node::configuration::resolved::ConfigurationNode;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use buck2_node::configuration::target_platform_detector::TargetPlatformDetector;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use gazebo::prelude::*;
use indexmap::IndexMap;
use indexmap::IndexSet;
use starlark::collections::SmallMap;
use thiserror::Error;

use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::configuration::ConfigurationCalculation;
use crate::configuration::ExecutionPlatforms;
use crate::configuration::ResolvedConfiguration;
use crate::interpreter::rule_defs::provider::builtin::configuration_info::FrozenConfigurationInfo;
use crate::interpreter::rule_defs::provider::builtin::execution_platform_registration_info::ExecutionPlatformRegistrationInfo;
use crate::interpreter::rule_defs::provider::builtin::platform_info::PlatformInfo;
use crate::nodes::calculation::NodeCalculation;

#[derive(Debug, Error)]
pub enum ConfigurationError {
    #[error("Expected a ConfigurationInfo provider from `{0}`.")]
    MissingConfigurationInfoProvider(TargetLabel),
    #[error("Expected `{0}` to be a `platform()` target, but it had no `PlatformInfo` provider.")]
    MissingPlatformInfo(TargetLabel),
    #[error(
        "Expected `{0}` to provide a `ExecutionPlatformRegistrationInfo` as it's configured as the `build.execution_platforms` value."
    )]
    MissingExecutionPlatformRegistrationInfo(TargetLabel),
}

async fn get_target_platform_detector(
    ctx: &DiceComputations,
) -> SharedResult<Arc<TargetPlatformDetector>> {
    // This requires a bit of computation so cache it on the graph.
    // TODO(cjhopman): Should we construct this (and similar buckconfig-derived objects) as part of the buck config itself?
    #[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq)]
    #[display(fmt = "TargetPlatformDetectorKey")]
    struct TargetPlatformDetectorKey;

    #[async_trait]
    impl Key for TargetPlatformDetectorKey {
        type Value = SharedResult<Arc<TargetPlatformDetector>>;
        async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
            // We get this off the root cell's config. It's not clear that that's the appropriate way to do it, but its the easiest to get working at FB.
            // TODO(cjhopman): Consider revisiting that approach.
            let resolver = ctx.get_cell_resolver().await?;
            let root_cell = resolver.root_cell();
            let cell_alias_resolver = resolver.get(root_cell).unwrap().cell_alias_resolver();

            Ok(Arc::new(
                match ctx
                    .get_legacy_config_property(
                        root_cell,
                        "parser",
                        "target_platform_detector_spec",
                    )
                    .await?
                {
                    None => TargetPlatformDetector::empty(),
                    Some(spec) => TargetPlatformDetector::parse_spec(cell_alias_resolver, &*spec)?,
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

/// Returns the configured [ExecutionPlatforms] or None if `build.execution_platforms` is not configured.
async fn get_execution_platforms(
    ctx: &DiceComputations,
) -> SharedResult<Option<ExecutionPlatforms>> {
    let cells = ctx.get_cell_resolver().await?;

    let execution_platforms_target = ctx
        .get_legacy_config_property(cells.root_cell(), "build", "execution_platforms")
        .await?;

    let execution_platforms_target = match execution_platforms_target {
        Some(v) => {
            let root_cell = cells.root_cell_instance();
            ParsedPattern::parse_precise(root_cell.cell_alias_resolver(), &*v)?.as_literal(&*v)?
        }
        None => {
            return Ok(None);
        }
    };

    let execution_platforms_target =
        TargetLabel::new(execution_platforms_target.0, execution_platforms_target.1);

    let analysis_result = ctx
        .get_configuration_analysis_result(&execution_platforms_target)
        .await?;
    let providers = analysis_result.providers();

    let result = ExecutionPlatformRegistrationInfo::from_providers(providers.provider_collection())
        .ok_or_else(|| {
            anyhow::anyhow!(
                ConfigurationError::MissingExecutionPlatformRegistrationInfo(
                    execution_platforms_target
                )
            )
        })?;

    let mut platforms = Vec::new();
    for platform in result.platforms()? {
        platforms.push(platform.to_execution_platform()?);
    }
    Ok(Some(Arc::new(platforms)))
}

/// The constraint introduced on execution platform resolution by
/// a toolchain rule (reached via a toolchain_dep).
#[derive(Dupe, Clone, PartialEq, Eq)]
pub struct ToolchainConstraints {
    // We know the set of execution platforms is fixed throughout the build,
    // so we can record just the ones we are incompatible with,
    // and assume all others we _are_ compatible with.
    incompatible: Arc<SmallMap<ExecutionPlatform, Arc<IncompatiblePlatformReason>>>,
}

impl ToolchainConstraints {
    fn new(incompatible: SmallMap<ExecutionPlatform, Arc<IncompatiblePlatformReason>>) -> Self {
        Self {
            incompatible: Arc::new(incompatible),
        }
    }

    fn allows(
        &self,
        exec_platform: &ExecutionPlatform,
    ) -> Result<(), Arc<IncompatiblePlatformReason>> {
        match self.incompatible.get(exec_platform) {
            None => Ok(()),
            Some(e) => Err(e.dupe()),
        }
    }
}

/// Check if a particular execution platform is compatible with the constraints or not.
/// Return either Ok/Ok if it is, or a reason if not.
async fn check_execution_platform(
    ctx: &DiceComputations,
    target_node_cell: &CellName,
    exec_compatible_with: &[TargetLabel],
    exec_deps: &IndexSet<TargetLabel>,
    exec_platform: &ExecutionPlatform,
    toolchain_allows: &[ToolchainConstraints],
) -> anyhow::Result<Result<(), ExecutionPlatformIncompatibleReason>> {
    // First check if the platform satisfies the toolchain requirements
    for allowed in toolchain_allows {
        if let Err(e) = allowed.allows(exec_platform) {
            return Ok(Err(
                ExecutionPlatformIncompatibleReason::ExecutionDependencyIncompatible(e),
            ));
        }
    }

    let resolved_platform_configuration = ctx
        .get_resolved_configuration(
            &exec_platform.cfg(),
            target_node_cell,
            exec_compatible_with.iter(),
        )
        .await?;

    // Then check if the platform satisfies compatible_with
    for constraint in exec_compatible_with {
        if resolved_platform_configuration
            .matches(constraint)
            .is_none()
        {
            return Ok(Err(
                ExecutionPlatformIncompatibleReason::ConstraintNotSatisfied(constraint.dupe()),
            ));
        }
    }

    // Then check that all exec_deps are compatible with the platform
    for dep in exec_deps {
        let dep_node = ctx
            .get_configured_target_node(&dep.configure(exec_platform.cfg().dupe()))
            .await?;
        if let MaybeCompatible::Incompatible(reason) = dep_node {
            return Ok(Err(
                ExecutionPlatformIncompatibleReason::ExecutionDependencyIncompatible(reason.dupe()),
            ));
        }
    }

    Ok(Ok(()))
}

async fn get_execution_platforms_non_empty(
    ctx: &DiceComputations,
) -> anyhow::Result<Arc<Vec<ExecutionPlatform>>> {
    match ctx.get_execution_platforms().await? {
        // The caller should've verified that some execution platforms are configured.
        None => unreachable!(
            "execution platform resolution should only be invoked when execution platforms are configured"
        ),
        Some(v) => Ok(v),
    }
}

async fn resolve_toolchain_constraints_from_constraints(
    ctx: &DiceComputations,
    target: &ConfiguredTargetLabel,
    exec_compatible_with: &[TargetLabel],
    exec_deps: &IndexSet<TargetLabel>,
    toolchain_allows: &[ToolchainConstraints],
) -> SharedResult<ToolchainConstraints> {
    let mut incompatible = SmallMap::new();
    for exec_platform in get_execution_platforms_non_empty(ctx).await?.iter() {
        if let Err(e) = check_execution_platform(
            ctx,
            target.pkg().cell_name(),
            exec_compatible_with,
            exec_deps,
            exec_platform,
            toolchain_allows,
        )
        .await?
        {
            incompatible.insert(
                exec_platform.dupe(),
                Arc::new(e.into_incompatible_platform_reason(target.dupe())),
            );
        }
    }
    Ok(ToolchainConstraints::new(incompatible))
}

async fn resolve_execution_platform_from_constraints(
    ctx: &DiceComputations,
    target_node_cell: &CellName,
    exec_compatible_with: &[TargetLabel],
    exec_deps: &IndexSet<TargetLabel>,
    toolchain_allows: &[ToolchainConstraints],
) -> SharedResult<ExecutionPlatformResolution> {
    let mut skipped = Vec::new();
    for exec_platform in get_execution_platforms_non_empty(ctx).await?.iter() {
        match check_execution_platform(
            ctx,
            target_node_cell,
            exec_compatible_with,
            exec_deps,
            exec_platform,
            toolchain_allows,
        )
        .await?
        {
            Ok(()) => {
                return Ok(ExecutionPlatformResolution::new(
                    Some(exec_platform.dupe()),
                    skipped,
                ));
            }
            Err(reason) => {
                skipped.push((exec_platform.id(), reason));
            }
        }
    }
    Ok(ExecutionPlatformResolution::new(None, skipped))
}

async fn configuration_matches(
    ctx: &DiceComputations,
    cfg: &Configuration,
    target_node_cell: &CellName,
    constraints_and_configs: &ConfigurationData,
) -> SharedResult<bool> {
    for (key, value) in &constraints_and_configs.constraints {
        match cfg.get_constraint_value(key)? {
            Some(v) if v == value => {}
            _ => return Ok(false),
        }
    }

    // Cell used for buckconfigs is set to cell of target that applies select to match Buck v1 behavior.
    // Eventually, we want this to be the cell of the platform instead.
    for (raw_section_and_key, config_value) in &constraints_and_configs.buckconfigs {
        let config_section_and_key = parse_config_section_and_key(raw_section_and_key, None)?;
        let v = ctx
            .get_legacy_config_property(
                target_node_cell,
                &config_section_and_key.section,
                &config_section_and_key.key,
            )
            .await?;
        match v {
            Some(v) if &*v == config_value => {}
            _ => return Ok(false),
        }
    }

    Ok(true)
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "ExecutionPlatforms")]
pub struct ExecutionPlatformsKey;

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "ConfigurationNode({}, {})", cfg_target, target_cfg)]
struct ConfigurationNodeKey {
    target_cfg: Configuration,
    target_cell: CellName,
    cfg_target: TargetLabel,
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "ResolvedConfigurationKey")]
struct ResolvedConfigurationKey {
    target_cfg: Configuration,
    target_cell: CellName,
    configuration_deps: Vec<TargetLabel>,
}

#[async_trait]
impl ConfigurationCalculation for DiceComputations {
    async fn get_platform_configuration(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<Configuration> {
        let result = self.get_configuration_analysis_result(target).await?;
        let platform_info = PlatformInfo::from_providers(result.providers().provider_collection())
            .ok_or_else(|| ConfigurationError::MissingPlatformInfo(target.dupe()))?;
        platform_info.to_configuration()
    }

    async fn get_default_platform(&self, target: &TargetLabel) -> SharedResult<Configuration> {
        let detector = get_target_platform_detector(self).await?;
        if let Some(target) = detector.detect(target) {
            return self.get_platform_configuration(target).await.shared_error();
        }
        // TODO(cjhopman): This needs to implement buck1's approach to determining target platform, it's currently missing the fallback to buckconfig parser.target_platform.
        Ok(Configuration::unspecified())
    }

    async fn get_resolved_configuration<'a, T: Iterator<Item = &'a TargetLabel> + Send>(
        &self,
        target_cfg: &Configuration,
        target_cell: &CellName,
        configuration_deps: T,
    ) -> SharedResult<ResolvedConfiguration> {
        #[async_trait]
        impl Key for ResolvedConfigurationKey {
            type Value = SharedResult<ResolvedConfiguration>;

            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let config_futures: Vec<_> = self.configuration_deps.map(|d| async move {
                    ctx.get_configuration_node(&self.target_cfg, &self.target_cell, d)
                        .await
                });
                let config_nodes = futures::future::join_all(config_futures).await;

                let mut resolved_settings = IndexMap::new();
                for node in config_nodes {
                    let node = node?;
                    resolved_settings.insert(ConfigurationSettingKey(node.label().dupe()), node);
                }
                Ok(ResolvedConfiguration::new(
                    self.target_cfg.dupe(),
                    resolved_settings,
                ))
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                false
            }
        }

        let configuration_deps: Vec<TargetLabel> = configuration_deps.map(|t| t.dupe()).collect();
        self.compute(&ResolvedConfigurationKey {
            target_cfg: target_cfg.dupe(),
            target_cell: target_cell.clone(),
            configuration_deps,
        })
        .await?
    }

    async fn get_configuration_node(
        &self,
        target_cfg: &Configuration,
        target_cell: &CellName,
        cfg_target: &TargetLabel,
    ) -> SharedResult<ConfigurationNode> {
        #[async_trait]
        impl Key for ConfigurationNodeKey {
            type Value = SharedResult<ConfigurationNode>;

            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let analysis_result = ctx
                    .get_configuration_analysis_result(&self.cfg_target)
                    .await?;
                let providers = analysis_result.providers();

                // capture the result so the temporaries get dropped before analysis_result
                let result = match FrozenConfigurationInfo::from_providers(
                    providers.provider_collection(),
                ) {
                    Some(configuration_info) => configuration_info,
                    None => {
                        return Err::<_, anyhow::Error>(
                            ConfigurationError::MissingConfigurationInfoProvider(
                                self.cfg_target.dupe(),
                            )
                            .into(),
                        )
                        .shared_error();
                    }
                }
                .to_configuration_data();

                let matches =
                    configuration_matches(ctx, &self.target_cfg, &self.target_cell, &result)
                        .await?;

                Ok(ConfigurationNode::new(
                    self.target_cfg.dupe(),
                    self.cfg_target.dupe(),
                    result,
                    matches,
                ))
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        self.compute(&ConfigurationNodeKey {
            target_cfg: target_cfg.dupe(),
            target_cell: target_cell.clone(),
            cfg_target: cfg_target.dupe(),
        })
        .await?
        .with_context(|| {
            format!(
                "when getting configuration node of `{}` within the `{}` configuration",
                cfg_target, target_cfg,
            )
        })
        .shared_error()
    }

    async fn get_execution_platforms(&self) -> SharedResult<Option<ExecutionPlatforms>> {
        #[async_trait]
        impl Key for ExecutionPlatformsKey {
            type Value = SharedResult<Option<ExecutionPlatforms>>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                get_execution_platforms(ctx).await
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                // TODO(cjhopman) should these be comparable for caching
                false
            }
        }

        self.compute(&ExecutionPlatformsKey).await?
    }

    async fn resolve_execution_platform_from_constraints(
        &self,
        target_node_cell: &CellName,
        exec_compatible_with: &[TargetLabel],
        exec_deps: &IndexSet<TargetLabel>,
        toolchain_allows: &[ToolchainConstraints],
    ) -> SharedResult<ExecutionPlatformResolution> {
        resolve_execution_platform_from_constraints(
            self,
            target_node_cell,
            exec_compatible_with,
            exec_deps,
            toolchain_allows,
        )
        .await
    }

    async fn resolve_toolchain_constraints_from_constraints(
        &self,
        target: &ConfiguredTargetLabel,
        exec_compatible_with: &[TargetLabel],
        exec_deps: &IndexSet<TargetLabel>,
        toolchain_allows: &[ToolchainConstraints],
    ) -> SharedResult<ToolchainConstraints> {
        resolve_toolchain_constraints_from_constraints(
            self,
            target,
            exec_compatible_with,
            exec_deps,
            toolchain_allows,
        )
        .await
    }
}
