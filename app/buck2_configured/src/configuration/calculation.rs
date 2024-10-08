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

use itertools::Itertools;
use allocative::Allocative;
use async_trait::async_trait;use buck2_build_api::interpreter::rule_defs::provider::builtin::platform_info::FrozenPlatformInfo;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::configs::parse_config_section_and_key;
use buck2_error::AnyhowContextForError;
use buck2_core::cells::name::CellName;
use futures::FutureExt;
use starlark_map::unordered_map::UnorderedMap;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::target::label::label::TargetLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::execution_types::execution::ExecutionPlatform;
use buck2_core::execution_types::execution::ExecutionPlatformError;
use buck2_core::execution_types::execution::ExecutionPlatformIncompatibleReason;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::execution_types::execution_platforms::ExecutionPlatformFallback;
use buck2_core::execution_types::execution_platforms::ExecutionPlatforms;
use buck2_core::execution_types::execution_platforms::ExecutionPlatformsData;
use buck2_node::configuration::resolved::ConfigurationNode;
use buck2_node::configuration::resolved::ResolvedConfigurationSettings;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use buck2_node::configuration::resolved::ResolvedConfiguration;
use buck2_node::configuration::target_platform_detector::TargetPlatformDetector;
use buck2_node::configuration::toolchain_constraints::ToolchainConstraints;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use buck2_futures::cancellation::CancellationContext;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::interpreter::rule_defs::provider::builtin::configuration_info::FrozenConfigurationInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::execution_platform_registration_info::FrozenExecutionPlatformRegistrationInfo;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::target::target_configured_target_label::TargetConfiguredTargetLabel;
use buck2_node::execution::{GetExecutionPlatformsImpl, GET_EXECUTION_PLATFORMS, GetExecutionPlatforms, EXECUTION_PLATFORMS_BUCKCONFIG};
use crate::nodes::calculation::ExecutionPlatformConstraints;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
pub enum ConfigurationError {
    #[error("Expected a ConfigurationInfo provider from `{0}`.")]
    MissingConfigurationInfoProvider(TargetLabel),
    #[error("Expected `{0}` to be a `platform()` target, but it had no `PlatformInfo` provider.")]
    MissingPlatformInfo(TargetLabel),
    #[error(
        "Expected `{0}` to provide a `ExecutionPlatformRegistrationInfo` as it's configured as the `build.execution_platforms` value."
    )]
    MissingExecutionPlatformRegistrationInfo(TargetLabel),
    #[error(
        "Platform target `{0}` evaluation returned `ProviderInfo` label `{1}` which resolved to an unequal configuration"
    )]
    PlatformEvalUnequalConfiguration(TargetLabel, TargetLabel),
}

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

/// Returns the configured [ExecutionPlatforms] or None if `build.execution_platforms` is not configured.
async fn compute_execution_platforms(
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<Option<ExecutionPlatforms>> {
    let cells = ctx.get_cell_resolver().await?;
    let cell_alias_resolver = ctx.get_cell_alias_resolver(cells.root_cell()).await?;

    let execution_platforms_target = ctx
        .get_legacy_config_property(cells.root_cell(), EXECUTION_PLATFORMS_BUCKCONFIG)
        .await?;

    let execution_platforms_target = match execution_platforms_target {
        Some(v) => TargetLabel::parse(&v, cells.root_cell(), &cells, &cell_alias_resolver)?,
        None => {
            return Ok(None);
        }
    };

    let providers = &ctx
        // Execution platform won't be supplied as a subtarget
        .get_configuration_analysis_result(&ProvidersLabel::default_for(
            execution_platforms_target.dupe(),
        ))
        .await?;

    let result = providers
        .provider_collection()
        .builtin_provider::<FrozenExecutionPlatformRegistrationInfo>()
        .ok_or_else(|| {
            anyhow::anyhow!(
                ConfigurationError::MissingExecutionPlatformRegistrationInfo(
                    execution_platforms_target.dupe()
                )
            )
        })?;

    let mut platforms = Vec::new();
    for platform in result.platforms()? {
        platforms.push(platform.to_execution_platform()?);
    }
    Ok(Some(Arc::new(ExecutionPlatformsData::new(
        execution_platforms_target,
        platforms,
        result.fallback()?,
    ))))
}

/// Check if a particular execution platform is compatible with the constraints or not.
/// Return either Ok/Ok if it is, or a reason if not.
async fn check_execution_platform(
    ctx: &mut DiceComputations<'_>,
    target_node_cell: CellName,
    exec_compatible_with: &[ConfigurationSettingKey],
    exec_deps: &[TargetLabel],
    exec_platform: &ExecutionPlatform,
    toolchain_allows: &[ToolchainConstraints],
) -> anyhow::Result<Result<(), ExecutionPlatformIncompatibleReason>> {
    let resolved_platform_configuration = ctx
        .get_resolved_configuration(
            exec_platform.cfg(),
            target_node_cell,
            toolchain_allows
                .iter()
                .flat_map(ToolchainConstraints::exec_compatible_with)
                .chain(exec_compatible_with),
        )
        .await?;

    // Then check if the platform satisfies compatible_with
    for constraint in toolchain_allows
        .iter()
        .flat_map(ToolchainConstraints::exec_compatible_with)
        .chain(exec_compatible_with)
    {
        if resolved_platform_configuration
            .settings()
            .setting_matches(constraint)
            .is_none()
        {
            return Ok(Err(
                ExecutionPlatformIncompatibleReason::ConstraintNotSatisfied(constraint.dupe().0),
            ));
        }
    }

    // Then check that all exec_deps are compatible with the platform. We collect errors separately,
    // so that we do not report an error if we would later find an incompatibility
    let mut errs = Vec::new();
    for dep in toolchain_allows
        .iter()
        .flat_map(ToolchainConstraints::exec_deps)
        .chain(exec_deps)
    {
        match ctx
            .get_internal_configured_target_node(
                &dep.configure_pair_no_exec(exec_platform.cfg_pair_no_exec().dupe()),
            )
            .await
        {
            Ok(MaybeCompatible::Compatible(_)) => (),
            Ok(MaybeCompatible::Incompatible(reason)) => {
                return Ok(Err(
                    ExecutionPlatformIncompatibleReason::ExecutionDependencyIncompatible(
                        reason.dupe(),
                    ),
                ));
            }
            Err(e) => errs.push(e.context(format!(
                "Error checking compatibility of `{}` with `{}`",
                dep,
                exec_platform.cfg()
            ))),
        };
    }
    if let Some(e) = errs.pop() {
        return Err(e);
    }

    Ok(Ok(()))
}

async fn get_execution_platforms_enabled(
    ctx: &mut DiceComputations<'_>,
) -> anyhow::Result<ExecutionPlatforms> {
    ctx.get_execution_platforms()
        .await?
        .context("Execution platforms are not enabled")
}

async fn resolve_execution_platform_from_constraints(
    ctx: &mut DiceComputations<'_>,
    target_node_cell: CellName,
    exec_compatible_with: &[ConfigurationSettingKey],
    exec_deps: &[TargetLabel],
    toolchain_allows: &[ToolchainConstraints],
) -> buck2_error::Result<ExecutionPlatformResolution> {
    let mut skipped = Vec::new();
    let execution_platforms = get_execution_platforms_enabled(ctx).await?;
    for exec_platform in execution_platforms.candidates() {
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

    match execution_platforms.fallback() {
        ExecutionPlatformFallback::UseUnspecifiedExec => {
            Ok(ExecutionPlatformResolution::new(None, skipped))
        }
        ExecutionPlatformFallback::Error => Err(anyhow::anyhow!(
            ExecutionPlatformError::NoCompatiblePlatform(Arc::new(skipped))
        )
        .into()),
        ExecutionPlatformFallback::Platform(platform) => Ok(ExecutionPlatformResolution::new(
            Some(platform.dupe()),
            skipped,
        )),
    }
}

async fn configuration_matches(
    ctx: &mut DiceComputations<'_>,
    cfg: &ConfigurationData,
    target_node_cell: CellName,
    constraints_and_configs: &ConfigSettingData,
) -> buck2_error::Result<bool> {
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
                BuckconfigKeyRef {
                    section: &config_section_and_key.section,
                    property: &config_section_and_key.key,
                },
            )
            .await?;
        match v {
            Some(v) if &*v == config_value => {}
            _ => return Ok(false),
        }
    }

    Ok(true)
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display("ExecutionPlatforms")]
pub struct ExecutionPlatformsKey;

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display("ConfigurationNode({}, {})", cfg_target, target_cfg)]
struct ConfigurationNodeKey {
    target_cfg: ConfigurationData,
    target_cell: CellName,
    cfg_target: ConfigurationSettingKey,
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(
    "ResolvedConfigurationKey(target_cfg: {}, cell: {}, configuration_deps size {})",
    target_cfg,
    target_cell,
    configuration_deps.len()
)]
struct ResolvedConfigurationKey {
    target_cfg: ConfigurationData,
    target_cell: CellName,
    configuration_deps: Vec<ConfigurationSettingKey>,
}

#[async_trait]
pub trait ConfigurationCalculation {
    async fn get_default_platform(
        &mut self,
        target: &TargetLabel,
    ) -> buck2_error::Result<ConfigurationData>;

    async fn get_platform_configuration(
        &mut self,
        target: &TargetLabel,
    ) -> anyhow::Result<ConfigurationData>;

    async fn get_resolved_configuration<
        'a,
        T: IntoIterator<Item = &'a ConfigurationSettingKey> + Send,
    >(
        &mut self,
        target_cfg: &ConfigurationData,
        target_node_cell: CellName,
        configuration_deps: T,
    ) -> buck2_error::Result<ResolvedConfiguration>;

    async fn get_configuration_node(
        &mut self,
        target_cfg: &ConfigurationData,
        target_cell: CellName,
        cfg_target: &ConfigurationSettingKey,
    ) -> buck2_error::Result<ConfigurationNode>;

    /// Gets the compatible execution platforms for a give list of compatible_with constraints and execution deps.
    ///
    /// We do this as a sort of monolithic computation (rather than checking things one-by-one or separating
    /// compatible_with and exec deps) because we expect those values to be common across many nodes (for example,
    /// all c++ targets targeting a specific platform are likely to share compatible_with and exec_deps except
    /// for some rare exceptions). By having a monolithic key like `(Vec<TargetLabel>, Vec<TargetLabel>)` allows all
    /// those nodes to just have a single dice dep. This approach has the downside that it is less incremental, but
    /// we expect these things to change rarely.
    async fn resolve_execution_platform_from_constraints(
        &mut self,
        target_node_cell: CellName,
        exec_compatible_with: Arc<[ConfigurationSettingKey]>,
        exec_deps: Arc<[TargetLabel]>,
        toolchain_allows: Arc<[ToolchainConstraints]>,
    ) -> buck2_error::Result<ExecutionPlatformResolution>;
}

async fn compute_platform_configuration_no_label_check(
    ctx: &mut DiceComputations<'_>,
    target: &TargetLabel,
) -> anyhow::Result<ConfigurationData> {
    let platform_info = (&ctx
        // TODO(T198223238): Not supporting platforms being supplied via subtargets for now
        .get_configuration_analysis_result(&ProvidersLabel::default_for(target.dupe()))
        .await?)
        .provider_collection()
        .builtin_provider::<FrozenPlatformInfo>()
        .ok_or_else(|| ConfigurationError::MissingPlatformInfo(target.dupe()))?;
    platform_info.to_configuration()
}

/// Basically, evaluate `platform()` rule.
async fn compute_platform_configuration(
    ctx: &mut DiceComputations<'_>,
    target: &TargetLabel,
) -> anyhow::Result<ConfigurationData> {
    let configuration_data = compute_platform_configuration_no_label_check(ctx, target).await?;

    let cell_resolver = ctx.get_cell_resolver().await?;
    let cell_alias_resolver = ctx
        .get_cell_alias_resolver(cell_resolver.root_cell())
        .await?;
    let parsed_target = TargetLabel::parse(
        configuration_data.label()?,
        cell_resolver.root_cell(),
        &cell_resolver,
        &cell_alias_resolver,
    )
    .context("`PlatformInfo` label for `platform()` rule should be a valid target label")?;

    if target != &parsed_target {
        // `target` may be an `alias` target. In this case we evaluate the label
        // from the configuration and check it resolves to the same configuration.

        let cfg_again = compute_platform_configuration_no_label_check(
            ctx,
            &parsed_target,
        )
        .await
        .context(
            "Checking whether label of returned `PlatformInfo` resolves to the same configuration",
        )?;
        if cfg_again != configuration_data {
            return Err(ConfigurationError::PlatformEvalUnequalConfiguration(
                target.dupe(),
                parsed_target,
            )
            .into());
        }
    }

    Ok(configuration_data)
}

#[async_trait]
impl Key for ResolvedConfigurationKey {
    type Value = buck2_error::Result<ResolvedConfiguration>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let config_nodes = ctx
            .compute_join(self.configuration_deps.iter(), |ctx, d| {
                async move {
                    (
                        d.dupe(),
                        ctx.get_configuration_node(&self.target_cfg, self.target_cell, d)
                            .await,
                    )
                }
                .boxed()
            })
            .await;

        let mut resolved_settings = UnorderedMap::with_capacity(config_nodes.len());
        for (label, node) in config_nodes {
            let node = node?;
            resolved_settings.insert(label, node);
        }
        let resolved_settings = ResolvedConfigurationSettings::new(resolved_settings);
        Ok(ResolvedConfiguration::new(
            ConfigurationNoExec::new(self.target_cfg.dupe()),
            resolved_settings,
        ))
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }
}

#[async_trait]
impl Key for ConfigurationNodeKey {
    type Value = buck2_error::Result<ConfigurationNode>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let providers = ctx
            // TODO(T198210718)
            .get_configuration_analysis_result(&ProvidersLabel::default_for(
                self.cfg_target.0.dupe(),
            ))
            .await?;

        // capture the result so the temporaries get dropped before providers
        let result = match providers
            .provider_collection()
            .builtin_provider::<FrozenConfigurationInfo>()
        {
            Some(configuration_info) => configuration_info,
            None => {
                return Err::<_, buck2_error::Error>(
                    ConfigurationError::MissingConfigurationInfoProvider(self.cfg_target.dupe().0)
                        .into(),
                );
            }
        }
        .to_config_setting_data();

        let matches =
            configuration_matches(ctx, &self.target_cfg, self.target_cell, &result).await?;

        Ok(ConfigurationNode::new(Some(result).filter(|_| matches)))
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[async_trait]
impl ConfigurationCalculation for DiceComputations<'_> {
    async fn get_platform_configuration(
        &mut self,
        target: &TargetLabel,
    ) -> anyhow::Result<ConfigurationData> {
        #[derive(derive_more::Display, Debug, Eq, Hash, PartialEq, Clone, Allocative)]
        struct PlatformConfigurationKey(TargetLabel);

        #[async_trait]
        impl Key for PlatformConfigurationKey {
            type Value = buck2_error::Result<ConfigurationData>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                compute_platform_configuration(ctx, &self.0)
                    .await
                    .map_err(buck2_error::Error::from)
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        self.compute(&PlatformConfigurationKey(target.dupe()))
            .await?
            .map_err(anyhow::Error::from)
    }

    async fn get_default_platform(
        &mut self,
        target: &TargetLabel,
    ) -> buck2_error::Result<ConfigurationData> {
        let detector = get_target_platform_detector(self).await?;
        if let Some(target) = detector.detect(target) {
            return self
                .get_platform_configuration(target)
                .await
                .map_err(buck2_error::Error::from);
        }
        // TODO(cjhopman): This needs to implement buck1's approach to determining target platform, it's currently missing the fallback to buckconfig parser.target_platform.
        Ok(ConfigurationData::unspecified())
    }

    async fn get_resolved_configuration<
        'a,
        T: IntoIterator<Item = &'a ConfigurationSettingKey> + Send,
    >(
        &mut self,
        target_cfg: &ConfigurationData,
        target_cell: CellName,
        configuration_deps: T,
    ) -> buck2_error::Result<ResolvedConfiguration> {
        let configuration_deps: Vec<ConfigurationSettingKey> =
            configuration_deps.into_iter().map(|t| t.dupe()).collect();
        self.compute(&ResolvedConfigurationKey {
            target_cfg: target_cfg.dupe(),
            target_cell,
            configuration_deps,
        })
        .await?
    }

    async fn get_configuration_node(
        &mut self,
        target_cfg: &ConfigurationData,
        target_cell: CellName,
        cfg_target: &ConfigurationSettingKey,
    ) -> buck2_error::Result<ConfigurationNode> {
        self.compute(&ConfigurationNodeKey {
            target_cfg: target_cfg.dupe(),
            target_cell,
            cfg_target: cfg_target.dupe(),
        })
        .await?
        .with_context(|| {
            format!(
                "Error getting configuration node of `{}` within the `{}` configuration",
                cfg_target, target_cfg,
            )
        })
        .map_err(buck2_error::Error::from)
    }

    async fn resolve_execution_platform_from_constraints(
        &mut self,
        target_node_cell: CellName,
        exec_compatible_with: Arc<[ConfigurationSettingKey]>,
        exec_deps: Arc<[TargetLabel]>,
        toolchain_allows: Arc<[ToolchainConstraints]>,
    ) -> buck2_error::Result<ExecutionPlatformResolution> {
        self.compute(&ExecutionPlatformResolutionKey {
            target_node_cell,
            exec_compatible_with,
            exec_deps,
            toolchain_allows,
        })
        .await?
    }
}

#[derive(Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative)]
pub struct ExecutionPlatformResolutionKey {
    target_node_cell: CellName,
    exec_compatible_with: Arc<[ConfigurationSettingKey]>,
    exec_deps: Arc<[TargetLabel]>,
    toolchain_allows: Arc<[ToolchainConstraints]>,
}

impl Display for ExecutionPlatformResolutionKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Resolving execution platform: cell:{}",
            self.target_node_cell
        )?;

        if !self.exec_compatible_with.is_empty() {
            write!(
                f,
                ", exec_compatible_with=[{}]",
                self.exec_compatible_with.iter().join(", ")
            )?
        }

        if !self.exec_deps.is_empty() {
            write!(f, ", exec_deps=[{}]", self.exec_deps.iter().join(", "))?
        }

        let mut iter = self
            .toolchain_allows
            .iter()
            .flat_map(|v| v.exec_compatible_with())
            .peekable();
        if iter.peek().is_some() {
            write!(f, ", toolchain_exec_compatible_with=[{}]", iter.join(", "))?
        }

        let mut iter = self
            .toolchain_allows
            .iter()
            .flat_map(|v| v.exec_deps())
            .peekable();
        if iter.peek().is_some() {
            write!(f, ", toolchain_exec_deps=[{}]", iter.join(", "))?
        }

        Ok(())
    }
}

#[async_trait]
impl Key for ExecutionPlatformResolutionKey {
    type Value = buck2_error::Result<ExecutionPlatformResolution>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        resolve_execution_platform_from_constraints(
            ctx,
            self.target_node_cell,
            &self.exec_compatible_with,
            &self.exec_deps,
            &self.toolchain_allows,
        )
        .await
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

struct GetExecutionPlatformsInstance;

#[async_trait]
impl Key for ExecutionPlatformsKey {
    type Value = buck2_error::Result<Option<ExecutionPlatforms>>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        compute_execution_platforms(ctx).await
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        // TODO(cjhopman) should these be comparable for caching
        false
    }
}

#[async_trait]
impl GetExecutionPlatformsImpl for GetExecutionPlatformsInstance {
    async fn get_execution_platforms_impl(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Option<ExecutionPlatforms>> {
        ctx.compute(&ExecutionPlatformsKey).await?
    }

    async fn execution_platform_resolution_one_for_cell(
        &self,
        dice: &mut DiceComputations<'_>,
        exec_deps: Arc<[TargetLabel]>,
        toolchain_deps: Arc<[TargetConfiguredTargetLabel]>,
        exec_compatible_with: Arc<[ConfigurationSettingKey]>,
        cell: CellName,
    ) -> buck2_error::Result<ExecutionPlatformResolution> {
        ExecutionPlatformConstraints::new_constraints(
            exec_deps,
            toolchain_deps,
            exec_compatible_with,
        )
        .one_for_cell(dice, cell)
        .await
    }
}

pub(crate) fn init_get_execution_platforms() {
    GET_EXECUTION_PLATFORMS.init(&GetExecutionPlatformsInstance);
}
