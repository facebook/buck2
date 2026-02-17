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
use buck2_build_api::actions::execute::dice_data::HasFallbackExecutorConfig;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::interpreter::rule_defs::provider::builtin::constraint_value_info::FrozenConstraintValueInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::execution_platform_registration_info::FrozenExecutionPlatformRegistrationInfo;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::execution_types::execution::APPLY_EXEC_MODIFIERS;
use buck2_core::execution_types::execution::ExecutionPlatform;
use buck2_core::execution_types::execution::ExecutionPlatformError;
use buck2_core::execution_types::execution::ExecutionPlatformIncompatibleReason;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::execution_types::execution::ExecutionPlatformResolutionPartial;
use buck2_core::execution_types::execution_platforms::ExecutionPlatformFallback;
use buck2_core::execution_types::execution_platforms::ExecutionPlatforms;
use buck2_core::execution_types::execution_platforms::ExecutionPlatformsData;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_core::target::target_configured_target_label::TargetConfiguredTargetLabel;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use dice_futures::cancellation::CancellationContext;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configuration_context::AttrConfigurationContextImpl;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::spec::internal::EXEC_COMPATIBLE_WITH_ATTRIBUTE;
use buck2_node::configuration::calculation::CellNameForConfigurationResolution;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use buck2_node::configuration::resolved::MatchedConfigurationSettingKeysWithCfg;
use buck2_node::execution::GetExecutionPlatforms;
use buck2_node::execution::GetExecutionPlatformsImpl;
use buck2_node::execution::EXECUTION_PLATFORMS_BUCKCONFIG;
use buck2_node::execution::GET_EXECUTION_PLATFORMS;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNodeRef;
use derive_more::Display;
use futures::future::FutureExt;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use itertools::Itertools;
use starlark_map::ordered_map::OrderedMap;

use crate::configuration::get_matched_cfg_keys;
use crate::configuration::compute_platform_cfgs;
use crate::configuration::get_matched_cfg_keys_for_node;
use crate::nodes::gather_deps;
use crate::nodes::GatheredDeps;
use crate::nodes::LookingUpConfiguredNodeContext;
use buck2_node::cfg_constructor::CFG_CONSTRUCTOR_CALCULATION_IMPL;
use buck2_core::configuration::pair::Configuration;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum ExecutionPlatformComputationError {
    #[error("Can't find toolchain_dep execution platform using configuration `{0}`")]
    ToolchainDepMissingPlatform(ConfigurationData),
    #[error("Target `{0}` has a transition_dep, which is not permitted on a toolchain rule")]
    ToolchainTransitionDep(TargetLabel),
    #[error(
        "Expected `{0}` to provide a `ExecutionPlatformRegistrationInfo` as it's configured as the `build.execution_platforms` value."
    )]
    MissingExecutionPlatformRegistrationInfo(TargetLabel),
    #[error("Expected `{0}` (exec_marker_constraint) to provide a `ConstraintValueInfo` provider.")]
    MissingConstraintValueInfo(ProvidersLabel),
}

async fn legacy_execution_platform(
    ctx: &DiceComputations<'_>,
    cfg: &ConfigurationNoExec,
) -> ExecutionPlatform {
    ExecutionPlatform::legacy_execution_platform(
        ctx.get_fallback_executor_config().clone(),
        cfg.dupe(),
    )
}

pub async fn find_execution_platform_by_configuration(
    ctx: &mut DiceComputations<'_>,
    exec_cfg: &ConfigurationData,
    cfg: &ConfigurationData,
) -> buck2_error::Result<ExecutionPlatform> {
    match ctx.get_execution_platforms().await? {
        Some(platforms) if exec_cfg != &ConfigurationData::unbound_exec() => {
            for c in platforms.candidates() {
                if c.cfg() == exec_cfg {
                    return Ok(c.dupe());
                }
            }
            Err(buck2_error::Error::from(
                ExecutionPlatformComputationError::ToolchainDepMissingPlatform(exec_cfg.dupe()),
            ))
        }
        _ => Ok(legacy_execution_platform(ctx, &ConfigurationNoExec::new(cfg.dupe())).await),
    }
}

struct ExecutionPlatformConstraints {
    exec_deps: Arc<[TargetLabel]>,
    toolchain_deps: Arc<[TargetConfiguredTargetLabel]>,
    exec_compatible_with: Arc<[ConfigurationSettingKey]>,
}

impl ExecutionPlatformConstraints {
    fn new_constraints(
        exec_deps: Arc<[TargetLabel]>,
        toolchain_deps: Arc<[TargetConfiguredTargetLabel]>,
        exec_compatible_with: Arc<[ConfigurationSettingKey]>,
    ) -> Self {
        Self {
            exec_deps,
            toolchain_deps,
            exec_compatible_with,
        }
    }

    fn new(
        node: TargetNodeRef,
        gathered_deps: &GatheredDeps,
        cfg_ctx: &(dyn AttrConfigurationContext + Sync),
    ) -> buck2_error::Result<Self> {
        let exec_compatible_with: Arc<[_]> = if let Some(a) =
            node.known_attr_or_none(EXEC_COMPATIBLE_WITH_ATTRIBUTE.id, AttrInspectOptions::All)
        {
            let configured_attr = a.configure(cfg_ctx).with_buck_error_context(|| {
                format!(
                    "Error configuring attribute `{}` to resolve execution platform",
                    EXEC_COMPATIBLE_WITH_ATTRIBUTE.name
                )
            })?;
            ConfiguredTargetNode::attr_as_target_compatible_with(configured_attr.value)
                .map(|label| {
                    label.with_buck_error_context(|| {
                        format!("attribute `{}`", EXEC_COMPATIBLE_WITH_ATTRIBUTE.name)
                    })
                })
                .collect::<Result<_, _>>()?
        } else {
            Arc::new([])
        };

        Ok(Self::new_constraints(
            gathered_deps
                .exec_deps
                .iter()
                .map(|c| c.0.target().unconfigured().dupe())
                .collect(),
            gathered_deps
                .toolchain_deps
                .iter()
                .map(|c| c.dupe())
                .collect(),
            exec_compatible_with,
        ))
    }

    /// Gets the compatible execution platforms for a give list of compatible_with constraints and execution deps.
    ///
    /// We do this as a sort of monolithic computation (rather than checking things one-by-one or separating
    /// compatible_with and exec deps) because we expect those values to be common across many nodes (for example,
    /// all c++ targets targeting a specific platform are likely to share compatible_with and exec_deps except
    /// for some rare exceptions). By having a monolithic key like `(Vec<TargetLabel>, Vec<TargetLabel>)` allows all
    /// those nodes to just have a single dice dep. This approach has the downside that it is less incremental, but
    /// we expect these things to change rarely.
    async fn one_for_cell(
        self,
        ctx: &mut DiceComputations<'_>,
        cell: CellNameForConfigurationResolution,
    ) -> buck2_error::Result<ExecutionPlatformResolutionPartial> {
        ctx.compute(&ExecutionPlatformResolutionKey {
            target_node_cell: cell,
            exec_compatible_with: self.exec_compatible_with,
            exec_deps: self.exec_deps,
            toolchain_deps: self.toolchain_deps,
        })
        .await?
    }
}

#[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq, Allocative)]
#[display(
        "ToolchainExecutionPlatformCompatibilityKey({}, {})",
        target,
        exec_platform.id()
    )]
pub(crate) struct ToolchainExecutionPlatformCompatibilityKey {
    target: TargetConfiguredTargetLabel,
    exec_platform: ExecutionPlatform,
}

impl ToolchainExecutionPlatformCompatibilityKey {
    async fn compute_impl(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Result<(), ExecutionPlatformIncompatibleReason>> {
        let node = ctx.get_target_node(self.target.unconfigured()).await?;
        if node.transition_deps().next().is_some() {
            // We could actually check this when defining the rule, but a bit of a corner
            // case, and much simpler to do so here.
            return Err(buck2_error::Error::from(
                ExecutionPlatformComputationError::ToolchainTransitionDep(
                    self.target.unconfigured().dupe(),
                ),
            ));
        }
        let cell_name = CellNameForConfigurationResolution(self.target.pkg().cell_name());
        let matched_cfg_keys =
            get_matched_cfg_keys_for_node(ctx, self.target.cfg(), cell_name, node.as_ref()).await?;
        let platform_cfgs = compute_platform_cfgs(ctx, node.as_ref()).await?;
        // We don't really need `resolved_transitions` here:
        // `Traversal` declared above ignores transitioned dependencies.
        // But we pass `resolved_transitions` here to prevent breakages in the future
        // if something here changes.
        let resolved_transitions = OrderedMap::new();
        let unspecified_resolution = ExecutionPlatformResolution::unspecified();
        let cfg_ctx = AttrConfigurationContextImpl::new(
            &matched_cfg_keys,
            &unspecified_resolution,
            &resolved_transitions,
            &platform_cfgs,
        );
        let (gathered_deps, errors_and_incompats) =
            gather_deps(&self.target, node.as_ref(), &cfg_ctx, ctx).await?;
        if let Some(ret) = errors_and_incompats.finalize() {
            // Statically assert that we hit one of the `?`s
            enum Void {}
            let _: Void = ret?.require_compatible()?;
        }
        let constraints =
            ExecutionPlatformConstraints::new(node.as_ref(), &gathered_deps, &cfg_ctx)?;

        check_execution_platform(
            ctx,
            cell_name,
            &constraints.exec_compatible_with,
            &constraints.exec_deps,
            &self.exec_platform,
            &constraints.toolchain_deps,
        )
        .await
    }
}

#[async_trait]
impl Key for ToolchainExecutionPlatformCompatibilityKey {
    type Value = buck2_error::Result<Result<(), ExecutionPlatformIncompatibleReason>>;
    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        Ok(LookingUpConfiguredNodeContext::add_context(
            self.compute_impl(ctx).await,
            self.target.inner().dupe(),
        )?)
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

async fn check_toolchain_execution_platform_compatibility(
    ctx: &mut DiceComputations<'_>,
    target: TargetConfiguredTargetLabel,
    exec_platform: ExecutionPlatform,
) -> buck2_error::Result<Result<(), ExecutionPlatformIncompatibleReason>> {
    ctx.compute(&ToolchainExecutionPlatformCompatibilityKey {
        target,
        exec_platform,
    })
    .await?
}

pub(crate) async fn get_execution_platform_toolchain_dep(
    ctx: &mut DiceComputations<'_>,
    target_label: &TargetConfiguredTargetLabel,
    target_node: TargetNodeRef<'_>,
) -> buck2_error::Result<MaybeCompatible<ExecutionPlatformResolutionPartial>> {
    assert!(target_node.is_toolchain_rule());
    let target_cfg = target_label.cfg();
    let target_cell = target_node.label().pkg().cell_name();
    let matched_cfg_keys = get_matched_cfg_keys_for_node(
        ctx,
        target_cfg,
        CellNameForConfigurationResolution(target_cell),
        target_node,
    )
    .await?;
    if target_node.transition_deps().next().is_some() {
        Err(buck2_error::Error::from(
            ExecutionPlatformComputationError::ToolchainTransitionDep(
                target_label.unconfigured().dupe(),
            ),
        ))
    } else {
        let platform_cfgs = compute_platform_cfgs(ctx, target_node).await?;
        let resolved_transitions = OrderedMap::new();
        let unspecified_resolution = ExecutionPlatformResolution::unspecified();
        let cfg_ctx = AttrConfigurationContextImpl::new(
            &matched_cfg_keys,
            &unspecified_resolution,
            &resolved_transitions,
            &platform_cfgs,
        );
        let (gathered_deps, errors_and_incompats) =
            gather_deps(target_label, target_node, &cfg_ctx, ctx).await?;
        if let Some(ret) = errors_and_incompats.finalize() {
            return ret;
        }
        Ok(MaybeCompatible::Compatible(
            resolve_execution_platform(
                ctx,
                target_node,
                &matched_cfg_keys,
                &gathered_deps,
                &cfg_ctx,
            )
            .await?,
        ))
    }
}

pub(crate) async fn resolve_execution_platform(
    ctx: &mut DiceComputations<'_>,
    node: TargetNodeRef<'_>,
    matched_cfg_keys: &MatchedConfigurationSettingKeysWithCfg,
    gathered_deps: &GatheredDeps,
    cfg_ctx: &(dyn AttrConfigurationContext + Sync),
) -> buck2_error::Result<ExecutionPlatformResolutionPartial> {
    // If no execution platforms are configured, we fall back to the legacy execution
    // platform behavior. We currently only support legacy execution platforms. That behavior is that there is a
    // single executor config (the fallback config) and the execution platform is in the same
    // configuration as the target.
    // The non-none case will be handled when we invoke the resolve_execution_platform() on ctx below, the none
    // case can't be handled there because we don't pass the full configuration into it.
    if ctx.get_execution_platforms().await?.is_none() {
        return Ok(ExecutionPlatformResolutionPartial::new(
            Some(legacy_execution_platform(ctx, matched_cfg_keys.cfg()).await),
            Vec::new(),
        ));
    };

    let constraints = ExecutionPlatformConstraints::new(node, gathered_deps, cfg_ctx)?;
    constraints
        .one_for_cell(
            ctx,
            CellNameForConfigurationResolution(node.label().pkg().cell_name()),
        )
        .await
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
            buck2_error::Error::from(
                ExecutionPlatformComputationError::MissingExecutionPlatformRegistrationInfo(
                    execution_platforms_target.dupe(),
                ),
            )
        })?;

    // Resolve the exec_marker_constraint if set
    let marker_constraint = if let Some(marker_str) = result.exec_marker_constraint() {
        let marker_label =
            ProvidersLabel::parse(marker_str, cells.root_cell(), &cells, &cell_alias_resolver)?;
        let marker_providers = ctx.get_configuration_analysis_result(&marker_label).await?;
        let constraint_value_info = marker_providers
            .provider_collection()
            .builtin_provider::<FrozenConstraintValueInfo>()
            .ok_or_else(|| {
                buck2_error::Error::from(
                    ExecutionPlatformComputationError::MissingConstraintValueInfo(
                        marker_label.dupe(),
                    ),
                )
            })?;

        Some(constraint_value_info.to_constraint_key_value())
    } else {
        None
    };

    let mut platforms = Vec::new();
    for platform in result.platforms()? {
        platforms.push(platform.to_execution_platform_with_marker(marker_constraint.as_ref())?);
    }
    Ok(Some(Arc::new(ExecutionPlatformsData::new(
        execution_platforms_target,
        platforms,
        result.fallback()?,
    ))))
}

/// Configure an exec_dep with modifiers applied from the execution platform.
/// This function is used in two places:
/// 1. During execution platform selection (check_execution_platform) - to check target_compatible_with
/// 2. During dependency graph construction (nodes.rs) - to create the final configured nodes
pub(crate) async fn configure_exec_dep_with_modifiers(
    ctx: &mut DiceComputations<'_>,
    exec_dep: &TargetLabel,
    execution_platform_cfg: &ConfigurationData,
) -> buck2_error::Result<MaybeCompatible<ConfiguredTargetNode>> {
    if !*APPLY_EXEC_MODIFIERS.get().unwrap_or(&false) {
        let cfg_pair = ConfigurationNoExec::new(execution_platform_cfg.dupe());
        return ctx
            .get_internal_configured_target_node(&exec_dep.configure_pair_no_exec(cfg_pair))
            .await;
    }

    let (node, super_package) = ctx.get_target_node_with_super_package(exec_dep).await?;

    if !execution_platform_cfg.is_bound() {
        // No valid execution platform was resolved (e.g., all candidates were incompatible
        // and fallback is UseUnspecifiedExec). We cannot apply modifiers without a bound
        // configuration, so configure the exec_dep directly. This will likely fail later
        // when resolving configuration settings.
        let cfg_pair = Configuration::new(execution_platform_cfg.dupe(), None);
        return ctx
            .get_internal_configured_target_node(&exec_dep.configure_pair(cfg_pair))
            .await;
    }

    // Extract constraints from the execution platform to use as high-priority modifiers.
    //
    // Execution platform constraints have the highest priority and cannot be
    // overridden by package-level or target-level modifiers on the exec dep.
    //
    // We pass these as cli_modifiers to eval_cfg_constructor because cli_modifiers
    // have the highest priority in modifier resolution. This is an implementation
    // detail - these are NOT actual user-provided CLI modifier.
    let exec_platform_constraints = Arc::new(
        execution_platform_cfg
            .data()?
            .constraints
            .values()
            .map(|value| value.to_string())
            .collect::<Vec<_>>(),
    );

    // Evaluate cfg_constructor to apply modifiers (package-level + target-level + exec platform)
    let cfg_config = CFG_CONSTRUCTOR_CALCULATION_IMPL
        .get()?
        .eval_cfg_constructor(
            ctx,
            node.as_ref(),
            &super_package,
            execution_platform_cfg.dupe(),
            &exec_platform_constraints, // passed as cli_modifiers parameter for highest priority
            node.rule_type(),
            // is configuring exec deps
            true,
        )
        .await
        .with_buck_error_context(|| {
            format!("Resolving modifiers for exec dep target `{}`", exec_dep)
        })?;

    // Create configuration pair with modifiers applied
    let cfg_pair = Configuration::new(cfg_config, None);

    // Configure exec_dep with modifiers applied
    ctx.get_internal_configured_target_node(&exec_dep.configure_pair(cfg_pair))
        .await
}

/// Check if a particular execution platform is compatible with the constraints or not.
/// Return either Ok/Ok if it is, or a reason if not.
async fn check_execution_platform(
    ctx: &mut DiceComputations<'_>,
    target_node_cell: CellNameForConfigurationResolution,
    exec_compatible_with: &[ConfigurationSettingKey],
    exec_deps: &[TargetLabel],
    exec_platform: &ExecutionPlatform,
    toolchain_deps: &[TargetConfiguredTargetLabel],
) -> buck2_error::Result<Result<(), ExecutionPlatformIncompatibleReason>> {
    let matched_cfg_keys = get_matched_cfg_keys(
        ctx,
        exec_platform.cfg(),
        target_node_cell,
        exec_compatible_with,
    )
    .await?;

    // Then check if the platform satisfies compatible_with
    for constraint in exec_compatible_with {
        if matched_cfg_keys
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
    // so that we do not report an error if we would later find an incompatibility.
    let dep_results = ctx
        .compute_join(exec_deps.iter(), |ctx, dep| {
            Box::pin(async move {
                let cfg = exec_platform.cfg().dupe();
                let result = configure_exec_dep_with_modifiers(ctx, dep, &cfg).await;
                match result {
                    Ok(MaybeCompatible::Compatible(_)) => Ok(None),
                    Ok(MaybeCompatible::Incompatible(reason)) => Ok(Some(reason)),
                    Err(e) => Err(e.context(format!(
                        "Error checking compatibility of `{}` with `{}`",
                        dep, cfg
                    ))),
                }
            })
        })
        .await;

    let mut errs = Vec::new();
    for result in dep_results {
        match result {
            Ok(None) => (),
            Ok(Some(reason)) => {
                return Ok(Err(
                    ExecutionPlatformIncompatibleReason::ExecutionDependencyIncompatible(
                        reason.dupe(),
                    ),
                ));
            }
            Err(e) => errs.push(e),
        };
    }

    for result in ctx
        .compute_join(toolchain_deps.iter(), |ctx, dep| {
            let dep = dep.dupe();
            let exec_platform = exec_platform.dupe();
            async move {
                check_toolchain_execution_platform_compatibility(ctx, dep, exec_platform).await
            }
            .boxed()
        })
        .await
    {
        match result {
            Ok(Ok(())) => {}
            Ok(Err(reason)) => {
                return Ok(Err(reason));
            }
            Err(e) => errs.push(e),
        }
    }
    if let Some(e) = errs.pop() {
        return Err(e);
    }

    Ok(Ok(()))
}

async fn get_execution_platforms_enabled(
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<ExecutionPlatforms> {
    ctx.get_execution_platforms()
        .await?
        .ok_or_else(|| internal_error!("Execution platforms are not enabled"))
}

async fn resolve_execution_platform_from_constraints(
    ctx: &mut DiceComputations<'_>,
    target_node_cell: CellNameForConfigurationResolution,
    exec_compatible_with: &[ConfigurationSettingKey],
    exec_deps: &[TargetLabel],
    toolchain_deps: &[TargetConfiguredTargetLabel],
) -> buck2_error::Result<ExecutionPlatformResolutionPartial> {
    let mut skipped = Vec::new();
    let execution_platforms = get_execution_platforms_enabled(ctx).await?;
    for exec_platform in execution_platforms.candidates() {
        match check_execution_platform(
            ctx,
            target_node_cell,
            exec_compatible_with,
            exec_deps,
            exec_platform,
            toolchain_deps,
        )
        .await?
        {
            Ok(()) => {
                return Ok(ExecutionPlatformResolutionPartial::new(
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
            Ok(ExecutionPlatformResolutionPartial::new(None, skipped))
        }
        ExecutionPlatformFallback::Error => {
            Err(ExecutionPlatformError::NoCompatiblePlatform(Arc::new(skipped)).into())
        }
        ExecutionPlatformFallback::Platform(platform) => Ok(
            ExecutionPlatformResolutionPartial::new(Some(platform.dupe()), skipped),
        ),
    }
}

#[derive(Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative)]
pub(crate) struct ExecutionPlatformResolutionKey {
    /// Determining a compatible execution platform requires checking the target and toolchain's
    /// exec_compatible_with. This in turn requires a ResolvedConfiguration, which resolves the
    /// buckconfig-related config_setting values based on the cell of the target the configuration
    /// is being resolved for.
    target_node_cell: CellNameForConfigurationResolution,
    exec_compatible_with: Arc<[ConfigurationSettingKey]>,
    exec_deps: Arc<[TargetLabel]>,
    toolchain_deps: Arc<[TargetConfiguredTargetLabel]>,
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

        if !self.toolchain_deps.is_empty() {
            write!(
                f,
                ", toolchain_deps=[{}]",
                self.toolchain_deps.iter().join(", ")
            )?;
        }

        Ok(())
    }
}

#[async_trait]
impl Key for ExecutionPlatformResolutionKey {
    type Value = buck2_error::Result<ExecutionPlatformResolutionPartial>;

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
            &self.toolchain_deps,
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

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display("ExecutionPlatforms")]
pub struct ExecutionPlatformsKey;

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

struct GetExecutionPlatformsInstance;

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
        cell: CellNameForConfigurationResolution,
    ) -> buck2_error::Result<ExecutionPlatformResolutionPartial> {
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
