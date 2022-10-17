/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Calculations relating to 'TargetNode's that runs on Dice

use std::ops::ControlFlow;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_common::ordered_map::OrderedMap;
use buck2_common::result::SharedError;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::configuration::Configuration;
use buck2_core::configuration::ConfigurationData;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_execute::execute::dice_data::HasFallbackExecutorConfig;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configuration_context::AttrConfigurationContextImpl;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::configured_traversal::ConfiguredAttrTraversal;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::internal::EXEC_COMPATIBLE_WITH_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD;
use buck2_node::attrs::internal::TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD;
use buck2_node::compatibility::IncompatiblePlatformReason;
use buck2_node::compatibility::IncompatiblePlatformReasonCause;
use buck2_node::compatibility::MaybeCompatible;
use buck2_node::configuration::execution::ExecutionPlatform;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use buck2_node::configuration::resolved::ConfigurationSettingKeyRef;
use buck2_node::configuration::resolved::ResolvedConfiguration;
use buck2_node::configuration::toolchain_constraints::ToolchainConstraints;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::visibility::VisibilityError;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexedSet;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use gazebo::prelude::*;
use indexmap::IndexSet;
use itertools::Either;
use itertools::Itertools;
use starlark::collections::SmallSet;
use thiserror::Error;

use crate::calculation::BuildErrors;
use crate::configuration::ConfigurationCalculation;
use crate::interpreter::rule_defs::transition::calculation_apply_transition::ApplyTransition;

#[derive(Debug, thiserror::Error)]
enum NodeCalculationError {
    #[error("expected `{0}` attribute to be a list but got `{1}`")]
    TargetCompatibleNotList(String, String),
    #[error(
        "`{0}` had both `{}` and `{}` attributes. It should only have one.",
        TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
        LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD
    )]
    BothTargetCompatibleWith(String),
}

#[async_trait]
pub(crate) trait NodeCalculation {
    /// For a TargetLabel, returns the TargetNode. This is really just part of the the interpreter
    /// results for the the label's package, and so this is just a utility for accessing that, it
    /// isn't separately cached.
    async fn get_target_node(&self, target: &TargetLabel) -> SharedResult<TargetNode>;

    /// Returns the ConfiguredTargetNode corresponding to a ConfiguredTargetLabel.
    async fn get_configured_target_node(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> SharedResult<MaybeCompatible<ConfiguredTargetNode>>;
}

enum CompatibilityConstraints {
    Any(ConfiguredAttr),
    All(ConfiguredAttr),
}

async fn compute_platform_cfgs(
    ctx: &DiceComputations,
    node: &TargetNode,
) -> anyhow::Result<OrderedMap<TargetLabel, Configuration>> {
    let mut platform_map = OrderedMap::new();
    for platform_target in node.platform_deps() {
        let config = ctx.get_platform_configuration(platform_target).await?;
        platform_map.insert(platform_target.dupe(), config);
    }

    Ok(platform_map)
}

async fn legacy_execution_platform(
    ctx: &DiceComputations,
    resolved_configuration: &ResolvedConfiguration,
) -> ExecutionPlatform {
    ExecutionPlatform::legacy_execution_platform(
        ctx.get_fallback_executor_config().clone(),
        resolved_configuration.cfg().dupe(),
    )
}

#[derive(Debug, Error)]
enum ToolchainDepError {
    #[error("Can't find toolchain_dep execution platform using configuration `{0}`")]
    ToolchainDepMissingPlatform(Configuration),
    #[error("Target `{0}` was used as a toolchain_dep, but is not a toolchain rule")]
    NonToolchainRuleUsedAsToolchainDep(TargetLabel),
    #[error("Target `{0}` was used not as a toolchain_dep, but is a toolchain rule")]
    ToolchainRuleUsedAsNormalDep(TargetLabel),
    #[error("Target `{0}` has a transition_dep, which is not permitted on a toolchain rule")]
    ToolchainTransitionDep(TargetLabel),
}

async fn find_execution_platform_by_configuration(
    ctx: &DiceComputations,
    exec_cfg: &Configuration,
    resolved_configuration: &ResolvedConfiguration,
) -> SharedResult<ExecutionPlatform> {
    match ctx.get_execution_platforms().await? {
        Some(candidates) if exec_cfg != &Configuration::unbound_exec() => {
            for c in candidates.iter() {
                if &c.cfg() == exec_cfg {
                    return Ok(c.dupe());
                }
            }
            Err(SharedError::new(
                ToolchainDepError::ToolchainDepMissingPlatform(exec_cfg.dupe()),
            ))
        }
        _ => Ok(legacy_execution_platform(ctx, resolved_configuration).await),
    }
}

#[derive(Default)]
struct ExecutionPlatformConstraints {
    exec_deps: IndexSet<TargetLabel>,
    toolchain_deps: IndexSet<ConfiguredTargetLabel>,
    exec_compatible_with: Vec<TargetLabel>,
}

impl<'a> ConfiguredAttrTraversal<'a> for ExecutionPlatformConstraints {
    fn dep(&mut self, _dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        Ok(())
    }

    fn exec_dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        // TODO(cjhopman): Check that the dep is in the unbound_exe configuration
        self.exec_deps.insert(dep.target().unconfigured().dupe());
        Ok(())
    }

    fn toolchain_dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
        self.toolchain_deps.insert(dep.target().dupe());
        Ok(())
    }
}

impl ExecutionPlatformConstraints {
    async fn new(
        ctx: &DiceComputations,
        node: &TargetNode,
        resolved_configuration: &ResolvedConfiguration,
        resolved_transitions: &OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
    ) -> SharedResult<Self> {
        let mut me = Self::default();

        let cfg_ctx = AttrConfigurationContextImpl {
            resolved_cfg: resolved_configuration,
            exec_cfg: &Configuration::unbound_exec(),
            // We don't really need `resolved_transitions` here:
            // `Traversal` declared above ignores transitioned dependencies.
            // But we pass `resolved_transitions` here to prevent breakages in the future
            // if something here changes.
            resolved_transitions,
            platform_cfgs: &compute_platform_cfgs(ctx, node).await?,
        };

        for (name, attr) in node.attrs(AttrInspectOptions::All) {
            let configured_attr = attr.configure(&cfg_ctx).with_context(|| {
                format!(
                    "when configuring attribute `{}` to resolve execution platform",
                    name
                )
            })?;
            configured_attr.traverse(&mut me)?;
            if name == EXEC_COMPATIBLE_WITH_ATTRIBUTE_FIELD {
                me.exec_compatible_with.extend(
                    ConfiguredTargetNode::attr_as_target_compatible_with(configured_attr),
                );
            }
        }
        Ok(me)
    }

    async fn toolchain_allows(
        &self,
        ctx: &DiceComputations,
    ) -> SharedResult<Vec<ToolchainConstraints>> {
        // We could merge these constraints together, but the time to do that
        // probably outweighs the benefits given there are likely to only be a few
        // execution platforms to test.
        let mut result = Vec::with_capacity(self.toolchain_deps.len());
        for x in &self.toolchain_deps {
            result.push(execution_platforms_for_toolchain(ctx, x.dupe()).await?)
        }
        Ok(result)
    }

    async fn one(
        &self,
        ctx: &DiceComputations,
        node: &TargetNode,
    ) -> SharedResult<ExecutionPlatformResolution> {
        ctx.resolve_execution_platform_from_constraints(
            node.label().pkg().cell_name(),
            &self.exec_compatible_with,
            &self.exec_deps,
            &self.toolchain_allows(ctx).await?,
        )
        .await
    }

    async fn many(
        &self,
        ctx: &DiceComputations,
        target: &ConfiguredTargetLabel,
    ) -> SharedResult<ToolchainConstraints> {
        ctx.resolve_toolchain_constraints_from_constraints(
            target,
            &self.exec_compatible_with,
            &self.exec_deps,
            &self.toolchain_allows(ctx).await?,
        )
        .await
    }
}

async fn execution_platforms_for_toolchain(
    ctx: &DiceComputations,
    target: ConfiguredTargetLabel,
) -> SharedResult<ToolchainConstraints> {
    #[derive(Clone, Display, Debug, Dupe, Eq, Hash, PartialEq)]
    #[display(fmt = "{}", .0)]
    struct ExecutionPlatformsForToolchainKey(ConfiguredTargetLabel);

    #[async_trait]
    impl Key for ExecutionPlatformsForToolchainKey {
        type Value = SharedResult<ToolchainConstraints>;
        async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
            let node = ctx.get_target_node(self.0.unconfigured()).await?;
            if node.transition_deps().next().is_some() {
                // We could actually check this when defining the rule, but a bit of a corner
                // case, and much simpler to do so here.
                return Err(SharedError::new(ToolchainDepError::ToolchainTransitionDep(
                    self.0.unconfigured().dupe(),
                )));
            }
            let resolved_configuration = &ctx
                .get_resolved_configuration(
                    self.0.cfg(),
                    self.0.pkg().cell_name(),
                    node.get_configuration_deps(),
                )
                .await?;
            let constraints = ExecutionPlatformConstraints::new(
                ctx,
                &node,
                resolved_configuration,
                &OrderedMap::new(),
            )
            .await?;
            constraints.many(ctx, &self.0).await
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            match (x, y) {
                (Ok(x), Ok(y)) => x == y,
                _ => false,
            }
        }
    }

    ctx.compute(&ExecutionPlatformsForToolchainKey(target))
        .await?
}

pub async fn get_execution_platform_toolchain_dep(
    ctx: &DiceComputations,
    target_label: &ConfiguredTargetLabel,
    target_node: &TargetNode,
) -> SharedResult<ExecutionPlatformResolution> {
    assert!(target_node.is_toolchain_rule());
    let target_cfg = target_label.cfg();
    let target_cell = target_node.label().pkg().cell_name();
    let resolved_configuration = ctx
        .get_resolved_configuration(
            target_cfg,
            target_cell,
            target_node.get_configuration_deps(),
        )
        .await?;
    if target_node.transition_deps().next().is_some() {
        Err(SharedError::new(ToolchainDepError::ToolchainTransitionDep(
            target_label.unconfigured().dupe(),
        )))
    } else {
        resolve_execution_platform(
            ctx,
            target_node,
            &resolved_configuration,
            &OrderedMap::new(),
        )
        .await
    }
}

async fn resolve_execution_platform(
    ctx: &DiceComputations,
    node: &TargetNode,
    resolved_configuration: &ResolvedConfiguration,
    resolved_transitions: &OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
) -> SharedResult<ExecutionPlatformResolution> {
    // If no execution platforms are configured, we fall back to the legacy execution
    // platform behavior. We currently only support legacy execution platforms. That behavior is that there is a
    // single executor config (the fallback config) and the execution platform is in the same
    // configuration as the target.
    // The non-none case will be handled when we invoke the resolve_execution_platform() on ctx below, the none
    // case can't be handled there because we don't pass the full configuration into it.
    if ctx.get_execution_platforms().await?.is_none() {
        return Ok(ExecutionPlatformResolution::new(
            Some(legacy_execution_platform(ctx, resolved_configuration).await),
            Vec::new(),
        ));
    };

    let constraints =
        ExecutionPlatformConstraints::new(ctx, node, resolved_configuration, resolved_transitions)
            .await?;
    constraints.one(ctx, node).await
}

fn unpack_target_compatible_with_attr(
    target_node: &TargetNode,
    resolved_cfg: &ResolvedConfiguration,
    attr_name: &str,
) -> anyhow::Result<Option<ConfiguredAttr>> {
    let attr = target_node.attr_or_none(attr_name, AttrInspectOptions::All);
    let attr = match attr {
        Some(attr) => attr,
        None => return Ok(None),
    };

    struct AttrConfigurationContextToResolveCompatibleWith<'c> {
        resolved_cfg: &'c ResolvedConfiguration,
    }

    impl<'c> AttrConfigurationContext for AttrConfigurationContextToResolveCompatibleWith<'c> {
        fn matches<'a>(&'a self, label: &TargetLabel) -> Option<&'a ConfigurationData> {
            self.resolved_cfg
                .setting_matches(ConfigurationSettingKeyRef(label))
        }

        fn cfg(&self) -> &Configuration {
            self.resolved_cfg.cfg()
        }

        fn platform_cfg(&self, _label: &TargetLabel) -> anyhow::Result<&Configuration> {
            unreachable!(
                "platform_cfg() is not needed to resolve `{}` or `{}`",
                TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
                LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD
            )
        }

        fn exec_cfg(&self) -> &Configuration {
            unreachable!(
                "exec_cfg() is not needed to resolve `{}` or `{}`",
                TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
                LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD
            )
        }

        fn resolved_transitions(&self) -> &OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>> {
            unreachable!(
                "resolved_transitions() is not needed to resolve `{}` or `{}`",
                TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
                LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD
            )
        }
    }

    let attr = attr
        .configure(&AttrConfigurationContextToResolveCompatibleWith { resolved_cfg })
        .with_context(|| format!("when configuring attribute `{}`", attr_name))?;

    match attr.unpack_list() {
        Some(values) => {
            if !values.is_empty() {
                Ok(Some(attr))
            } else {
                Ok(None)
            }
        }
        None => Err(NodeCalculationError::TargetCompatibleNotList(
            attr_name.to_owned(),
            attr.to_string(),
        )
        .into()),
    }
}

fn check_compatible(
    target_label: &ConfiguredTargetLabel,
    target_node: &TargetNode,
    resolved_cfg: &ResolvedConfiguration,
) -> anyhow::Result<MaybeCompatible<()>> {
    let target_compatible_with = unpack_target_compatible_with_attr(
        target_node,
        resolved_cfg,
        TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
    )?;
    let legacy_compatible_with = unpack_target_compatible_with_attr(
        target_node,
        resolved_cfg,
        LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
    )?;

    let compatibility_constraints = match (target_compatible_with, legacy_compatible_with) {
        (None, None) => return Ok(MaybeCompatible::Compatible(())),
        (Some(..), Some(..)) => {
            return Err(
                NodeCalculationError::BothTargetCompatibleWith(target_label.to_string()).into(),
            );
        }
        (Some(target_compatible_with), None) => {
            CompatibilityConstraints::All(target_compatible_with)
        }
        (None, Some(legacy_compatible_with)) => {
            CompatibilityConstraints::Any(legacy_compatible_with)
        }
    };

    // We are compatible if the list of target expressions is empty,
    // OR if we match ANY expression in the list of attributes.
    let check_compatibility = |attr| -> (Vec<_>, Vec<_>) {
        ConfiguredTargetNode::attr_as_target_compatible_with(attr).partition_map(|label| {
            match resolved_cfg.matches(&label) {
                Some(_) => Either::Left(label),
                None => Either::Right(label),
            }
        })
    };

    // We only record the first incompatibility, for either ANY or ALL.
    // TODO(cjhopman): Should we report _all_ the things that are incompatible?
    let incompatible_target = match compatibility_constraints {
        CompatibilityConstraints::Any(attr) => {
            let (compatible, incompatible) = check_compatibility(attr);
            let incompatible = incompatible.into_iter().next();
            match (compatible.is_empty(), incompatible.into_iter().next()) {
                (false, _) | (true, None) => {
                    return Ok(MaybeCompatible::Compatible(()));
                }
                (true, Some(v)) => v,
            }
        }
        CompatibilityConstraints::All(attr) => {
            let (_compatible, incompatible) = check_compatibility(attr);
            match incompatible.into_iter().next() {
                Some(label) => label,
                None => {
                    return Ok(MaybeCompatible::Compatible(()));
                }
            }
        }
    };
    Ok(MaybeCompatible::Incompatible(Arc::new(
        IncompatiblePlatformReason {
            target: target_label.dupe(),
            cause: IncompatiblePlatformReasonCause::UnsatisfiedConfig(incompatible_target),
        },
    )))
}

/// Compute configured target node ignoring transition for this node.
async fn compute_configured_target_node_no_transition(
    target_label: &ConfiguredTargetLabel,
    target_node: TargetNode,
    ctx: &DiceComputations,
) -> SharedResult<MaybeCompatible<ConfiguredTargetNode>> {
    let target_cfg = target_label.cfg();
    let target_cell = target_node.label().pkg().cell_name();
    let resolved_configuration = ctx
        .get_resolved_configuration(
            target_cfg,
            target_cell,
            target_node.get_configuration_deps(),
        )
        .await?;

    // Must check for compatibility before evaluating non-compatibility attributes.
    if let MaybeCompatible::Incompatible(reason) =
        check_compatible(target_label, &target_node, &resolved_configuration)?
    {
        return Ok(MaybeCompatible::Incompatible(reason));
    }

    struct Traversal<'a> {
        deps: &'a mut SmallSet<ConfiguredProvidersLabel>,
        exec_deps: &'a mut SmallSet<ConfiguredProvidersLabel>,
    }

    impl<'a> ConfiguredAttrTraversal<'a> for Traversal<'_> {
        fn dep(&mut self, dep: &ConfiguredProvidersLabel) -> anyhow::Result<()> {
            self.deps.insert(dep.clone());
            Ok(())
        }

        fn exec_dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
            self.exec_deps.insert(dep.clone());
            Ok(())
        }
    }

    let mut resolved_transitions = OrderedMap::new();
    for (_dep, tr) in target_node.transition_deps() {
        let resolved_cfg = ctx.apply_transition(&target_node, target_cfg, tr).await?;
        resolved_transitions.insert(tr.dupe(), resolved_cfg);
    }

    let execution_platform_resolution = if target_cfg.is_unbound() {
        // The unbound configuration is used when evaluation configuration nodes.
        // That evaluation is
        // (1) part of execution platform resolution and
        // (2) isn't allowed to do execution
        // And so we use an "unspecified" execution platform to avoid cycles and cause any attempts at execution to fail.
        ExecutionPlatformResolution::unspecified()
    } else if let Some(exec_cfg) = target_label.exec_cfg() {
        // The label was produced by a toolchain_dep, so we use the execution platform of our parent
        // We need to convert that to an execution platform, so just find the one with the same configuration.
        ExecutionPlatformResolution::new(
            Some(
                find_execution_platform_by_configuration(ctx, exec_cfg, &resolved_configuration)
                    .await?,
            ),
            Vec::new(),
        )
    } else {
        resolve_execution_platform(
            ctx,
            &target_node,
            &resolved_configuration,
            &resolved_transitions,
        )
        .await?
    };

    let mut deps = SmallSet::new();
    let mut exec_deps = SmallSet::new();

    let platform_cfgs = compute_platform_cfgs(ctx, &target_node).await?;

    // We need to collect deps and to ensure that all attrs can be successfully
    // configured so that we don't need to support propagate configuration errors on attr access.
    for (attr_name, attr) in target_node.attrs(AttrInspectOptions::All) {
        let mut traversal = Traversal {
            deps: &mut deps,
            exec_deps: &mut exec_deps,
        };
        let attr_cfg_ctx = AttrConfigurationContextImpl {
            resolved_cfg: &resolved_configuration,
            exec_cfg: &execution_platform_resolution.cfg(),
            resolved_transitions: &resolved_transitions,
            platform_cfgs: &platform_cfgs,
        };

        let configured_attr = attr
            .configure(&attr_cfg_ctx)
            .with_context(|| format!("when configuring attribute `{}`", attr_name))?;
        configured_attr.traverse(&mut traversal)?;
    }

    // Check transitive target compatibility.
    let dep_futures: Vec<_> = deps
        .iter()
        .map(|v| ctx.get_configured_target_node(v.target()))
        .collect();

    let exec_dep_futures: Vec<_> = exec_deps
        .iter()
        .map(|v| ctx.get_configured_target_node(v.target()))
        .collect();

    let (dep_results, exec_dep_results): (Vec<_>, Vec<_>) = futures::future::join(
        futures::future::join_all(dep_futures),
        futures::future::join_all(exec_dep_futures),
    )
    .await;
    let unpack_dep = |
        result: SharedResult<MaybeCompatible<ConfiguredTargetNode>>| -> ControlFlow<_, ConfiguredTargetNode>
    {
        match result {
            Err(e) => ControlFlow::Break(Err(e)),
            Ok(MaybeCompatible::Incompatible(reason)) => {
                ControlFlow::Break(Ok(MaybeCompatible::Incompatible(Arc::new(IncompatiblePlatformReason {
                    target: target_label.dupe(),
                    cause: IncompatiblePlatformReasonCause::Dependency(reason.dupe()),
                }))))
            }
            Ok(MaybeCompatible::Compatible(dep)) => {
                if !dep.is_visible_to(target_label.unconfigured()) {
                    ControlFlow::Break(
                        Err(anyhow::anyhow!(VisibilityError::NotVisibleTo(
                            dep.name().unconfigured().dupe(),
                            target_label.unconfigured().dupe(),
                        )))
                        .shared_error(),
                    )
                } else {
                    ControlFlow::Continue(dep)
                }
            }
        }
    };

    let mut deps = LabelIndexedSet::new();
    for dep in dep_results {
        match unpack_dep(dep) {
            ControlFlow::Continue(dep) => deps.insert(dep),
            ControlFlow::Break(r) => return r,
        };
    }

    let mut exec_deps = LabelIndexedSet::new();
    for dep in exec_dep_results {
        match unpack_dep(dep) {
            ControlFlow::Continue(dep) => exec_deps.insert(dep),
            ControlFlow::Break(r) => return r,
        };
    }

    Ok(MaybeCompatible::Compatible(ConfiguredTargetNode::new(
        target_label.dupe(),
        target_node.dupe(),
        resolved_configuration,
        resolved_transitions,
        execution_platform_resolution,
        deps,
        exec_deps,
        platform_cfgs,
    )))
}

/// Compute configured target node after transition is applied to the target.
///
/// This function creates two node: transitioned node and a forward node.
/// Forward node is returned.
async fn compute_configured_target_node_with_transition(
    key: &ConfiguredTransitionedNodeKey,
    ctx: &DiceComputations,
) -> SharedResult<MaybeCompatible<ConfiguredTargetNode>> {
    assert_eq!(
        key.forward.unconfigured(),
        key.transitioned.unconfigured(),
        "Transition can be done only to the nodes with different configuration; \
               this valid case was ruled out earlier"
    );
    assert_ne!(
        key.forward, key.transitioned,
        "Transition can only happen to a node with the same unconfigured target"
    );

    let target_node = ctx.get_target_node(key.transitioned.unconfigured()).await?;
    let transitioned_node =
        compute_configured_target_node_no_transition(&key.transitioned, target_node.dupe(), ctx)
            .await?;
    Ok(transitioned_node.map(|transitioned_node| {
        ConfiguredTargetNode::new_forward(key.forward.dupe(), transitioned_node)
    }))
}

async fn compute_configured_target_node(
    key: &ConfiguredNodeKey,
    ctx: &DiceComputations,
) -> SharedResult<MaybeCompatible<ConfiguredTargetNode>> {
    let target_node = ctx
        .get_target_node(key.0.unconfigured())
        .await
        .with_context(|| {
            format!(
                "looking up unconfigured target node `{}`",
                key.0.unconfigured()
            )
        })?;

    match key.0.exec_cfg() {
        None if target_node.is_toolchain_rule() => {
            return Err(SharedError::new(
                ToolchainDepError::ToolchainRuleUsedAsNormalDep(key.0.unconfigured().dupe()),
            ));
        }
        Some(_) if !target_node.is_toolchain_rule() => {
            return Err(SharedError::new(
                ToolchainDepError::NonToolchainRuleUsedAsToolchainDep(key.0.unconfigured().dupe()),
            ));
        }
        _ => {}
    }

    if let Some(transition_id) = &target_node.0.cfg {
        #[async_trait]
        impl Key for ConfiguredTransitionedNodeKey {
            type Value = SharedResult<MaybeCompatible<ConfiguredTargetNode>>;

            async fn compute(
                &self,
                ctx: &DiceComputations,
            ) -> SharedResult<MaybeCompatible<ConfiguredTargetNode>> {
                compute_configured_target_node_with_transition(self, ctx).await
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                if let (Ok(x), Ok(y)) = (x, y) {
                    x == y
                } else {
                    false
                }
            }
        }

        let cfg = ctx
            .apply_transition(&target_node, key.0.cfg(), transition_id)
            .await?;
        let configured_target_label = key.0.unconfigured().configure(cfg.single()?.dupe());

        if configured_target_label == key.0 {
            // Transitioned to identical configured target, no need to create a forward node.
            compute_configured_target_node_no_transition(&key.0, target_node.dupe(), ctx).await
        } else {
            ctx.compute(&ConfiguredTransitionedNodeKey {
                forward: key.0.dupe(),
                transitioned: configured_target_label,
            })
            .await?
        }
    } else {
        // We are not caching `ConfiguredTransitionedNodeKey` because this is cheap,
        // and no need to fetch `target_node` again.
        compute_configured_target_node_no_transition(&key.0.dupe(), target_node, ctx).await
    }
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "{}", _0)]
pub struct ConfiguredNodeKey(pub ConfiguredTargetLabel);

/// Similar to [`ConfiguredNodeKey`], but used when the target
/// is transitioned to different configuration because rule definition requires it.
#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "ConfiguredTransitionedNodeKey({}, {})", forward, transitioned)]
pub struct ConfiguredTransitionedNodeKey {
    /// Forward node label.
    forward: ConfiguredTargetLabel,
    /// Transitional node label.
    transitioned: ConfiguredTargetLabel,
}

#[async_trait]
impl NodeCalculation for DiceComputations {
    async fn get_target_node(&self, target: &TargetLabel) -> SharedResult<TargetNode> {
        Ok(self
            .get_interpreter_results(target.pkg())
            .await
            .with_context(|| format!("loading targets in package `{}`", target.pkg()))?
            .targets()
            .get(target.name())
            .ok_or_else(|| {
                anyhow::anyhow!(BuildErrors::MissingTarget(
                    target.pkg().dupe(),
                    target.name().dupe()
                ))
            })?
            .dupe())
    }

    async fn get_configured_target_node(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> SharedResult<MaybeCompatible<ConfiguredTargetNode>> {
        #[async_trait]
        impl Key for ConfiguredNodeKey {
            type Value = SharedResult<MaybeCompatible<ConfiguredTargetNode>>;
            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let res = compute_configured_target_node(self, ctx).await;
                Ok(res.with_context(|| format!("when looking up configured node {}", self.0))?)
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        self.compute(&ConfiguredNodeKey(target.dupe())).await?
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_common::executor_config::CommandExecutorConfig;
    use buck2_core::build_file_path::BuildFilePath;
    use buck2_core::bzl::ImportPath;
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::paths::FileNameBuf;
    use buck2_core::package::testing::PackageExt;
    use buck2_core::package::Package;
    use buck2_core::provider::label::ProvidersLabel;
    use buck2_core::provider::label::ProvidersName;
    use buck2_core::target::TargetLabel;
    use buck2_core::target::TargetName;
    use buck2_execute::execute::dice_data::set_fallback_executor_config;
    use buck2_interpreter_for_build::attrs::coerce::testing::CoercedAttrExt;
    use buck2_interpreter_for_build::attrs::coerce::testing::ConfiguredAttrExt;
    use buck2_interpreter_for_build::interpreter::calculation::testing::InterpreterResultsKey;
    use buck2_node::attrs::attr::testing::AttributeExt;
    use buck2_node::attrs::attr::Attribute;
    use buck2_node::attrs::attr_type::any::AnyAttrType;
    use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
    use buck2_node::attrs::attr_type::dep::DepAttr;
    use buck2_node::attrs::attr_type::dep::DepAttrTransition;
    use buck2_node::attrs::attr_type::dep::DepAttrType;
    use buck2_node::attrs::attr_type::AttrType;
    use buck2_node::attrs::coerced_attr::CoercedAttr;
    use buck2_node::attrs::configured_attr::ConfiguredAttr;
    use buck2_node::attrs::inspect_options::AttrInspectOptions;
    use buck2_node::nodes::eval_result::EvaluationResult;
    use buck2_node::nodes::unconfigured::testing::TargetNodeExt;
    use buck2_node::nodes::unconfigured::TargetNode;
    use buck2_node::rule_type::RuleType;
    use buck2_node::rule_type::StarlarkRuleType;
    use dice::testing::DiceBuilder;
    use dice::UserComputationData;
    use gazebo::prelude::*;
    use indexmap::indexmap;
    use starlark::collections::SmallMap;
    use starlark_map::smallmap;

    use crate::configuration::calculation::ExecutionPlatformsKey;
    use crate::nodes::calculation::NodeCalculation;

    #[tokio::test]
    async fn test_get_node() -> anyhow::Result<()> {
        let cfg = Configuration::testing_new();
        let pkg = Package::testing_new("cell", "foo/bar");

        let name1 = TargetName::unchecked_new("t1");
        let label1 = TargetLabel::new(pkg.dupe(), name1.dupe());

        let name2 = TargetName::unchecked_new("t2");
        let label2 = TargetLabel::new(pkg.dupe(), name2.dupe());

        let rule_type = RuleType::Starlark(Arc::new(StarlarkRuleType {
            import_path: ImportPath::unchecked_new("cell", "foo/bar", "def.bzl"),
            name: "some_rule".to_owned(),
        }));
        let attrs1 = vec![
            (
                "bool_field",
                Attribute::testing_new(None, AttrType::bool()),
                CoercedAttr::from_literal(AttrLiteral::Bool(false)),
            ),
            (
                "another_field",
                Attribute::testing_new(None, AttrType::string()),
                CoercedAttr::from_literal(AttrLiteral::String("some_string".into())),
            ),
            (
                "some_deps",
                Attribute::testing_new(None, AttrType::list(AttrType::dep(Vec::new()))),
                CoercedAttr::from_literal(AttrLiteral::List(
                    vec![CoercedAttr::from_literal(AttrLiteral::Dep(
                        box DepAttr::new(
                            DepAttrType::new(Vec::new(), DepAttrTransition::Identity),
                            ProvidersLabel::new(label2.dupe(), ProvidersName::Default),
                        ),
                    ))]
                    .into_boxed_slice(),
                    AttrType::dep(Vec::new()),
                )),
            ),
        ];

        let node1 = TargetNode::testing_new(label1.dupe(), rule_type.dupe(), attrs1);

        let attrs2 = vec![
            (
                "bool_field",
                Attribute::testing_new(None, AttrType::bool()),
                CoercedAttr::from_literal(AttrLiteral::Bool(true)),
            ),
            (
                "another_field",
                Attribute::testing_new(None, AttrType::string()),
                CoercedAttr::from_literal(AttrLiteral::String("another_string".into())),
            ),
            (
                "some_deps",
                Attribute::testing_new(None, AttrType::list(AttrType::dep(Vec::new()))),
                AnyAttrType::empty_list(AttrType::dep(Vec::new())),
            ),
        ];

        let node2 = TargetNode::testing_new(label2.dupe(), rule_type.dupe(), attrs2);

        let eval_result = EvaluationResult::new(
            Arc::new(BuildFilePath::new(
                pkg.dupe(),
                FileNameBuf::unchecked_new("BUCK"),
            )),
            Vec::new(),
            indexmap![name1 => node1.dupe(), name2 => node2.dupe()],
        );

        let mut data = UserComputationData::new();
        set_fallback_executor_config(&mut data.data, CommandExecutorConfig::testing_local());
        let mut computations = DiceBuilder::new()
            .mock_and_return(InterpreterResultsKey(pkg), Ok(Arc::new(eval_result)))
            .mock_and_return(ExecutionPlatformsKey, Ok(None))
            .build(data)?;
        computations = computations.commit();

        let node = computations.get_target_node(&label1).await?;
        assert_eq!(node.0, node1.0);

        let node = computations.get_target_node(&label2).await?;
        assert_eq!(node.0, node2.0);

        let conf_attrs1 = smallmap![
            "bool_field" => ConfiguredAttr::from_literal(AttrLiteral::Bool(false)),
            "another_field" =>
             ConfiguredAttr::from_literal(AttrLiteral::String("some_string".into())),
            "some_deps" =>
             ConfiguredAttr::from_literal(AttrLiteral::List(vec![
                ConfiguredAttr::from_literal(AttrLiteral::Dep(box DepAttr::new(
                    DepAttrType::new(Vec::new(), DepAttrTransition::Identity),
                    ProvidersLabel::new(label2.dupe(), ProvidersName::Default)
                        .configure(cfg.dupe()),
                ))),
            ].into_boxed_slice(), AttrType::dep(Vec::new()))),
        ];

        let conf_attrs2 = smallmap![
            "bool_field" => ConfiguredAttr::from_literal(AttrLiteral::Bool(true)),
            "another_field" =>
             ConfiguredAttr::from_literal(AttrLiteral::String("another_string".into())),
            "some_deps" => ConfiguredAttr::from_literal(AttrLiteral::List(Default::default(), AttrType::dep(Vec::new()))),
        ];

        let node = computations.get_target_node(&label1).await?;
        assert_eq!(node.0, node1.0);

        let node = computations.get_target_node(&label2).await?;
        assert_eq!(node.0, node2.0);

        let node = computations
            .get_configured_target_node(&label1.configure(cfg.dupe()))
            .await?;
        let node = node.require_compatible()?;
        let node_attrs: SmallMap<_, _> = node.attrs(AttrInspectOptions::All).collect();
        assert_eq!(node_attrs, conf_attrs1);

        let node = computations
            .get_configured_target_node(&label2.configure(cfg.dupe()))
            .await?;
        let node = node.require_compatible()?;
        let node_attrs: SmallMap<_, _> = node.attrs(AttrInspectOptions::All).collect();
        assert_eq!(node_attrs, conf_attrs2);

        Ok(())
    }
}
