/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Calculations relating to 'TargetNode's that runs on Dice

use std::collections::BTreeMap;
use std::sync::Arc;

use anyhow::anyhow;
use anyhow::Context;
use async_trait::async_trait;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::configuration::Configuration;
use buck2_core::configuration::ConfigurationData;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::result::SharedResult;
use buck2_core::result::ToSharedResultExt;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use gazebo::prelude::*;
use indexmap::IndexMap;
use indexmap::IndexSet;
use itertools::Either;
use itertools::Itertools;
use starlark::collections::SmallSet;

use crate::attrs::attr_type::attr_literal::ConfiguredAttrTraversal;
use crate::attrs::configured_attr::ConfiguredAttrExt;
use crate::calculation::BuildErrors;
use crate::configuration::execution::ExecutionPlatform;
use crate::configuration::execution::ExecutionPlatformResolution;
use crate::configuration::AttrConfigurationContextImpl;
use crate::configuration::ConfigurationCalculation;
use crate::configuration::ConfigurationSettingKeyRef;
use crate::configuration::ResolvedConfiguration;
use crate::execute::commands::dice_data::HasFallbackExecutorConfig;
use crate::interpreter::calculation::InterpreterCalculation;
use crate::interpreter::rule_defs::transition::calculation_apply_transition::ApplyTransition;
use crate::nodes::attr_internal::EXEC_COMPATIBLE_WITH_ATTRIBUTE_FIELD;
use crate::nodes::attr_internal::LEGACY_TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD;
use crate::nodes::attr_internal::TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD;
use crate::nodes::compatibility::IncompatiblePlatformReason;
use crate::nodes::compatibility::MaybeCompatible;
use crate::nodes::configured::ConfiguredTargetNode;
use crate::nodes::unconfigured::TargetNode;
use crate::nodes::visibility::VisibilityError;
use crate::nodes::AttributeError;

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
) -> anyhow::Result<BTreeMap<TargetLabel, Configuration>> {
    let mut platform_map = BTreeMap::new();
    for platform_target in node.platform_deps() {
        let config = ctx.get_platform_configuration(platform_target).await?;
        platform_map.insert(platform_target.dupe(), config);
    }

    Ok(platform_map)
}

async fn resolve_execution_platform(
    ctx: &DiceComputations,
    node: &TargetNode,
    resolved_configuration: &ResolvedConfiguration,
    resolved_transitions: &IndexMap<Arc<TransitionId>, Arc<TransitionApplied>>,
) -> SharedResult<ExecutionPlatformResolution> {
    // If no execution platforms are configured, we fall back to the legacy execution
    // platform behavior. We currently only support legacy execution platforms. That behavior is that there is a
    // single executor config (the fallback config) and the execution platform is in the same
    // configuration as the target.
    // The non-none case will be handled when we invoke the resolve_execution_platform() on ctx below, the none
    // case can't be handled there because we don't pass the full configuration into it.
    if ctx.get_execution_platforms().await?.is_none() {
        let execution_platform = Arc::new(ExecutionPlatform::LegacyExecutionPlatform {
            executor_config: ctx.get_fallback_executor_config().clone(),
            cfg: resolved_configuration.cfg().dupe(),
        });
        return Ok(ExecutionPlatformResolution::new(
            Some(execution_platform),
            Vec::new(),
        ));
    };

    let mut exec_compatible_with = Vec::new();
    let mut exec_deps = IndexSet::new();
    let unbound_exe_configuration = Configuration::unbound_exec();

    struct Traversal<'a> {
        exec_deps: &'a mut IndexSet<TargetLabel>,
    }
    impl<'a> ConfiguredAttrTraversal<'a> for Traversal<'_> {
        fn dep(&mut self, _dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
            Ok(())
        }

        fn exec_dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
            // TODO(cjhopman): Check that the dep is in the unbound_exe configuration
            self.exec_deps.insert(dep.target().unconfigured().dupe());
            Ok(())
        }
    }
    let mut traversal = Traversal {
        exec_deps: &mut exec_deps,
    };

    let platform_cfgs = &compute_platform_cfgs(ctx, node).await?;

    for (name, attr) in node.attrs() {
        let configured_attr = attr
            .configure(&AttrConfigurationContextImpl {
                resolved_cfg: resolved_configuration,
                exec_cfg: &unbound_exe_configuration,
                // We don't really need `resolved_transitions` here:
                // `Traversal` declared above ignores transitioned dependencies.
                // But we pass `resolved_transitions` here to prevent breakages in the future
                // if something here changes.
                resolved_transitions,
                platform_cfgs,
            })
            .with_context(|| {
                format!(
                    "when configuring attribute `{}` to resolve execution platform",
                    name
                )
            })?;
        configured_attr.traverse(&mut traversal)?;
        if name == EXEC_COMPATIBLE_WITH_ATTRIBUTE_FIELD {
            exec_compatible_with.extend(ConfiguredTargetNode::attr_as_target_compatible_with(
                configured_attr,
            ));
        }
    }

    ctx.resolve_execution_platform(
        node.label().pkg().cell_name(),
        exec_compatible_with,
        exec_deps,
    )
    .await
}

fn unpack_target_compatible_with_attr(
    target_node: &TargetNode,
    resolved_cfg: &ResolvedConfiguration,
    attr_name: &str,
) -> anyhow::Result<Option<ConfiguredAttr>> {
    let attr = target_node.attr_or_none(attr_name);
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

        fn resolved_transitions(&self) -> &IndexMap<Arc<TransitionId>, Arc<TransitionApplied>> {
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
        None => Err(AttributeError::TargetCompatibleNotList(
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
            return Err(AttributeError::BothTargetCompatibleWith(target_label.to_string()).into());
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
            root_incompatible_target: target_label.dupe(),
            unsatisfied_config: incompatible_target,
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

    let mut resolved_transitions = IndexMap::new();
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
    for (attr_name, attr) in target_node.attrs() {
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
    let dep_nodes: Vec<_> = deps
        .iter()
        .chain(exec_deps.iter())
        .map(|v| ctx.get_configured_target_node(v.target()))
        .collect();

    let dep_nodes: Vec<_> = futures::future::join_all(dep_nodes).await;

    for dep in dep_nodes {
        let dep = dep?;
        match dep {
            MaybeCompatible::Incompatible(reason) => {
                return Ok(MaybeCompatible::Incompatible(reason.dupe()));
            }
            MaybeCompatible::Compatible(dep) => {
                if !dep.is_visible_to(target_label.unconfigured()) {
                    return Err(anyhow::anyhow!(VisibilityError::NotVisibleTo(
                        dep.name().unconfigured().dupe(),
                        target_label.unconfigured().dupe(),
                    )))
                    .shared_error();
                }
            }
        }
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
    let target_node = ctx.get_target_node(key.0.unconfigured()).await?;

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
            .await
        }
    } else {
        // We are not caching `ConfiguredTransitionedNodeKey` because this is cheap,
        // and no need to fetch `target_node` again.
        compute_configured_target_node_no_transition(&key.0.dupe(), target_node, ctx).await
    }
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "ConfiguredNode({})", _0)]
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
            .await?
            .targets()
            .get(target.name())
            .ok_or_else(|| {
                anyhow!(BuildErrors::MissingTarget(
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

        self.compute(&ConfiguredNodeKey(target.dupe())).await
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::bzl::ImportPath;
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::paths::FileNameBuf;
    use buck2_core::package::testing::PackageExt;
    use buck2_core::package::Package;
    use buck2_core::provider::label::ProvidersLabel;
    use buck2_core::provider::label::ProvidersName;
    use buck2_core::result::SharedResult;
    use buck2_core::target::TargetLabel;
    use buck2_core::target::TargetName;
    use buck2_interpreter::common::BuildFilePath;
    use buck2_node::attrs::attr_type::any::AnyAttrType;
    use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
    use buck2_node::attrs::attr_type::dep::DepAttr;
    use buck2_node::attrs::attr_type::dep::DepAttrTransition;
    use buck2_node::attrs::attr_type::dep::DepAttrType;
    use buck2_node::attrs::attr_type::AttrType;
    use buck2_node::attrs::configured_attr::ConfiguredAttr;
    use dice::testing::DiceBuilder;
    use dice::UserComputationData;
    use gazebo::prelude::*;
    use indexmap::indexmap;
    use starlark::collections::SmallMap;

    use crate::attrs::attr_type::any::AnyAttrTypeExt;
    use crate::attrs::coerced_attr::CoercedAttr;
    use crate::attrs::testing::CoercedAttrExt;
    use crate::attrs::testing::ConfiguredAttrExt;
    use crate::configuration::calculation::ExecutionPlatformsKey;
    use crate::execute::commands::dice_data::set_fallback_executor_config;
    use crate::execute::CommandExecutorConfig;
    use crate::interpreter::calculation::testing::InterpreterResultsKey;
    use crate::interpreter::module_internals::EvaluationResult;
    use crate::interpreter::rule_defs::attr::testing::AttributeExt;
    use crate::interpreter::rule_defs::attr::Attribute;
    use crate::nodes::calculation::NodeCalculation;
    use crate::nodes::unconfigured::testing::TargetNodeExt;
    use crate::nodes::unconfigured::TargetNode;
    use crate::nodes::RuleType;
    use crate::nodes::StarlarkRuleType;

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
                FileNameBuf::unchecked_new("BUCK".to_owned()),
            )),
            Vec::new(),
            indexmap![name1 => node1.dupe(), name2 => node2.dupe()],
        );

        let mut data = UserComputationData::new();
        set_fallback_executor_config(&mut data.data, CommandExecutorConfig::testing_local());
        let mut computations = DiceBuilder::new()
            .mock_and_return(
                InterpreterResultsKey(pkg),
                SharedResult::Ok(Arc::new(eval_result)),
            )
            .mock_and_return(ExecutionPlatformsKey, SharedResult::Ok(None))
            .build(data);
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
        let node_attrs: SmallMap<_, _> = node.attrs().collect();
        assert_eq!(node_attrs, conf_attrs1);

        let node = computations
            .get_configured_target_node(&label2.configure(cfg.dupe()))
            .await?;
        let node = node.require_compatible()?;
        let node_attrs: SmallMap<_, _> = node.attrs().collect();
        assert_eq!(node_attrs, conf_attrs2);

        Ok(())
    }
}
