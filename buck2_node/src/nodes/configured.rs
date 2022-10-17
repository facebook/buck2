/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Formatter;
use std::hash::Hash;
use std::sync::Arc;

use buck2_common::ordered_map::OrderedMap;
use buck2_core::buck_path::BuckPath;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::configuration::Configuration;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_query::query::syntax::simple::eval::label_indexed::LabelIndexedSet;
use either::Either;
use gazebo::dupe::Dupe;
use indexmap::IndexMap;

use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dep::DepAttrTransition;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::attr_type::dep::ProviderIdSet;
use crate::attrs::attr_type::query::ResolvedQueryLiterals;
use crate::attrs::attr_type::AttrType;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::configuration_context::AttrConfigurationContextImpl;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::attrs::internal::TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD;
use crate::attrs::internal::TESTS_ATTRIBUTE_FIELD;
use crate::configuration::execution::ExecutionPlatformResolution;
use crate::configuration::resolved::ResolvedConfiguration;
use crate::nodes::attributes::DEPS;
use crate::nodes::attributes::EXECUTION_PLATFORM;
use crate::nodes::attributes::ONCALL;
use crate::nodes::attributes::PACKAGE;
use crate::nodes::attributes::TARGET_CONFIGURATION;
use crate::nodes::attributes::TYPE;
use crate::nodes::unconfigured::RuleKind;
use crate::nodes::unconfigured::TargetNode;
use crate::rule_type::RuleType;

/// ConfiguredTargetNode contains the information for a target in a particular configuration.
///
/// Most information (like attribute values) is constructed when requested and not stored
/// in the node, instead the node just stores the base TargetNode and a configuration for
/// resolving the attributes. This saves memory, but users should try avoid repeatedly
/// requesting the same information.
#[derive(Debug, Clone, Dupe, Eq, PartialEq, Hash)]
pub struct ConfiguredTargetNode(Arc<ConfiguredTargetNodeData>);

#[derive(Debug, Eq, PartialEq, Hash)]
enum TargetNodeOrForward {
    TargetNode(TargetNode),
    // Coerced attr is always a dependency.
    Forward(CoercedAttr, ConfiguredTargetNode),
}

const ACTUAL_ATTR_NAME: &str = "actual";

impl TargetNodeOrForward {
    fn attrs(&self, opts: AttrInspectOptions) -> impl Iterator<Item = (&str, &CoercedAttr)> {
        match self {
            TargetNodeOrForward::TargetNode(target_node) => Either::Left(target_node.attrs(opts)),
            TargetNodeOrForward::Forward(actual, _) => {
                let actual_attr = if opts.include_defined() {
                    Some((ACTUAL_ATTR_NAME, actual))
                } else {
                    None
                };
                Either::Right(actual_attr.into_iter())
            }
        }
    }

    fn rule_type(&self) -> &RuleType {
        match self {
            TargetNodeOrForward::TargetNode(target_node) => target_node.rule_type(),
            TargetNodeOrForward::Forward(..) => &RuleType::Forward,
        }
    }

    fn rule_kind(&self) -> RuleKind {
        match self {
            TargetNodeOrForward::TargetNode(x) => x.rule_kind(),
            TargetNodeOrForward::Forward(_, x) => x.rule_kind(),
        }
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        match self {
            TargetNodeOrForward::TargetNode(target_node) => target_node.buildfile_path(),
            TargetNodeOrForward::Forward(_, x) => x.buildfile_path(),
        }
    }

    fn is_visible_to(&self, target: &TargetLabel) -> bool {
        match self {
            TargetNodeOrForward::TargetNode(node) => node.is_visible_to(target),
            TargetNodeOrForward::Forward(_, forward) => forward.is_visible_to(target),
        }
    }

    fn oncall(&self) -> Option<&str> {
        match self {
            TargetNodeOrForward::TargetNode(node) => node.oncall(),
            TargetNodeOrForward::Forward(_, forward) => forward.oncall(),
        }
    }

    fn attr_or_none(&self, name: &str, opts: AttrInspectOptions) -> Option<&CoercedAttr> {
        match self {
            TargetNodeOrForward::TargetNode(target_node) => target_node.attr_or_none(name, opts),
            TargetNodeOrForward::Forward(actual, _) => {
                if name == ACTUAL_ATTR_NAME && opts.include_defined() {
                    Some(actual)
                } else {
                    None
                }
            }
        }
    }
}

// TODO(cjhopman): There's a lot of optimization opportunities here.
//  1. we iterate over and configure the attributes multiple times, that could be improved in a bunch of ways
//  2. we store the same resolvedconfiguration probably in a bunch of nodes, that could be made smaller or shared
//  3. deps could probably be approximated a diff against the targetnode's deps
#[derive(Eq, PartialEq, Hash)]
struct ConfiguredTargetNodeData {
    name: ConfiguredTargetLabel,
    target_node: TargetNodeOrForward,
    resolved_configuration: ResolvedConfiguration,
    resolved_transition_configurations: OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
    execution_platform_resolution: ExecutionPlatformResolution,
    // Deps includes regular deps and transitioned deps,
    // but excludes exec deps or configuration deps.
    // TODO(cjhopman): Should this be a diff against the node's deps?
    // TODO(nga): this hashes incorrectly: if order of dependencies change,
    //   hash remains unchanged, but it should be different because
    //   there are places where we rely on iteration order
    //   (for example, in deps query output, which is observable by user).
    deps: LabelIndexedSet<ConfiguredTargetNode>,
    exec_deps: LabelIndexedSet<ConfiguredTargetNode>,
    platform_cfgs: OrderedMap<TargetLabel, Configuration>,
}

impl Debug for ConfiguredTargetNodeData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConfiguredTargetNodeData")
            .field("name", &self.name)
            .finish_non_exhaustive()
    }
}

/// The ConfiguredAttr and other derived information about it.
#[derive(Debug, Eq, PartialEq, Hash)]
struct ConfiguredNodeAttr {
    attr: ConfiguredAttr,
    has_queries: bool,
}

impl ConfiguredTargetNode {
    /// Creates a minimal ConfiguredTargetNode. Some operations may unexpectedly fail.
    pub fn testing_new(
        name: ConfiguredTargetLabel,
        rule_type: RuleType,
        attrs: Vec<(&str, crate::attrs::attr::Attribute, CoercedAttr)>,
        execution_platform_resolution: ExecutionPlatformResolution,
    ) -> Self {
        use crate::nodes::unconfigured::testing::TargetNodeExt;
        Self::new(
            name.dupe(),
            TargetNode::testing_new(name.unconfigured().dupe(), rule_type, attrs),
            ResolvedConfiguration::new(name.cfg().dupe(), IndexMap::new()),
            OrderedMap::new(),
            execution_platform_resolution,
            LabelIndexedSet::new(),
            LabelIndexedSet::new(),
            OrderedMap::new(),
        )
    }

    pub fn new(
        name: ConfiguredTargetLabel,
        target_node: TargetNode,
        resolved_configuration: ResolvedConfiguration,
        resolved_tr_configurations: OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
        execution_platform_resolution: ExecutionPlatformResolution,
        deps: LabelIndexedSet<ConfiguredTargetNode>,
        exec_deps: LabelIndexedSet<ConfiguredTargetNode>,
        platform_cfgs: OrderedMap<TargetLabel, Configuration>,
    ) -> Self {
        Self(Arc::new(ConfiguredTargetNodeData {
            name,
            target_node: TargetNodeOrForward::TargetNode(target_node),
            resolved_configuration,
            resolved_transition_configurations: resolved_tr_configurations,
            execution_platform_resolution,
            deps,
            exec_deps,
            platform_cfgs,
        }))
    }

    /// New `ConfiguredTargetNode` for a forward node for transitioned target.
    pub fn new_forward(
        // Forward node to create.
        name: ConfiguredTargetLabel,
        // The transitioned target node.
        transitioned_node: ConfiguredTargetNode,
    ) -> Self {
        assert_eq!(
            name.unconfigured(),
            transitioned_node.name().unconfigured(),
            "Forward node can only be created for the same unconfigured node"
        );
        assert_ne!(
            &name,
            transitioned_node.name(),
            "Creating a forward node to identical target label would be a mistake"
        );

        // TODO(nga): how to obtain the correct providers name?
        let providers_name = ProvidersName::Default;
        let providers_label = ProvidersLabel::new(name.unconfigured().dupe(), providers_name);

        let configured_providers_label =
            providers_label.configure(transitioned_node.name().cfg().dupe());
        Self(Arc::new(ConfiguredTargetNodeData {
            name: name.dupe(),
            target_node: TargetNodeOrForward::Forward(
                CoercedAttr::Literal(AttrLiteral::ConfiguredDep(box DepAttr::new(
                    DepAttrType::new(ProviderIdSet::new(), DepAttrTransition::Identity),
                    configured_providers_label,
                ))),
                transitioned_node.dupe(),
            ),
            // We have no attributes with selects, so resolved configurations is empty.
            resolved_configuration: ResolvedConfiguration::new(name.cfg().dupe(), IndexMap::new()),
            // We have no attributes to transition, so empty map is fine.
            resolved_transition_configurations: OrderedMap::new(),
            // Nothing to execute for a forward node.
            execution_platform_resolution: ExecutionPlatformResolution::unspecified(),
            deps: LabelIndexedSet::from_iter([transitioned_node]),
            exec_deps: LabelIndexedSet::new(),
            platform_cfgs: OrderedMap::new(),
        }))
    }

    pub fn target_compatible_with(&self) -> impl Iterator<Item = TargetLabel> {
        self.get(
            TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
            AttrInspectOptions::All,
        )
        .into_iter()
        .flat_map(Self::attr_as_target_compatible_with)
    }

    pub fn attr_as_target_compatible_with(
        attr: ConfiguredAttr,
    ) -> impl Iterator<Item = TargetLabel> {
        attr.try_into_list().into_iter().flat_map(|v| {
            v.into_iter().map(|val| {
                val.try_into_configuration_dep()
                    .expect("target_compatible_with should be list<dep>")
            })
        })
    }

    pub fn execution_platform_resolution(&self) -> &ExecutionPlatformResolution {
        &self.0.execution_platform_resolution
    }

    /// Returns all deps for this node that we know about after processing the build file
    /// (it may be missing things like toolchain deps or other things that are determined
    /// later in the build process).
    // TODO(cjhopman): Should this include configuration deps? Should it include the configuration deps that were inspected resolving selects?
    pub fn deps(&self) -> impl Iterator<Item = &ConfiguredTargetNode> {
        self.0.deps.iter().chain(self.0.exec_deps.iter())
    }

    pub fn toolchain_deps(&self) -> impl Iterator<Item = &ConfiguredTargetNode> {
        // Since we validate that all toolchain dependencies are of kind Toolchain,
        // we can use that to filter the deps.
        self.0
            .deps
            .iter()
            .filter(|x| x.rule_kind() == RuleKind::Toolchain)
    }

    pub fn inputs(&self) -> impl Iterator<Item = CellPath> + '_ {
        struct InputsCollector {
            inputs: Vec<CellPath>,
        }
        impl<'a> ConfiguredAttrTraversal<'a> for InputsCollector {
            fn dep(&mut self, _dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn input(&mut self, path: &'a BuckPath) -> anyhow::Result<()> {
                self.inputs.push(path.to_cell_path());
                Ok(())
            }
        }
        let mut traversal = InputsCollector { inputs: Vec::new() };
        for (_, attr) in self.attrs(AttrInspectOptions::All) {
            attr.traverse(&mut traversal)
                .expect("inputs collector shouldn't return errors");
        }
        traversal.inputs.into_iter()
    }

    // TODO(cjhopman): switch to for_each_query?
    pub fn queries(&self) -> impl Iterator<Item = (String, ResolvedQueryLiterals<ConfiguredAttr>)> {
        struct Traversal {
            queries: Vec<(String, ResolvedQueryLiterals<ConfiguredAttr>)>,
        }
        let mut traversal = Traversal {
            queries: Vec::new(),
        };
        impl<'attr> ConfiguredAttrTraversal<'attr> for Traversal {
            fn dep(&mut self, _dep: &'attr ConfiguredProvidersLabel) -> anyhow::Result<()> {
                // ignored.
                Ok(())
            }

            fn query_macro(
                &mut self,
                query: &'attr str,
                resolved_literals: &'attr ResolvedQueryLiterals<ConfiguredAttr>,
            ) -> anyhow::Result<()> {
                self.queries
                    .push((query.to_owned(), resolved_literals.clone()));
                Ok(())
            }
        }
        // TODO(cjhopman): optimize for non-query attrs
        for (_, attr) in self.attrs(AttrInspectOptions::All) {
            attr.traverse(&mut traversal).unwrap();
        }
        traversal.queries.into_iter()
    }

    pub fn target_deps(&self) -> impl Iterator<Item = &ConfiguredTargetNode> {
        self.0.deps.iter()
    }

    pub fn exec_deps(&self) -> impl Iterator<Item = &ConfiguredTargetNode> {
        self.0.exec_deps.iter()
    }

    /// Return the `tests` declared for this target.
    pub fn tests(&self) -> impl Iterator<Item = ConfiguredProvidersLabel> {
        #[derive(Default)]
        struct TestCollector {
            labels: Vec<ConfiguredProvidersLabel>,
        }

        impl<'attr> ConfiguredAttrTraversal<'attr> for TestCollector {
            fn dep(&mut self, _dep: &'attr ConfiguredProvidersLabel) -> anyhow::Result<()> {
                // ignored.
                Ok(())
            }

            fn label(&mut self, label: &'attr ConfiguredProvidersLabel) -> anyhow::Result<()> {
                self.labels.push(label.clone());
                Ok(())
            }
        }

        let mut traversal = TestCollector::default();
        if let Some(tests) = self.get(TESTS_ATTRIBUTE_FIELD, AttrInspectOptions::All) {
            tests.traverse(&mut traversal).unwrap();
        }
        traversal.labels.into_iter()
    }

    pub fn name(&self) -> &ConfiguredTargetLabel {
        &self.0.name
    }

    pub fn rule_type(&self) -> &RuleType {
        self.0.target_node.rule_type()
    }

    pub fn rule_kind(&self) -> RuleKind {
        self.0.target_node.rule_kind()
    }

    pub fn buildfile_path(&self) -> &BuildFilePath {
        self.0.target_node.buildfile_path()
    }

    pub fn is_visible_to(&self, target: &TargetLabel) -> bool {
        self.0.target_node.is_visible_to(target)
    }

    pub fn special_attrs(&self) -> impl Iterator<Item = (&str, ConfiguredAttr)> {
        let typ_attr = ConfiguredAttr::new(AttrLiteral::String(self.rule_type().name().to_owned()));
        let deps_attr = ConfiguredAttr::new(AttrLiteral::List(
            self.deps()
                .map(|t| {
                    ConfiguredAttr(AttrLiteral::Label(box ConfiguredProvidersLabel::new(
                        t.name().dupe(),
                        ProvidersName::Default,
                    )))
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            AttrType::dep(Vec::new()),
        ));
        let package_attr =
            ConfiguredAttr::new(AttrLiteral::String(self.buildfile_path().to_string()));
        vec![
            (TYPE, typ_attr),
            (DEPS, deps_attr),
            (PACKAGE, package_attr),
            (
                ONCALL,
                ConfiguredAttr::new(match self.oncall() {
                    None => AttrLiteral::None,
                    Some(x) => AttrLiteral::String(x.to_owned()),
                }),
            ),
            (
                TARGET_CONFIGURATION,
                ConfiguredAttr::new(AttrLiteral::String(self.0.name.cfg().to_string())),
            ),
            (
                EXECUTION_PLATFORM,
                ConfiguredAttr::new(AttrLiteral::String(
                    self.0
                        .execution_platform_resolution
                        .platform()
                        .map_or("<NONE>".to_owned(), |v| v.id()),
                )),
            ),
        ]
        .into_iter()
    }

    pub fn oncall(&self) -> Option<&str> {
        self.0.target_node.oncall()
    }

    pub fn attrs<'a>(
        &'a self,
        opts: AttrInspectOptions,
    ) -> impl Iterator<Item = (&str, ConfiguredAttr)> + 'a {
        self.0.target_node.attrs(opts).map(move |(name, attr)| {
            (
                name,
                attr.configure(&AttrConfigurationContextImpl {
                    resolved_cfg: &self.0.resolved_configuration,
                    exec_cfg: &self.0.execution_platform_resolution.cfg(),
                    resolved_transitions: &self.0.resolved_transition_configurations,
                    platform_cfgs: &self.0.platform_cfgs,
                })
                .expect("checked attr configuration in constructor"),
            )
        })
    }

    pub fn get(&self, attr: &str, opts: AttrInspectOptions) -> Option<ConfiguredAttr> {
        self.0.target_node.attr_or_none(attr, opts).map(|v| {
            v.configure(&AttrConfigurationContextImpl {
                resolved_cfg: &self.0.resolved_configuration,
                exec_cfg: &self.0.execution_platform_resolution.cfg(),
                resolved_transitions: &self.0.resolved_transition_configurations,
                platform_cfgs: &self.0.platform_cfgs,
            })
            .expect("checked attr configuration in constructor")
        })
    }

    pub fn call_stack(&self) -> Option<String> {
        match &self.0.target_node {
            TargetNodeOrForward::TargetNode(n) => n.call_stack(),
            TargetNodeOrForward::Forward(_, n) => n.call_stack(),
        }
    }
}
