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
use std::hash::Hasher;
use std::iter;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::buck_path::BuckPathRef;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::collections::ordered_map::OrderedMap;
use buck2_core::collections::unordered_map::UnorderedMap;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::configuration::Configuration;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use either::Either;
use once_cell::sync::Lazy;
use starlark_map::Hashed;

use crate::attrs::attr::Attribute;
use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dep::DepAttrTransition;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::attr_type::query::ResolvedQueryLiterals;
use crate::attrs::attr_type::AttrType;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::coerced_attr_full::CoercedAttrFull;
use crate::attrs::configuration_context::AttrConfigurationContextImpl;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_attr_full::ConfiguredAttrFull;
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
use crate::provider_id_set::ProviderIdSet;
use crate::rule_type::RuleType;

/// ConfiguredTargetNode contains the information for a target in a particular configuration.
///
/// Most information (like attribute values) is constructed when requested and not stored
/// in the node, instead the node just stores the base TargetNode and a configuration for
/// resolving the attributes. This saves memory, but users should try avoid repeatedly
/// requesting the same information.
#[derive(Debug, Clone, Dupe, Eq, PartialEq, Hash, Allocative)]
pub struct ConfiguredTargetNode(Arc<Hashed<ConfiguredTargetNodeData>>);

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
enum TargetNodeOrForward {
    TargetNode(TargetNode),
    // Coerced attr is always a dependency.
    Forward(CoercedAttr, ConfiguredTargetNode),
}

const ACTUAL_ATTR_NAME: &str = "actual";

impl TargetNodeOrForward {
    fn attrs<'a>(
        &'a self,
        opts: AttrInspectOptions,
    ) -> impl Iterator<Item = CoercedAttrFull<'a>> + 'a {
        match self {
            TargetNodeOrForward::TargetNode(target_node) => Either::Left(target_node.attrs(opts)),
            TargetNodeOrForward::Forward(actual, _) => {
                let actual_attr = Some(CoercedAttrFull {
                    name: ACTUAL_ATTR_NAME,
                    attr: ConfiguredTargetNode::actual_attribute(),
                    value: actual,
                });
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

    fn is_visible_to(&self, target: &TargetLabel) -> anyhow::Result<bool> {
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

    fn attr_or_none<'a>(
        &'a self,
        name: &str,
        opts: AttrInspectOptions,
    ) -> Option<CoercedAttrFull<'a>> {
        match self {
            TargetNodeOrForward::TargetNode(target_node) => target_node.attr_or_none(name, opts),
            TargetNodeOrForward::Forward(actual, _) => {
                if name == ACTUAL_ATTR_NAME {
                    Some(CoercedAttrFull {
                        name: ACTUAL_ATTR_NAME,
                        attr: ConfiguredTargetNode::actual_attribute(),
                        value: actual,
                    })
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
#[derive(Eq, PartialEq, Hash, Allocative)]
struct ConfiguredTargetNodeData {
    label: ConfiguredTargetLabel,
    target_node: TargetNodeOrForward,
    resolved_configuration: ResolvedConfiguration,
    resolved_transition_configurations: OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
    execution_platform_resolution: ExecutionPlatformResolution,
    // Deps includes regular deps and transitioned deps,
    // but excludes exec deps or configuration deps.
    // TODO(cjhopman): Should this be a diff against the node's deps?
    deps: ConfiguredTargetNodeDeps,
    exec_deps: ConfiguredTargetNodeDeps,
    platform_cfgs: OrderedMap<TargetLabel, Configuration>,
}

impl Debug for ConfiguredTargetNodeData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConfiguredTargetNodeData")
            .field("label", &self.label)
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
            ResolvedConfiguration::new(name.cfg().dupe(), UnorderedMap::new()),
            OrderedMap::new(),
            execution_platform_resolution,
            Vec::new(),
            Vec::new(),
            OrderedMap::new(),
        )
    }

    pub fn new(
        name: ConfiguredTargetLabel,
        target_node: TargetNode,
        resolved_configuration: ResolvedConfiguration,
        resolved_tr_configurations: OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
        execution_platform_resolution: ExecutionPlatformResolution,
        deps: Vec<ConfiguredTargetNode>,
        exec_deps: Vec<ConfiguredTargetNode>,
        platform_cfgs: OrderedMap<TargetLabel, Configuration>,
    ) -> Self {
        Self(Arc::new(Hashed::new(ConfiguredTargetNodeData {
            label: name,
            target_node: TargetNodeOrForward::TargetNode(target_node),
            resolved_configuration,
            resolved_transition_configurations: resolved_tr_configurations,
            execution_platform_resolution,
            deps: ConfiguredTargetNodeDeps(deps.into_boxed_slice()),
            exec_deps: ConfiguredTargetNodeDeps(exec_deps.into_boxed_slice()),
            platform_cfgs,
        })))
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
            transitioned_node.label().unconfigured(),
            "Forward node can only be created for the same unconfigured node"
        );
        assert_ne!(
            &name,
            transitioned_node.label(),
            "Creating a forward node to identical target label would be a mistake"
        );

        // TODO(nga): how to obtain the correct providers name?
        let providers_name = ProvidersName::Default;
        let providers_label = ProvidersLabel::new(name.unconfigured().dupe(), providers_name);

        let configured_providers_label =
            providers_label.configure(transitioned_node.label().cfg().dupe());
        Self(Arc::new(Hashed::new(ConfiguredTargetNodeData {
            label: name.dupe(),
            target_node: TargetNodeOrForward::Forward(
                CoercedAttr::Literal(AttrLiteral::ConfiguredDep(box DepAttr {
                    attr_type: DepAttrType::new(ProviderIdSet::EMPTY, DepAttrTransition::Identity),
                    label: configured_providers_label,
                })),
                transitioned_node.dupe(),
            ),
            // We have no attributes with selects, so resolved configurations is empty.
            resolved_configuration: ResolvedConfiguration::new(
                name.cfg().dupe(),
                UnorderedMap::new(),
            ),
            // We have no attributes to transition, so empty map is fine.
            resolved_transition_configurations: OrderedMap::new(),
            // Nothing to execute for a forward node.
            execution_platform_resolution: ExecutionPlatformResolution::unspecified(),
            deps: ConfiguredTargetNodeDeps(box [transitioned_node]),
            exec_deps: ConfiguredTargetNodeDeps(box []),
            platform_cfgs: OrderedMap::new(),
        })))
    }

    pub(crate) fn actual_attribute() -> &'static Attribute {
        static ATTRIBUTE: Lazy<Attribute> =
            Lazy::new(|| Attribute::new(None, "", AttrType::configured_dep(ProviderIdSet::EMPTY)));
        &ATTRIBUTE
    }

    pub fn target_compatible_with(&self) -> impl Iterator<Item = anyhow::Result<TargetLabel>> + '_ {
        self.get(
            TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD,
            AttrInspectOptions::All,
        )
        .into_iter()
        .flat_map(|a| {
            Self::attr_as_target_compatible_with(a.value).map(|a| {
                a.with_context(|| format!("attribute `{}`", TARGET_COMPATIBLE_WITH_ATTRIBUTE_FIELD))
            })
        })
    }

    pub fn attr_as_target_compatible_with(
        attr: ConfiguredAttr,
    ) -> impl Iterator<Item = anyhow::Result<TargetLabel>> {
        let list = match attr.try_into_list() {
            Ok(list) => list,
            Err(e) => return Either::Left(iter::once(Err(e))),
        };
        Either::Right(list.into_iter().map(|val| val.try_into_configuration_dep()))
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

            fn input(&mut self, path: BuckPathRef) -> anyhow::Result<()> {
                self.inputs.push(path.to_cell_path());
                Ok(())
            }
        }
        let mut traversal = InputsCollector { inputs: Vec::new() };
        for a in self.attrs(AttrInspectOptions::All) {
            a.traverse(self.label().pkg(), &mut traversal)
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
        for a in self.attrs(AttrInspectOptions::All) {
            a.traverse(self.label().pkg(), &mut traversal).unwrap();
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
            tests.traverse(self.label().pkg(), &mut traversal).unwrap();
        }
        traversal.labels.into_iter()
    }

    pub fn label(&self) -> &ConfiguredTargetLabel {
        &self.0.label
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

    pub fn is_visible_to(&self, target: &TargetLabel) -> anyhow::Result<bool> {
        self.0.target_node.is_visible_to(target)
    }

    pub fn special_attrs(&self) -> impl Iterator<Item = (&str, ConfiguredAttr)> {
        let typ_attr = ConfiguredAttr::new(AttrLiteral::String(self.rule_type().name().into()));
        let deps_attr = ConfiguredAttr::new(AttrLiteral::List(
            self.deps()
                .map(|t| {
                    ConfiguredAttr(AttrLiteral::Label(box ConfiguredProvidersLabel::new(
                        t.label().dupe(),
                        ProvidersName::Default,
                    )))
                })
                .collect::<Vec<_>>()
                .into(),
        ));
        let package_attr = ConfiguredAttr::new(AttrLiteral::String(ArcStr::from(
            self.buildfile_path().to_string(),
        )));
        vec![
            (TYPE, typ_attr),
            (DEPS, deps_attr),
            (PACKAGE, package_attr),
            (
                ONCALL,
                ConfiguredAttr::new(match self.oncall() {
                    None => AttrLiteral::None,
                    Some(x) => AttrLiteral::String(ArcStr::from(x)),
                }),
            ),
            (
                TARGET_CONFIGURATION,
                ConfiguredAttr::new(AttrLiteral::String(ArcStr::from(
                    self.0.label.cfg().to_string(),
                ))),
            ),
            (
                EXECUTION_PLATFORM,
                ConfiguredAttr::new(AttrLiteral::String(
                    self.0
                        .execution_platform_resolution
                        .platform()
                        .map_or_else(|_| ArcStr::from("<NONE>"), |v| ArcStr::from(v.id())),
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
    ) -> impl Iterator<Item = ConfiguredAttrFull<'a>> + 'a {
        self.0.target_node.attrs(opts).map(move |a| {
            a.configure(&AttrConfigurationContextImpl {
                resolved_cfg: &self.0.resolved_configuration,
                exec_cfg: &self.0.execution_platform_resolution.cfg(),
                resolved_transitions: &self.0.resolved_transition_configurations,
                platform_cfgs: &self.0.platform_cfgs,
            })
            .expect("checked attr configuration in constructor")
        })
    }

    pub fn get<'a>(
        &'a self,
        attr: &str,
        opts: AttrInspectOptions,
    ) -> Option<ConfiguredAttrFull<'a>> {
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

    /// Hash the fields that impact how this target is built.
    /// Don't do any recursive hashing of the dependencies.
    /// Hashes the attributes _after_ configuration, so changing unconfigured branches that
    /// are not taken will not change the hash.
    pub fn target_hash<H: Hasher>(&self, state: &mut H) {
        self.label().hash(state);
        self.rule_type().hash(state);
        self.attrs(AttrInspectOptions::All).for_each(|x| {
            // We deliberately don't hash the attribute, as if the value being passed to analysis
            // stays the same, we don't care if the attribute that generated it changed.
            x.name.hash(state);
            x.value.hash(state);
        });
    }
}

/// The representation of the deps for a ConfiguredTargetNode. Provides the operations we require
/// (iteration, eq, and hash), but guarantees those aren't recursive of the dep nodes' data.
#[derive(Allocative)]
struct ConfiguredTargetNodeDeps(Box<[ConfiguredTargetNode]>);

impl ConfiguredTargetNodeDeps {
    fn iter(&self) -> impl ExactSizeIterator<Item = &ConfiguredTargetNode> {
        self.0.iter()
    }
}

/// We only do Arc comparisons here. The rationale is that each ConfiguredTargetNode should only
/// ever exist in one instance in the graph, so if the ptr eq doesn't match, then we don't do a
/// deep comparison.
impl PartialEq for ConfiguredTargetNodeDeps {
    fn eq(&self, other: &Self) -> bool {
        let it1 = self.iter();
        let it2 = other.iter();
        it1.len() == it2.len() && it1.zip(it2).all(|(x, y)| Arc::ptr_eq(&x.0, &y.0))
    }
}

impl Eq for ConfiguredTargetNodeDeps {}

/// This has historically only hashed the labels. This may or may not be right, because it means
/// two nodes that reference different instances of a given dependency will hash equal, even if
/// the dependency has definitely changed (e.g. because its own deps changed).
impl Hash for ConfiguredTargetNodeDeps {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let it = self.0.iter();
        state.write_usize(it.len());
        for node in it {
            node.label().hash(state);
        }
    }
}
