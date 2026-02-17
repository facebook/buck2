/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::iter;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::execution_types::execution::ExecutionPlatform;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::package::source_path::SourcePathRef;
use buck2_core::plugins::PluginKind;
use buck2_core::plugins::PluginKindSet;
use buck2_core::plugins::PluginLists;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use either::Either;
use once_cell::sync::Lazy;
use starlark_map::Hashed;
use starlark_map::ordered_map::OrderedMap;

use crate::attrs::attr::Attribute;
use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dep::DepAttrTransition;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::attr_type::query::ResolvedQueryLiterals;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::coerced_attr_full::CoercedAttrFull;
use crate::attrs::configuration_context::AttrConfigurationContextImpl;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_attr_full::ConfiguredAttrFull;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::attrs::spec::internal::TESTS_ATTRIBUTE;
use crate::bzl_or_bxl_path::BzlOrBxlPath;
use crate::call_stack::StarlarkCallStack;
use crate::call_stack::StarlarkTargetCallStackRoot;
use crate::configuration::resolved::ConfigurationSettingKey;
use crate::configuration::resolved::MatchedConfigurationSettingKeys;
use crate::configuration::resolved::MatchedConfigurationSettingKeysWithCfg;
use crate::nodes::attributes::DEPS;
use crate::nodes::attributes::EXECUTION_PLATFORM;
use crate::nodes::attributes::ONCALL;
use crate::nodes::attributes::PACKAGE;
use crate::nodes::attributes::PLUGINS;
use crate::nodes::attributes::TARGET_CONFIGURATION;
use crate::nodes::attributes::TYPE;
use crate::nodes::unconfigured::RuleKind;
use crate::nodes::unconfigured::TargetNode;
use crate::provider_id_set::ProviderIdSet;
use crate::rule_type::RuleType;
use crate::rule_type::StarlarkRuleType;

/// ConfiguredTargetNode contains the information for a target in a particular configuration.
///
/// Most information (like attribute values) is constructed when requested and not stored
/// in the node, instead the node just stores the base TargetNode and a configuration for
/// resolving the attributes. This saves memory, but users should try avoid repeatedly
/// requesting the same information.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Allocative)]
pub struct ConfiguredTargetNode(triomphe::Arc<Hashed<ConfiguredTargetNodeData>>);

impl Dupe for ConfiguredTargetNode {}

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
enum TargetNodeOrForward {
    TargetNode(TargetNode),
    // Coerced attr is always a dependency.
    Forward(CoercedAttr, ConfiguredTargetNode),
}

const ACTUAL_ATTR_NAME: &str = "actual";

impl TargetNodeOrForward {
    fn attrs(&self, opts: AttrInspectOptions) -> impl Iterator<Item = CoercedAttrFull<'_>> + '_ {
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

    fn underlying_rule_type(&self) -> &RuleType {
        match self {
            TargetNodeOrForward::TargetNode(target_node) => target_node.rule_type(),
            TargetNodeOrForward::Forward(_, node) => node.underlying_rule_type(),
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

    fn is_visible_to(&self, target: &TargetLabel) -> buck2_error::Result<bool> {
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
    label: Hashed<ConfiguredTargetLabel>,
    target_node: TargetNodeOrForward,
    resolved_configuration: MatchedConfigurationSettingKeysWithCfg,
    resolved_transition_configurations: OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
    execution_platform_resolution: ExecutionPlatformResolution,
    // all_deps includes regular deps and transitioned deps,
    // and includes exec deps and configuration deps.
    // TODO(cjhopman): Should this be a diff against the node's deps?
    all_deps: ConfiguredTargetNodeDeps,
    platform_cfgs: OrderedMap<TargetLabel, ConfigurationData>,
    // TODO(JakobDegen): Consider saving some memory by using a more tset like representation of
    // the plugin lists
    plugin_lists: PluginLists,
}

impl Debug for ConfiguredTargetNodeData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConfiguredTargetNodeData")
            .field("label", &self.label)
            .finish_non_exhaustive()
    }
}

impl ConfiguredTargetNode {
    /// Creates a minimal ConfiguredTargetNode. Some operations may unexpectedly fail.
    pub fn testing_new(
        name: ConfiguredTargetLabel,
        rule_type: &str,
        execution_platform_resolution: ExecutionPlatformResolution,
        attrs: Vec<(&str, Attribute, CoercedAttr)>,
        call_stack: Option<StarlarkCallStack>,
    ) -> Self {
        use crate::nodes::unconfigured::testing::TargetNodeExt;

        let rule_type = RuleType::Starlark(Arc::new(StarlarkRuleType {
            path: BzlOrBxlPath::Bzl(ImportPath::testing_new("cell//pkg:rules.bzl")),
            name: rule_type.to_owned(),
        }));

        Self::new(
            name.dupe(),
            TargetNode::testing_new(name.unconfigured().dupe(), rule_type, attrs, call_stack),
            MatchedConfigurationSettingKeysWithCfg::new(
                ConfigurationNoExec::new(name.cfg().dupe()),
                MatchedConfigurationSettingKeys::empty(),
            ),
            OrderedMap::new(),
            execution_platform_resolution,
            Vec::new(),
            Vec::new(),
            OrderedMap::new(),
            PluginLists::new(),
        )
    }

    pub fn new(
        name: ConfiguredTargetLabel,
        target_node: TargetNode,
        resolved_configuration: MatchedConfigurationSettingKeysWithCfg,
        resolved_tr_configurations: OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
        execution_platform_resolution: ExecutionPlatformResolution,
        deps: Vec<ConfiguredTargetNode>,
        exec_deps: Vec<ConfiguredTargetNode>,
        platform_cfgs: OrderedMap<TargetLabel, ConfigurationData>,
        plugin_lists: PluginLists,
    ) -> Self {
        Self(triomphe::Arc::new(Hashed::new(ConfiguredTargetNodeData {
            label: Hashed::new(name),
            target_node: TargetNodeOrForward::TargetNode(target_node),
            resolved_configuration,
            resolved_transition_configurations: resolved_tr_configurations,
            execution_platform_resolution,
            all_deps: ConfiguredTargetNodeDeps::new(deps, exec_deps),
            platform_cfgs,
            plugin_lists,
        })))
    }

    /// New `ConfiguredTargetNode` for a forward node for transitioned target.
    pub fn new_forward(
        // Forward node to create.
        name: ConfiguredTargetLabel,
        // The transitioned target node.
        transitioned_node: ConfiguredTargetNode,
    ) -> buck2_error::Result<Self> {
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
        Ok(ConfiguredTargetNode(triomphe::Arc::new(Hashed::new(
            ConfiguredTargetNodeData {
                label: Hashed::new(name.dupe()),
                target_node: TargetNodeOrForward::Forward(
                    CoercedAttr::ConfiguredDepForForwardNode(Box::new(DepAttr {
                        attr_type: DepAttrType::new(
                            ProviderIdSet::EMPTY,
                            DepAttrTransition::Identity(PluginKindSet::EMPTY),
                        ),
                        label: configured_providers_label,
                    })),
                    transitioned_node.dupe(),
                ),
                // We have no attributes with selects, so resolved configurations is empty.
                resolved_configuration: MatchedConfigurationSettingKeysWithCfg::new(
                    name.cfg_pair().check_no_exec_cfg()?,
                    MatchedConfigurationSettingKeys::empty(),
                ),
                // We have no attributes to transition, so empty map is fine.
                resolved_transition_configurations: OrderedMap::new(),
                // Set the execution platform equal to the transitioned node's execution platform
                // so we can call `buck2 test` on Forward.
                execution_platform_resolution: transitioned_node
                    .execution_platform_resolution()
                    .dupe(),
                plugin_lists: transitioned_node.plugin_lists().clone(),
                all_deps: ConfiguredTargetNodeDeps::new(vec![transitioned_node], vec![]),
                platform_cfgs: OrderedMap::new(),
            },
        ))))
    }

    pub(crate) fn actual_attribute() -> &'static Attribute {
        static ATTRIBUTE: Lazy<Attribute> =
            Lazy::new(|| Attribute::new(None, "", AttrType::configured_dep(ProviderIdSet::EMPTY)));
        &ATTRIBUTE
    }

    pub fn attr_as_target_compatible_with(
        attr: ConfiguredAttr,
    ) -> impl Iterator<Item = buck2_error::Result<ConfigurationSettingKey>> {
        let list = match attr.try_into_list() {
            Ok(list) => list,
            Err(e) => return Either::Left(iter::once(Err(e))),
        };
        Either::Right(list.into_iter().map(|val| {
            val.try_into_configuration_dep()
                .map(ConfigurationSettingKey)
        }))
    }

    pub fn execution_platform_resolution(&self) -> &ExecutionPlatformResolution {
        &self.0.execution_platform_resolution
    }

    /// Returns all deps for this node:
    /// - target ("normal") deps
    /// - execution deps
    /// - toolchain deps
    /// - configuration deps
    // TODO(cjhopman): Should this include configuration deps? Should it include the configuration deps that were inspected resolving selects?
    pub fn deps(&self) -> impl Iterator<Item = &ConfiguredTargetNode> {
        self.0.all_deps.all_deps.iter()
    }

    pub fn configuration_deps(&self) -> impl Iterator<Item = &ConfiguredTargetNode> {
        // Since we validate that all configuration dependencies are of kind Configuration,
        // we can use that to filter the deps.
        self.0
            .all_deps
            .deps()
            .iter()
            .filter(|x| x.rule_kind() == RuleKind::Configuration)
    }

    pub fn toolchain_deps(&self) -> impl Iterator<Item = &ConfiguredTargetNode> {
        // Since we validate that all toolchain dependencies are of kind Toolchain,
        // we can use that to filter the deps.
        self.0
            .all_deps
            .deps()
            .iter()
            .filter(|x| x.rule_kind() == RuleKind::Toolchain)
    }

    pub fn inputs(&self) -> impl Iterator<Item = CellPath> + '_ {
        self.as_ref().inputs()
    }

    #[inline]
    pub fn queries(
        &self,
    ) -> impl Iterator<Item = (String, ResolvedQueryLiterals<ConfiguredProvidersLabel>)> + '_ {
        self.as_ref().queries()
    }

    pub fn target_deps(&self) -> impl Iterator<Item = &ConfiguredTargetNode> {
        self.0
            .all_deps
            .deps()
            .iter()
            .filter(|x| x.rule_kind() == RuleKind::Normal)
    }

    pub fn exec_deps(&self) -> impl Iterator<Item = &ConfiguredTargetNode> {
        self.0.all_deps.exec_deps().iter()
    }

    /// Return the `tests` declared for this target configured in same target platform as this target.
    pub fn tests(&self) -> impl Iterator<Item = ConfiguredProvidersLabel> + use<> {
        #[derive(Default)]
        struct TestCollector {
            labels: Vec<ConfiguredProvidersLabel>,
        }

        impl ConfiguredAttrTraversal for TestCollector {
            fn dep(&mut self, _dep: &ConfiguredProvidersLabel) -> buck2_error::Result<()> {
                // ignored.
                Ok(())
            }

            fn label(&mut self, label: &ConfiguredProvidersLabel) -> buck2_error::Result<()> {
                self.labels.push(label.dupe());
                Ok(())
            }
        }

        let mut traversal = TestCollector::default();
        if let Some(tests) = self.get(TESTS_ATTRIBUTE.name, AttrInspectOptions::All) {
            tests.traverse(self.label().pkg(), &mut traversal).unwrap();
        }
        traversal.labels.into_iter()
    }

    pub fn label(&self) -> &ConfiguredTargetLabel {
        &self.0.label
    }

    #[inline]
    pub fn hashed_label(&self) -> Hashed<&ConfiguredTargetLabel> {
        self.0.label.as_ref()
    }

    pub fn target_node(&self) -> &TargetNode {
        match &self.0.target_node {
            TargetNodeOrForward::TargetNode(n) => n,
            TargetNodeOrForward::Forward(_, n) => n.target_node(),
        }
    }

    pub fn rule_type(&self) -> &RuleType {
        self.0.target_node.rule_type()
    }

    pub fn underlying_rule_type(&self) -> &RuleType {
        self.0.target_node.underlying_rule_type()
    }

    pub fn rule_kind(&self) -> RuleKind {
        self.0.target_node.rule_kind()
    }

    pub fn buildfile_path(&self) -> &BuildFilePath {
        self.0.target_node.buildfile_path()
    }

    pub fn is_visible_to(&self, target: &TargetLabel) -> buck2_error::Result<bool> {
        self.0.target_node.is_visible_to(target)
    }

    #[inline]
    pub fn special_attr_or_none(&self, key: &str) -> Option<ConfiguredAttr> {
        self.as_ref().special_attr_or_none(key)
    }

    #[inline]
    pub fn special_attrs(&self) -> impl Iterator<Item = (&str, ConfiguredAttr)> {
        self.as_ref().special_attrs()
    }

    #[inline]
    pub fn oncall(&self) -> Option<&str> {
        self.as_ref().oncall()
    }

    pub fn attrs(
        &self,
        opts: AttrInspectOptions,
    ) -> impl Iterator<Item = ConfiguredAttrFull<'_>> + '_ {
        self.as_ref().attrs(opts)
    }

    pub fn get<'a>(
        &'a self,
        attr: &str,
        opts: AttrInspectOptions,
    ) -> Option<ConfiguredAttrFull<'a>> {
        self.as_ref().get(attr, opts)
    }

    pub fn call_stack(&self) -> Option<String> {
        match &self.0.target_node {
            TargetNodeOrForward::TargetNode(n) => n.call_stack(),
            TargetNodeOrForward::Forward(_, n) => n.call_stack(),
        }
    }

    pub fn root_location(&self) -> Option<StarlarkTargetCallStackRoot> {
        match &self.0.target_node {
            TargetNodeOrForward::TargetNode(n) => n.root_location(),
            TargetNodeOrForward::Forward(_, n) => n.root_location(),
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

    /// If this node is a forward node, return the target it forwards to.
    pub fn forward_target(&self) -> Option<&ConfiguredTargetNode> {
        match &self.0.target_node {
            TargetNodeOrForward::TargetNode(_) => None,
            TargetNodeOrForward::Forward(_, n) => Some(n),
        }
    }

    pub fn unwrap_forward(&self) -> &ConfiguredTargetNode {
        match self.forward_target() {
            None => self,
            Some(t) => t,
        }
    }

    #[inline]
    pub fn target_configuration(&self) -> &ConfigurationData {
        self.as_ref().0.get().label.cfg()
    }

    #[inline]
    pub fn execution_platform(&self) -> buck2_error::Result<&ExecutionPlatform> {
        self.as_ref().execution_platform_resolution().platform()
    }

    #[inline]
    pub fn uses_plugins(&self) -> &[PluginKind] {
        self.as_ref().uses_plugins()
    }

    pub fn plugin_lists(&self) -> &PluginLists {
        &self.0.plugin_lists
    }

    #[inline]
    pub fn as_ref(&self) -> ConfiguredTargetNodeRef<'_> {
        ConfiguredTargetNodeRef(triomphe::Arc::borrow_arc(&self.0))
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        triomphe::Arc::ptr_eq(&self.0, &other.0)
    }
}

/// The representation of the deps for a ConfiguredTargetNode. Provides the operations we require
/// (iteration, eq, and hash), but guarantees those aren't recursive of the dep nodes' data.
#[derive(Allocative)]
struct ConfiguredTargetNodeDeps {
    /// Number of deps, excluding exec deps. Used as an index to retrieve exec_deps
    deps_count: usize,
    /// (target deps and toolchain deps) followed by `exec_deps`.
    all_deps: Box<[ConfiguredTargetNode]>,
}

impl ConfiguredTargetNodeDeps {
    fn new(deps: Vec<ConfiguredTargetNode>, exec_deps: Vec<ConfiguredTargetNode>) -> Self {
        if deps.is_empty() {
            ConfiguredTargetNodeDeps {
                deps_count: 0,
                all_deps: exec_deps.into_boxed_slice(),
            }
        } else if exec_deps.is_empty() {
            ConfiguredTargetNodeDeps {
                deps_count: deps.len(),
                all_deps: deps.into_boxed_slice(),
            }
        } else {
            ConfiguredTargetNodeDeps {
                deps_count: deps.len(),
                all_deps: deps
                    .into_iter()
                    .chain(exec_deps)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            }
        }
    }

    fn deps(&self) -> &[ConfiguredTargetNode] {
        &self.all_deps[..self.deps_count]
    }

    fn exec_deps(&self) -> &[ConfiguredTargetNode] {
        &self.all_deps[self.deps_count..]
    }
}

/// We only do Arc comparisons here. The rationale is that each ConfiguredTargetNode should only
/// ever exist in one instance in the graph, so if the ptr eq doesn't match, then we don't do a
/// deep comparison.
impl PartialEq for ConfiguredTargetNodeDeps {
    fn eq(&self, other: &Self) -> bool {
        let ConfiguredTargetNodeDeps {
            deps_count,
            all_deps,
        } = self;
        *deps_count == other.deps_count && all_deps.len() == other.all_deps.len() && {
            let it1 = all_deps.iter();
            let it2 = other.all_deps.iter();
            it1.zip(it2).all(|(x, y)| triomphe::Arc::ptr_eq(&x.0, &y.0))
        }
    }
}

impl Eq for ConfiguredTargetNodeDeps {}

/// This has historically only hashed the labels. This may or may not be right, because it means
/// two nodes that reference different instances of a given dependency will hash equal, even if
/// the dependency has definitely changed (e.g. because its own deps changed).
impl Hash for ConfiguredTargetNodeDeps {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ConfiguredTargetNodeDeps {
            deps_count,
            all_deps,
        } = self;
        state.write_usize(*deps_count);
        for node in &**all_deps {
            node.label().hash(state);
        }
    }
}

/// Like `&ConfiguredTargetNode`, but cheaper (one fewer indirection).
#[derive(Debug, Copy, Clone)]
pub struct ConfiguredTargetNodeRef<'a>(triomphe::ArcBorrow<'a, Hashed<ConfiguredTargetNodeData>>);

impl Dupe for ConfiguredTargetNodeRef<'_> {}

impl<'a> ConfiguredTargetNodeRef<'a> {
    #[inline]
    pub fn to_owned(self) -> ConfiguredTargetNode {
        ConfiguredTargetNode(triomphe::ArcBorrow::clone_arc(&self.0))
    }

    #[inline]
    pub fn deps(self) -> impl Iterator<Item = &'a ConfiguredTargetNode> {
        self.0.get().all_deps.all_deps.iter()
    }

    #[inline]
    pub fn ptr_eq(self, other: Self) -> bool {
        triomphe::ArcBorrow::ptr_eq(&self.0, &other.0)
    }

    #[inline]
    pub fn label(self) -> &'a ConfiguredTargetLabel {
        self.0.get().label.key()
    }

    #[inline]
    pub fn hashed_label(self) -> Hashed<&'a ConfiguredTargetLabel> {
        self.0.get().label.as_ref()
    }

    fn attr_configuration_context(self) -> AttrConfigurationContextImpl<'a> {
        AttrConfigurationContextImpl::new(
            &self.0.get().resolved_configuration,
            &self.0.get().execution_platform_resolution,
            &self.0.get().resolved_transition_configurations,
            &self.0.get().platform_cfgs,
        )
    }

    pub fn oncall(self) -> Option<&'a str> {
        self.0.get().target_node.oncall()
    }

    pub fn special_attr_or_none(&self, key: &str) -> Option<ConfiguredAttr> {
        match key {
            TYPE => Some(ConfiguredAttr::String(StringLiteral(
                self.rule_type().name().into(),
            ))),
            DEPS => Some(ConfiguredAttr::List(
                self.deps()
                    .map(|t| {
                        ConfiguredAttr::Label(ConfiguredProvidersLabel::new(
                            t.label().dupe(),
                            ProvidersName::Default,
                        ))
                    })
                    .collect(),
            )),
            PACKAGE => Some(ConfiguredAttr::String(StringLiteral(ArcStr::from(
                self.buildfile_path().to_string(),
            )))),
            ONCALL => Some(match self.oncall() {
                None => ConfiguredAttr::None,
                Some(x) => ConfiguredAttr::String(StringLiteral(ArcStr::from(x))),
            }),
            TARGET_CONFIGURATION => Some(ConfiguredAttr::String(StringLiteral(ArcStr::from(
                self.0.label.cfg().to_string(),
            )))),
            EXECUTION_PLATFORM => Some(ConfiguredAttr::String(StringLiteral(
                self.0
                    .execution_platform_resolution
                    .platform()
                    .map_or_else(|_| ArcStr::from("<NONE>"), |v| ArcStr::from(v.id())),
            ))),
            PLUGINS => Some(self.plugins_as_attr()),
            _ => None,
        }
    }

    pub fn special_attrs(self) -> impl Iterator<Item = (&'a str, ConfiguredAttr)> {
        [
            TYPE,
            DEPS,
            PACKAGE,
            ONCALL,
            TARGET_CONFIGURATION,
            EXECUTION_PLATFORM,
            PLUGINS,
        ]
        .into_iter()
        .map(move |key| {
            (
                key,
                // use unwrap here, if fail here it means we iter over a key not in the match list from `special_attr_or_none`
                self.special_attr_or_none(key).unwrap(),
            )
        })
    }

    pub fn attrs(
        self,
        opts: AttrInspectOptions,
    ) -> impl Iterator<Item = ConfiguredAttrFull<'a>> + 'a {
        self.0.get().target_node.attrs(opts).map(move |a| {
            a.configure(&self.attr_configuration_context())
                .expect("checked attr configuration in constructor")
        })
    }

    pub fn get(self, attr: &str, opts: AttrInspectOptions) -> Option<ConfiguredAttrFull<'a>> {
        self.0.get().target_node.attr_or_none(attr, opts).map(|v| {
            v.configure(&self.attr_configuration_context())
                .expect("checked attr configuration in constructor")
        })
    }

    pub fn inputs(self) -> impl Iterator<Item = CellPath> + 'a {
        struct InputsCollector {
            inputs: Vec<CellPath>,
        }
        impl ConfiguredAttrTraversal for InputsCollector {
            fn dep(&mut self, _dep: &ConfiguredProvidersLabel) -> buck2_error::Result<()> {
                Ok(())
            }

            fn input(&mut self, path: SourcePathRef) -> buck2_error::Result<()> {
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
    pub fn queries(
        self,
    ) -> impl Iterator<Item = (String, ResolvedQueryLiterals<ConfiguredProvidersLabel>)> + 'a {
        struct Traversal {
            queries: Vec<(String, ResolvedQueryLiterals<ConfiguredProvidersLabel>)>,
        }
        let mut traversal = Traversal {
            queries: Vec::new(),
        };
        impl ConfiguredAttrTraversal for Traversal {
            fn dep(&mut self, _dep: &ConfiguredProvidersLabel) -> buck2_error::Result<()> {
                // ignored.
                Ok(())
            }

            fn query(
                &mut self,
                query: &str,
                resolved_literals: &ResolvedQueryLiterals<ConfiguredProvidersLabel>,
            ) -> buck2_error::Result<()> {
                self.queries
                    .push((query.to_owned(), resolved_literals.clone()));
                Ok(())
            }
        }

        for a in self.attrs(AttrInspectOptions::All) {
            // Optimization.
            if !a.attr.coercer().0.may_have_queries {
                continue;
            }

            a.traverse(self.label().pkg(), &mut traversal).unwrap();
        }
        traversal.queries.into_iter()
    }

    pub fn rule_type(self) -> &'a RuleType {
        self.0.get().target_node.rule_type()
    }

    pub fn execution_platform_resolution(self) -> &'a ExecutionPlatformResolution {
        &self.0.get().execution_platform_resolution
    }

    pub fn uses_plugins(self) -> &'a [PluginKind] {
        match &self.0.get().target_node {
            TargetNodeOrForward::TargetNode(target_node) => target_node.uses_plugins(),
            TargetNodeOrForward::Forward(_, _) => &[],
        }
    }

    fn plugins_as_attr(self) -> ConfiguredAttr {
        let mut kinds = Vec::new();
        for (kind, plugins) in self.plugin_lists().iter_by_kind() {
            // Using plugin dep here is a bit of an abuse. However, there's no
            // `ConfiguredAttr::TargetLabel` type, and it also seems excessive to add one for this
            // reason alone
            let plugins = plugins
                .map(|(target, _)| ConfiguredAttr::PluginDep(target.dupe(), kind.dupe()))
                .collect();
            kinds.push((
                ConfiguredAttr::String(StringLiteral(ArcStr::from(kind.as_str()))),
                ConfiguredAttr::List(plugins),
            ));
        }
        ConfiguredAttr::Dict(kinds.into_iter().collect())
    }

    pub fn plugin_lists(self) -> &'a PluginLists {
        &self.0.get().plugin_lists
    }

    pub fn buildfile_path(self) -> &'a BuildFilePath {
        self.0.get().target_node.buildfile_path()
    }
}
