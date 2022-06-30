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
use buck2_core::buck_path::BuckPath;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use buck2_query::query::syntax::simple::eval::file_set::FileNode;
use gazebo::dupe::Dupe;
use indexmap::IndexMap;

use crate::attrs::attr_type::attr_literal::AttrLiteral;
use crate::attrs::attr_type::AttrType;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::coerced_deps_collector::CoercedDepsCollector;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::attrs::internal::DEFAULT_TARGET_PLATFORM_ATTRIBUTE_FIELD;
use crate::attrs::internal::TESTS_ATTRIBUTE_FIELD;
use crate::attrs::spec::AttributeSpec;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::attrs::values::AttrValues;
use crate::call_stack::StarlarkCallStack;
use crate::rule_type::RuleType;
use crate::visibility::VisibilitySpecification;

/// Map of target -> details of those targets within a build file.
pub type TargetsMap = IndexMap<TargetName, TargetNode>;

/// Describes a target including its name, type, and the values that the user provided.
/// Some information (e.g. deps) is extracted eagerly, most is in the attrs map and needs to be
/// accessed via attribute visitors.
///
/// For attributes, to avoid duplicating data across many nodes the TargetNode itself doesn't store
/// the attribute names and it doesn't store an entry for something that has a default value. All
/// that information is contained in the AttributeSpec. This means that to access an attribute we
/// need to look at both the attrs held by the TargetNode and the information in the AttributeSpec.
#[derive(Debug, Clone, Dupe, Eq, PartialEq, Hash)]
pub struct TargetNode(pub Arc<TargetNodeData>);

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct TargetNodeData {
    label: TargetLabel,

    /// The 'type', used to find the implementation function from the graph
    rule_type: RuleType,

    /// The build file which defined this target, e.g. `fbcode//foo/bar/TARGETS`
    buildfile_path: Arc<BuildFilePath>,

    /// The rule is marked as configuration rule.
    is_configuration_rule: bool,

    /// Transition to apply to the target.
    pub cfg: Option<Arc<TransitionId>>,

    /// The attribute spec. This holds the attribute name -> index mapping and the default values
    /// (for those attributes without explicit values).
    attr_spec: Arc<AttributeSpec>,

    /// The attribute->value mapping for this rule. It's guaranteed that if an attribute does not
    /// have a value here, it does have a default value in the AttributeSpec.
    attributes: AttrValues,

    // TODO(cjhopman): Consider removing these cached derived fields. Query definitely needs deps
    // cached, but for builds it's potentially unimportant.
    deps_cache: CoercedDepsCollector,

    /// Visibility specification restricts what targets can depend on this one.
    visibility: VisibilitySpecification,

    /// Call stack for the target.
    call_stack: Option<StarlarkCallStack>,
}

impl TargetNode {
    pub fn new(
        label: TargetLabel,
        rule_type: RuleType,
        buildfile_path: Arc<BuildFilePath>,
        is_configuration_rule: bool,
        cfg: Option<Arc<TransitionId>>,
        attr_spec: Arc<AttributeSpec>,
        attributes: AttrValues,
        deps_cache: CoercedDepsCollector,
        visibility: VisibilitySpecification,
        call_stack: Option<StarlarkCallStack>,
    ) -> TargetNode {
        TargetNode(Arc::new(TargetNodeData {
            label,
            rule_type,
            buildfile_path,
            is_configuration_rule,
            cfg,
            attr_spec,
            attributes,
            deps_cache,
            visibility,
            call_stack,
        }))
    }

    pub fn is_configuration_rule(&self) -> bool {
        self.0.is_configuration_rule
    }

    pub fn get_default_target_platform(&self) -> Option<&TargetLabel> {
        match self.attr_or_none(
            DEFAULT_TARGET_PLATFORM_ATTRIBUTE_FIELD,
            AttrInspectOptions::All,
        ) {
            Some(v) => match v {
                CoercedAttr::Literal(v) => match v {
                    AttrLiteral::None => None,
                    AttrLiteral::Dep(t) => Some(t.label().target()),
                    _ => unreachable!("coercer verified the attribute is dep"),
                },
                CoercedAttr::Selector(_) | CoercedAttr::Concat(_) => {
                    unreachable!("coercer verified attribute is not configurable")
                }
            },
            None => None,
        }
    }

    /// Returns the input files for this node.
    pub fn files(&self) -> anyhow::Result<Vec<FileNode>> {
        unimplemented!()
    }

    pub fn rule_type(&self) -> &RuleType {
        &self.0.rule_type
    }

    pub fn buildfile_path(&self) -> &BuildFilePath {
        &*self.0.buildfile_path
    }

    fn deps_cache(&self) -> &CoercedDepsCollector {
        &self.0.deps_cache
    }

    /// Returns all deps for this node that we know about after processing the build file
    pub fn deps(&self) -> impl Iterator<Item = &TargetLabel> {
        let deps_cache = self.deps_cache();
        deps_cache
            .deps
            .iter()
            .chain(deps_cache.transition_deps.iter().map(|(dep, _tr)| dep))
            .chain(deps_cache.exec_deps.iter())
    }

    /// Deps which are to be transitioned to other configuration using transition function.
    pub fn transition_deps(&self) -> impl Iterator<Item = (&TargetLabel, &Arc<TransitionId>)> {
        self.deps_cache()
            .transition_deps
            .iter()
            .map(|x| (&x.0, &x.1))
    }

    pub fn label(&self) -> &TargetLabel {
        &self.0.label
    }

    pub(crate) fn special_attrs(&self) -> impl Iterator<Item = (String, CoercedAttr)> {
        vec![
            (
                "buck.type".to_owned(),
                CoercedAttr::new_literal(AttrLiteral::String(self.rule_type().name().to_owned())),
            ),
            (
                "buck.configuration_deps".to_owned(),
                CoercedAttr::new_literal(AttrLiteral::List(
                    self.get_configuration_deps()
                        .map(|t| CoercedAttr::new_literal(AttrLiteral::ConfigurationDep(t.dupe())))
                        .collect(),
                    AttrType::configuration_dep(),
                )),
            ),
        ]
        .into_iter()
    }

    pub fn is_visible_to(&self, target: &TargetLabel) -> bool {
        if self.label().pkg() == target.pkg() {
            return true;
        }
        self.0.visibility.is_visible_to(target)
    }

    pub fn attrs(&self, opts: AttrInspectOptions) -> impl Iterator<Item = (&str, &CoercedAttr)> {
        self.0.attr_spec.attrs(&self.0.attributes, opts)
    }

    pub fn platform_deps(&self) -> impl Iterator<Item = &TargetLabel> {
        self.deps_cache().platform_deps.iter()
    }

    /// Return `None` if attribute is not present or unknown.
    pub fn attr_or_none(&self, key: &str, opts: AttrInspectOptions) -> Option<&CoercedAttr> {
        self.0.attr_spec.attr_or_none(&self.0.attributes, key, opts)
    }

    /// Get attribute.
    ///
    /// * `None` if attribute is known but not set and no default.
    /// * error if attribute is unknown.
    pub fn attr(
        &self,
        key: &str,
        opts: AttrInspectOptions,
    ) -> anyhow::Result<Option<&CoercedAttr>> {
        self.0
            .attr_spec
            .attr(&self.0.attributes, key, opts)
            .with_context(|| format!("attribute `{}` not found", key))
    }

    pub fn target_deps(&self) -> impl Iterator<Item = &TargetLabel> {
        self.deps_cache().deps.iter()
    }

    pub fn execution_deps(&self) -> impl Iterator<Item = &TargetLabel> {
        self.deps_cache().exec_deps.iter()
    }

    pub fn get_configuration_deps(&self) -> impl Iterator<Item = &TargetLabel> {
        self.deps_cache().configuration_deps.iter()
    }

    pub fn tests(&self) -> impl Iterator<Item = &ProvidersLabel> {
        #[derive(Default)]
        struct TestCollector<'a> {
            labels: Vec<&'a ProvidersLabel>,
        }

        impl<'a> CoercedAttrTraversal<'a> for TestCollector<'a> {
            fn input(&mut self, _path: &'a BuckPath) -> anyhow::Result<()> {
                Ok(())
            }

            fn dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn exec_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn toolchain_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn transition_dep(
                &mut self,
                _dep: &'a TargetLabel,
                _tr: &Arc<TransitionId>,
            ) -> anyhow::Result<()> {
                Ok(())
            }

            fn platform_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn split_transition_dep(
                &mut self,
                _dep: &'a TargetLabel,
                _tr: &Arc<TransitionId>,
            ) -> anyhow::Result<()> {
                Ok(())
            }

            fn configuration_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn label(&mut self, label: &'a ProvidersLabel) -> anyhow::Result<()> {
                self.labels.push(label);
                Ok(())
            }
        }

        let tests = self
            .attr_or_none(TESTS_ATTRIBUTE_FIELD, AttrInspectOptions::All)
            .expect("tests is an internal attribute field and will always be present");

        let mut traversal = TestCollector::default();
        tests.traverse(&mut traversal).unwrap();
        traversal.labels.into_iter()
    }

    pub fn inputs(&self) -> impl Iterator<Item = CellPath> + '_ {
        struct InputsCollector {
            inputs: Vec<CellPath>,
        }

        impl<'a> CoercedAttrTraversal<'a> for InputsCollector {
            fn input(&mut self, path: &'a BuckPath) -> anyhow::Result<()> {
                self.inputs.push(path.to_cell_path());
                Ok(())
            }

            fn dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn exec_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn toolchain_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn platform_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn transition_dep(
                &mut self,
                _dep: &'a TargetLabel,
                _tr: &Arc<TransitionId>,
            ) -> anyhow::Result<()> {
                Ok(())
            }

            fn split_transition_dep(
                &mut self,
                _dep: &'a TargetLabel,
                _tr: &Arc<TransitionId>,
            ) -> anyhow::Result<()> {
                Ok(())
            }

            fn configuration_dep(&mut self, _dep: &'a TargetLabel) -> anyhow::Result<()> {
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

    pub fn call_stack(&self) -> Option<String> {
        self.0.call_stack.as_ref().map(|s| s.to_string())
    }
}

pub mod testing {
    use std::sync::Arc;

    use buck2_core::build_file_path::BuildFilePath;
    use buck2_core::fs::paths::FileNameBuf;
    use buck2_core::target::TargetLabel;
    use gazebo::dupe::Dupe;
    use small_map::map::SmallMap;

    use crate::attrs::attr::Attribute;
    use crate::attrs::coerced_attr::CoercedAttr;
    use crate::attrs::coerced_deps_collector::CoercedDepsCollector;
    use crate::attrs::id::AttributeId;
    use crate::attrs::spec::AttributeSpec;
    use crate::attrs::values::AttrValues;
    use crate::nodes::unconfigured::TargetNode;
    use crate::rule_type::RuleType;
    use crate::visibility::VisibilitySpecification;

    pub trait TargetNodeExt {
        fn testing_new(
            label: TargetLabel,
            rule_type: RuleType,
            attrs: Vec<(&str, Attribute, CoercedAttr)>,
        ) -> Self;
    }

    impl TargetNodeExt for TargetNode {
        fn testing_new(
            label: TargetLabel,
            rule_type: RuleType,
            attrs: Vec<(&str, Attribute, CoercedAttr)>,
        ) -> TargetNode {
            let mut indices = SmallMap::with_capacity(attrs.len());
            let mut instances = Vec::with_capacity(attrs.len());
            let mut attributes = AttrValues::with_capacity(attrs.len());

            let mut deps_cache = CoercedDepsCollector::new();

            for (index_in_attribute_spec, (name, attr, val)) in attrs.into_iter().enumerate() {
                let idx = AttributeId {
                    index_in_attribute_spec,
                };
                indices.insert(name.to_owned(), idx);
                instances.push(attr);
                val.traverse(&mut deps_cache).unwrap();
                attributes.push_sorted(idx, val);
            }

            let buildfile_path = Arc::new(BuildFilePath::new(
                label.pkg().dupe(),
                FileNameBuf::unchecked_new("BUCK".to_owned()),
            ));
            TargetNode::new(
                label,
                rule_type,
                buildfile_path,
                false,
                None,
                Arc::new(AttributeSpec::testing_new(indices, instances)),
                attributes,
                deps_cache,
                VisibilitySpecification::Public,
                None,
            )
        }
    }
}
