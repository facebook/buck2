/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::buck_path::path::BuckPathRef;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::TargetLabel;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;

use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::coerced_attr_full::CoercedAttrFull;
use crate::attrs::coerced_deps_collector::CoercedDeps;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::attrs::internal::DEFAULT_TARGET_PLATFORM_ATTRIBUTE_FIELD;
use crate::attrs::internal::TESTS_ATTRIBUTE_FIELD;
use crate::attrs::spec::AttributeSpec;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::attrs::values::AttrValues;
use crate::call_stack::StarlarkCallStack;
use crate::nodes::attributes::CONFIGURATION_DEPS;
use crate::nodes::attributes::DEPS;
use crate::nodes::attributes::ONCALL;
use crate::nodes::attributes::PACKAGE;
use crate::nodes::attributes::TYPE;
use crate::package::Package;
use crate::rule::Rule;
use crate::rule_type::RuleType;
use crate::visibility::VisibilitySpecification;

#[derive(Debug, thiserror::Error)]
enum TargetNodeError {
    #[error("`visibility` attribute coerced incorrectly (`{0}`) (internal error)")]
    IncorrectVisibilityAttribute(String),
}

/// Describes a target including its name, type, and the values that the user provided.
/// Some information (e.g. deps) is extracted eagerly, most is in the attrs map and needs to be
/// accessed via attribute visitors.
///
/// For attributes, to avoid duplicating data across many nodes the TargetNode itself doesn't store
/// the attribute names and it doesn't store an entry for something that has a default value. All
/// that information is contained in the AttributeSpec. This means that to access an attribute we
/// need to look at both the attrs held by the TargetNode and the information in the AttributeSpec.
#[derive(Debug, Clone, Dupe, Eq, PartialEq, Hash, Allocative)]
pub struct TargetNode(pub Arc<TargetNodeData>);

/// The kind of the rule, denoting where it can be used and how.
#[derive(Debug, Copy, Clone, Dupe, Eq, PartialEq, Hash, Allocative)]
pub enum RuleKind {
    /// A normal rule with no special properties.
    Normal,
    /// A configuration rule, meaning it is usable in a configuration context.
    Configuration,
    /// A toolchain rule, meaning it is only usable as a toolchain dep.
    Toolchain,
}

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct TargetNodeData {
    /// Rule type for this target.
    pub rule: Arc<Rule>,

    /// Package.
    package: Arc<Package>,

    label: TargetLabel,

    /// The attribute->value mapping for this rule. It's guaranteed that if an attribute does not
    /// have a value here, it does have a default value in the AttributeSpec.
    attributes: AttrValues,

    // TODO(cjhopman): Consider removing these cached derived fields. Query definitely needs deps
    // cached, but for builds it's potentially unimportant.
    deps_cache: CoercedDeps,

    /// Call stack for the target.
    call_stack: Option<StarlarkCallStack>,
}

impl TargetNode {
    pub fn new(
        rule: Arc<Rule>,
        package: Arc<Package>,
        label: TargetLabel,
        attributes: AttrValues,
        deps_cache: CoercedDeps,
        call_stack: Option<StarlarkCallStack>,
    ) -> TargetNode {
        TargetNode(Arc::new(TargetNodeData {
            rule,
            package,
            label,
            attributes,
            deps_cache,
            call_stack,
        }))
    }

    pub fn rule_kind(&self) -> RuleKind {
        self.0.rule.rule_kind
    }

    pub fn is_configuration_rule(&self) -> bool {
        self.0.rule.rule_kind == RuleKind::Configuration
    }

    pub fn is_toolchain_rule(&self) -> bool {
        self.0.rule.rule_kind == RuleKind::Toolchain
    }

    pub fn get_default_target_platform(&self) -> Option<&TargetLabel> {
        match self.attr_or_none(
            DEFAULT_TARGET_PLATFORM_ATTRIBUTE_FIELD,
            AttrInspectOptions::All,
        ) {
            Some(v) => match v.value {
                CoercedAttr::None => None,
                CoercedAttr::Dep(t) => Some(t.label.target()),
                CoercedAttr::Selector(_) | CoercedAttr::Concat(_) => {
                    unreachable!("coercer verified attribute is not configurable")
                }
                _ => unreachable!("coercer verified the attribute is dep"),
            },
            None => None,
        }
    }

    pub fn rule_type(&self) -> &RuleType {
        &self.0.rule.rule_type
    }

    pub fn buildfile_path(&self) -> &BuildFilePath {
        &self.0.package.buildfile_path
    }

    fn deps_cache(&self) -> &CoercedDeps {
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
            .chain(deps_cache.toolchain_deps.iter())
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

    pub(crate) fn special_attrs(&self) -> impl Iterator<Item = (&str, CoercedAttr)> {
        let typ_attr = CoercedAttr::String(StringLiteral(self.rule_type().name().into()));
        let deps_attr = CoercedAttr::List(
            self.deps()
                .map(|t| CoercedAttr::Label(ProvidersLabel::default_for(t.dupe())))
                .collect(),
        );
        let package_attr = CoercedAttr::String(StringLiteral(ArcStr::from(
            self.buildfile_path().to_string(),
        )));
        vec![
            (TYPE, typ_attr),
            (
                CONFIGURATION_DEPS,
                CoercedAttr::List(
                    self.get_configuration_deps()
                        .map(|t| CoercedAttr::ConfigurationDep(t.dupe()))
                        .collect(),
                ),
            ),
            (DEPS, deps_attr),
            (PACKAGE, package_attr),
            (
                ONCALL,
                match self.oncall() {
                    None => CoercedAttr::None,
                    Some(x) => CoercedAttr::String(StringLiteral(ArcStr::from(x))),
                },
            ),
        ]
        .into_iter()
    }

    pub fn oncall(&self) -> Option<&str> {
        self.0.package.oncall.as_ref().map(|x| x.as_str())
    }

    pub fn visibility(&self) -> anyhow::Result<&VisibilitySpecification> {
        match self.0.attributes.get(AttributeSpec::visibility_attr_id()) {
            Some(CoercedAttr::Visibility(v)) => Ok(v),
            Some(a) => {
                // This code is unreachable: visibility attributes are validated
                // at the coercion stage. But if we did it wrong,
                // better error with all the context than panic.
                Err(TargetNodeError::IncorrectVisibilityAttribute(
                    a.as_display_no_ctx().to_string(),
                )
                .into())
            }
            None => {
                static DEFAULT: VisibilitySpecification = VisibilitySpecification::DEFAULT;
                Ok(&DEFAULT)
            }
        }
    }

    pub fn is_visible_to(&self, target: &TargetLabel) -> anyhow::Result<bool> {
        if self.label().pkg() == target.pkg() {
            return Ok(true);
        }
        Ok(self.visibility()?.is_visible_to(target))
    }

    pub fn attrs(&self, opts: AttrInspectOptions) -> impl Iterator<Item = CoercedAttrFull> {
        self.0.rule.attributes.attrs(&self.0.attributes, opts)
    }

    pub fn platform_deps(&self) -> impl Iterator<Item = &TargetLabel> {
        self.deps_cache().platform_deps.iter()
    }

    /// Return `None` if attribute is not present or unknown.
    pub fn attr_or_none<'a>(
        &'a self,
        key: &str,
        opts: AttrInspectOptions,
    ) -> Option<CoercedAttrFull<'a>> {
        self.0
            .rule
            .attributes
            .attr_or_none(&self.0.attributes, key, opts)
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
        self.0.rule.attributes.attr(&self.0.attributes, key, opts)
    }

    pub fn target_deps(&self) -> impl Iterator<Item = &TargetLabel> {
        self.deps_cache().deps.iter()
    }

    pub fn exec_deps(&self) -> impl Iterator<Item = &TargetLabel> {
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
            fn input(&mut self, _path: BuckPathRef) -> anyhow::Result<()> {
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
        tests.traverse(self.label().pkg(), &mut traversal).unwrap();
        traversal.labels.into_iter()
    }

    pub fn inputs(&self) -> impl Iterator<Item = CellPath> + '_ {
        struct InputsCollector {
            inputs: Vec<CellPath>,
        }

        impl<'a> CoercedAttrTraversal<'a> for InputsCollector {
            fn input(&mut self, path: BuckPathRef) -> anyhow::Result<()> {
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
        for a in self.attrs(AttrInspectOptions::All) {
            a.traverse(self.label().pkg(), &mut traversal)
                .expect("inputs collector shouldn't return errors");
        }

        traversal.inputs.into_iter()
    }

    pub fn call_stack(&self) -> Option<String> {
        self.0.call_stack.as_ref().map(|s| s.to_string())
    }

    /// Hash the fields that impact how this target is built.
    /// Don't do any recursive hashing of the dependencies.
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

pub mod testing {
    use std::sync::Arc;

    use buck2_core::build_file_path::BuildFilePath;
    use buck2_core::fs::paths::file_name::FileNameBuf;
    use buck2_core::package::PackageLabel;
    use buck2_core::target::label::TargetLabel;
    use dupe::Dupe;
    use serde_json::map::Map;
    use serde_json::value::Value;

    use super::*;
    use crate::attrs::attr::Attribute;
    use crate::attrs::coerced_attr::CoercedAttr;
    use crate::attrs::coerced_deps_collector::CoercedDepsCollector;
    use crate::attrs::fmt_context::AttrFmtContext;
    use crate::attrs::inspect_options::AttrInspectOptions;
    use crate::attrs::spec::AttributeSpec;
    use crate::attrs::values::AttrValues;
    use crate::nodes::targets_map::TargetsMap;
    use crate::rule_type::RuleType;

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
            let attr_spec = AttributeSpec::testing_new(
                attrs
                    .iter()
                    .map(|(name, attr, _)| ((*name).to_owned(), attr.clone()))
                    .collect(),
            );

            let mut attributes = AttrValues::with_capacity(attrs.len() + 1);

            attributes.push_sorted(
                AttributeSpec::name_attr_id(),
                CoercedAttr::String(StringLiteral(label.name().as_str().into())),
            );

            let mut deps_cache = CoercedDepsCollector::new();

            for (name, _attr, val) in attrs.into_iter() {
                let idx = attr_spec.attribute_id_by_name(name).unwrap();
                val.traverse(label.pkg(), &mut deps_cache).unwrap();
                attributes.push_sorted(idx, val);
            }

            let buildfile_path = Arc::new(BuildFilePath::new(
                label.pkg().dupe(),
                FileNameBuf::unchecked_new("BUCK"),
            ));
            TargetNode::new(
                Arc::new(Rule {
                    attributes: attr_spec,
                    rule_type,
                    rule_kind: RuleKind::Normal,
                    cfg: None,
                }),
                Arc::new(Package {
                    buildfile_path,
                    oncall: None,
                    default_visibility_to_public: false,
                }),
                label,
                attributes,
                CoercedDeps::from(deps_cache),
                None,
            )
        }
    }

    /// Take a TargetsMap and convert it to a nice json representation. Adds in a __type__ attr
    /// for each target's values to make it clear what the rule type is. That can probably go
    /// away eventually.
    pub fn targets_to_json(
        target: &TargetsMap,
        pkg: PackageLabel,
        opts: AttrInspectOptions,
    ) -> anyhow::Result<Value> {
        let map: Map<String, Value> = target
            .iter()
            .map(|(target_name, values)| {
                let mut json_values: Map<String, Value> = values
                    .attrs(opts)
                    .map(|a| {
                        Ok((
                            a.name.to_owned(),
                            a.value.to_json(&AttrFmtContext {
                                package: Some(pkg.dupe()),
                            })?,
                        ))
                    })
                    .collect::<anyhow::Result<Map<String, Value>>>()?;
                json_values.insert(
                    "__type__".to_owned(),
                    Value::String(values.rule_type().to_string()),
                );
                Ok((target_name.to_string(), Value::from(json_values)))
            })
            .collect::<anyhow::Result<Map<String, Value>>>()?;
        Ok(Value::from(map))
    }
}
