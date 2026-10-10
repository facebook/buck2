/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;

use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::target::label::label::TargetLabel;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::graph::node::LabeledNode;
use dupe::Dupe;

use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::nodes::unconfigured::TargetNode;
use crate::nodes::unconfigured::TargetNodeData;

impl LabeledNode for TargetNode {
    type Key = TargetLabel;

    fn node_key(&self) -> &Self::Key {
        TargetNode::label(self)
    }
}

impl QueryTarget for TargetNode {
    type Attr<'a> = CoercedAttr;

    fn rule_type(&self) -> Cow<'_, str> {
        Cow::Borrowed(TargetNodeData::rule_type(self).name())
    }

    fn name(&self) -> Cow<'_, str> {
        Cow::Borrowed(self.label().name().as_str())
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        TargetNode::buildfile_path(self)
    }

    fn deps(&self) -> impl Iterator<Item = &Self::Key> + Send + '_ {
        TargetNode::deps(self)
    }

    fn exec_deps(&self) -> impl Iterator<Item = &Self::Key> + Send + '_ {
        TargetNode::exec_deps(self).iter()
    }

    fn target_deps(&self) -> impl Iterator<Item = &Self::Key> + Send + '_ {
        TargetNode::target_deps(self).iter()
    }

    fn configuration_deps(&self) -> impl Iterator<Item = &Self::Key> + Send + '_ {
        TargetNode::get_configuration_deps(self).map(|k| k.target())
    }

    fn toolchain_deps(&self) -> impl Iterator<Item = &Self::Key> + Send + '_ {
        TargetNode::toolchain_deps(self).iter()
    }
    fn tests(&self) -> Option<impl Iterator<Item = Self::Key> + Send + '_> {
        Some(self.tests().map(|t| t.target().dupe()))
    }

    fn attr_any_matches(
        attr: &Self::Attr<'_>,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        attr.any_matches(filter)
    }

    fn special_attrs_for_each<
        E: From<buck2_error::Error>,
        F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>,
    >(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in TargetNode::special_attrs(self) {
            func(name, &attr)?;
        }
        Ok(())
    }

    fn attrs_for_each<
        E: From<buck2_error::Error>,
        F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>,
    >(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for a in self.attrs(AttrInspectOptions::All) {
            func(a.name, a.value)?;
        }
        Ok(())
    }

    fn defined_attrs_for_each<
        E: From<buck2_error::Error>,
        F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>,
    >(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for a in self.attrs(AttrInspectOptions::DefinedOnly) {
            func(a.name, a.value)?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(
        &self,
        key: &str,
        mut func: F,
    ) -> buck2_error::Result<R> {
        Ok(func(
            self.attr_or_none(key, AttrInspectOptions::All)
                .as_ref()
                .map(|a| a.value),
        ))
    }

    fn map_any_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(
        &self,
        key: &str,
        mut func: F,
    ) -> buck2_error::Result<R> {
        Ok(match self.attr_or_none(key, AttrInspectOptions::All) {
            Some(attr) => func(Some(attr.value)),
            None => match self.special_attr_or_none(key) {
                Some(special) => func(Some(&special)),
                None => func(None),
            },
        })
    }

    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for input in self.inputs() {
            func(input)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::bzl::ImportPath;
    use buck2_core::plugins::PluginKindSet;
    use buck2_core::provider::label::ProvidersLabel;
    use buck2_core::target::label::label::TargetLabel;
    use buck2_query::query::syntax::simple::eval::set::TargetSet;
    use buck2_util::arc_str::ArcSlice;

    use crate::attrs::attr::Attribute;
    use crate::attrs::attr_type::AttrType;
    use crate::attrs::attr_type::list::ListLiteral;
    use crate::attrs::coerced_attr::CoercedAttr;
    use crate::bzl_or_bxl_path::BzlOrBxlPath;
    use crate::nodes::unconfigured::TargetNode;
    use crate::nodes::unconfigured::testing::TargetNodeExt;
    use crate::provider_id_set::ProviderIdSet;
    use crate::rule_type::RuleType;
    use crate::rule_type::StarlarkRuleType;

    fn node(label: &str, rule_name: &str, dep: &str) -> TargetNode {
        TargetNode::testing_new(
            TargetLabel::testing_parse(label),
            RuleType::Starlark(Arc::new(StarlarkRuleType {
                path: BzlOrBxlPath::Bzl(ImportPath::testing_new("root//rules:defs.bzl")),
                name: rule_name.to_owned(),
            })),
            vec![(
                "deps",
                Attribute::new(
                    None,
                    "",
                    AttrType::list(AttrType::dep(ProviderIdSet::EMPTY, PluginKindSet::EMPTY)),
                )
                .unwrap(),
                CoercedAttr::List(ListLiteral(ArcSlice::new([CoercedAttr::Dep(
                    ProvidersLabel::default_for(TargetLabel::testing_parse(dep)),
                )]))),
            )],
            None,
        )
    }

    fn labels(set: &TargetSet<TargetNode>) -> Vec<String> {
        set.iter().map(|t| t.label().to_string()).collect()
    }

    /// `attrfilter` looks attributes up with `map_any_attr`, which includes the `buck.*` special
    /// attributes; `nattrfilter`, "the opposite of attrfilter", uses `map_attr` and treats a
    /// special attribute as missing, so neither function returns the target.
    #[test]
    fn nattrfilter_ignores_special_attributes() {
        let set = TargetSet::from_iter([node("root//bin:the_binary", "my_rule", "root//lib:lib1")]);
        let is_other_rule = |s: &str| Ok(s == "other_rule");
        assert!(
            set.attrfilter("buck.type", &is_other_rule)
                .unwrap()
                .is_empty()
        );
        assert_eq!(
            labels(&set.nattrfilter("buck.type", &is_other_rule).unwrap()),
            Vec::<String>::new()
        );
        // User attributes work as documented.
        let is_lib1 = |s: &str| Ok(s == "root//lib:lib1");
        assert_eq!(
            labels(&set.attrfilter("deps", &is_lib1).unwrap()),
            vec!["root//bin:the_binary".to_owned()]
        );
        assert!(set.nattrfilter("deps", &is_lib1).unwrap().is_empty());
    }
}
