/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;

use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::graph::node::LabeledNode;
use dupe::Dupe;
use starlark_map::Hashed;

use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::nodes::configured::ConfiguredTargetNode;
use crate::nodes::configured::ConfiguredTargetNodeRef;

impl LabeledNode for ConfiguredTargetNode {
    type Key = ConfiguredTargetLabel;

    fn node_key(&self) -> &Self::Key {
        ConfiguredTargetNode::label(self)
    }

    fn hashed_node_key(&self) -> Hashed<&Self::Key> {
        ConfiguredTargetNode::hashed_label(self)
    }
}

impl QueryTarget for ConfiguredTargetNode {
    type Attr<'a> = ConfiguredAttr;

    fn label_for_filter(&self) -> String {
        self.label().unconfigured().to_string()
    }

    fn rule_type(&self) -> Cow<str> {
        Cow::Borrowed(ConfiguredTargetNode::rule_type(self).name())
    }

    fn name(&self) -> Cow<str> {
        Cow::Borrowed(self.label().name().as_str())
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        ConfiguredTargetNode::buildfile_path(self)
    }

    fn deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        ConfiguredTargetNode::deps(self).map(|v| v.label())
    }

    fn exec_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        ConfiguredTargetNode::exec_deps(self).map(|v| v.label())
    }

    fn target_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        ConfiguredTargetNode::target_deps(self).map(|v| v.label())
    }

    fn configuration_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        ConfiguredTargetNode::configuration_deps(self).map(|v| v.label())
    }

    fn toolchain_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        ConfiguredTargetNode::toolchain_deps(self).map(|v| v.label())
    }
    fn tests<'a>(&'a self) -> Option<impl Iterator<Item = Self::Key> + Send + 'a> {
        Some(self.tests().map(|t| t.target().dupe()))
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in ConfiguredTargetNode::special_attrs(self) {
            func(name, &attr)?;
        }
        Ok(())
    }

    fn attr_any_matches(
        attr: &Self::Attr<'_>,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        attr.any_matches(filter)
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for a in self.attrs(AttrInspectOptions::All) {
            func(a.name, &a.value)?;
        }
        Ok(())
    }

    fn defined_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for a in self.attrs(AttrInspectOptions::DefinedOnly) {
            func(a.name, &a.value)?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, key: &str, mut func: F) -> R {
        func(
            self.get(key, AttrInspectOptions::All)
                .as_ref()
                .map(|v| &v.value),
        )
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

    fn map_any_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, key: &str, mut func: F) -> R {
        match self
            .get(key, AttrInspectOptions::All)
            .as_ref()
            .map(|v| &v.value)
        {
            Some(attr) => func(Some(attr)),
            None => func(self.special_attr_or_none(key).as_ref()),
        }
    }
}

impl<'a> LabeledNode for ConfiguredTargetNodeRef<'a> {
    type Key = ConfiguredTargetLabel;

    fn node_key(&self) -> &Self::Key {
        ConfiguredTargetNodeRef::label(*self)
    }

    fn hashed_node_key(&self) -> Hashed<&Self::Key> {
        ConfiguredTargetNodeRef::hashed_label(*self)
    }
}
