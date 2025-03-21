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

    fn rule_type(&self) -> Cow<str> {
        Cow::Borrowed(TargetNodeData::rule_type(self).name())
    }

    fn name(&self) -> Cow<str> {
        Cow::Borrowed(self.label().name().as_str())
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        TargetNode::buildfile_path(self)
    }

    fn deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        TargetNode::deps(self)
    }

    fn exec_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        TargetNode::exec_deps(self)
    }

    fn target_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        TargetNode::target_deps(self)
    }

    fn configuration_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        TargetNode::get_configuration_deps(self).map(|k| k.target())
    }

    fn toolchain_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        TargetNode::toolchain_deps(self)
    }
    fn tests<'a>(&'a self) -> Option<impl Iterator<Item = Self::Key> + Send + 'a> {
        Some(self.tests().map(|t| t.target().dupe()))
    }

    fn attr_any_matches(
        attr: &Self::Attr<'_>,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        attr.any_matches(filter)
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in TargetNode::special_attrs(self) {
            func(name, &attr)?;
        }
        Ok(())
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for a in self.attrs(AttrInspectOptions::All) {
            func(a.name, a.value)?;
        }
        Ok(())
    }

    fn defined_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for a in self.attrs(AttrInspectOptions::DefinedOnly) {
            func(a.name, a.value)?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, key: &str, mut func: F) -> R {
        func(
            self.attr_or_none(key, AttrInspectOptions::All)
                .as_ref()
                .map(|a| a.value),
        )
    }

    fn map_any_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, key: &str, mut func: F) -> R {
        match self.attr_or_none(key, AttrInspectOptions::All) {
            Some(attr) => func(Some(attr.value)),
            None => match self.special_attr_or_none(key) {
                Some(special) => func(Some(&special)),
                None => func(None),
            },
        }
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
