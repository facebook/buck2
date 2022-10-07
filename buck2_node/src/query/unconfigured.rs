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
use buck2_core::target::TargetLabel;
use buck2_query::query::environment::LabeledNode;
use buck2_query::query::environment::QueryTarget;
use gazebo::dupe::Dupe;

use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::nodes::unconfigured::TargetNode;

impl LabeledNode for TargetNode {
    type NodeRef = TargetLabel;

    fn node_ref(&self) -> &Self::NodeRef {
        TargetNode::label(self)
    }
}

impl QueryTarget for TargetNode {
    type Attr = CoercedAttr;

    fn rule_type(&self) -> Cow<str> {
        Cow::Borrowed(TargetNode::rule_type(self).name())
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        TargetNode::buildfile_path(self)
    }

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        box TargetNode::deps(self)
    }

    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        box TargetNode::exec_deps(self)
    }

    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        box TargetNode::target_deps(self)
    }

    fn tests<'a>(&'a self) -> Option<Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a>> {
        Some(box self.tests().map(|t| t.target().dupe()))
    }

    fn attr_any_matches(
        attr: &Self::Attr,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
        attr.any_matches(filter)
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in TargetNode::special_attrs(self) {
            func(name, &attr)?;
        }
        Ok(())
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in self.attrs(AttrInspectOptions::All) {
            func(name, attr)?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr>) -> R>(&self, key: &str, mut func: F) -> R {
        func(self.attr_or_none(key, AttrInspectOptions::All))
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

    fn call_stack(&self) -> Option<String> {
        self.call_stack()
    }
}
