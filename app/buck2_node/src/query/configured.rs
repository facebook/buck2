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
use buck2_query::query::environment::LabeledNode;
use buck2_query::query::environment::QueryTarget;
use dupe::Dupe;
use serde::Serializer;
use starlark_map::Hashed;

use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::attrs::serialize::AttrSerializeWithContext;
use crate::nodes::configured::ConfiguredTargetNode;
use crate::nodes::configured::ConfiguredTargetNodeRef;

impl LabeledNode for ConfiguredTargetNode {
    type NodeRef = ConfiguredTargetLabel;

    fn node_ref(&self) -> &Self::NodeRef {
        ConfiguredTargetNode::label(self)
    }

    fn hashed_node_ref(&self) -> Hashed<&Self::NodeRef> {
        ConfiguredTargetNode::hashed_label(self)
    }
}

impl QueryTarget for ConfiguredTargetNode {
    type Attr<'a> = ConfiguredAttr;

    fn label_for_filter(&self) -> String {
        return self.label().unconfigured().to_string();
    }

    fn rule_type(&self) -> Cow<str> {
        Cow::Borrowed(ConfiguredTargetNode::rule_type(self).name())
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        ConfiguredTargetNode::buildfile_path(self)
    }

    fn deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::NodeRef> + Send + 'a {
        ConfiguredTargetNode::deps(self).map(|v| v.label())
    }

    fn exec_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::NodeRef> + Send + 'a {
        ConfiguredTargetNode::exec_deps(self).map(|v| v.label())
    }

    fn target_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::NodeRef> + Send + 'a {
        ConfiguredTargetNode::target_deps(self).map(|v| v.label())
    }

    fn tests<'a>(&'a self) -> Option<impl Iterator<Item = Self::NodeRef> + Send + 'a> {
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
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
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

    fn call_stack(&self) -> Option<String> {
        self.call_stack()
    }

    fn attr_to_string_alternate(&self, attr: &Self::Attr<'_>) -> String {
        format!(
            "{:#}",
            attr.as_display(&AttrFmtContext {
                package: Some(self.label().pkg().dupe()),
            })
        )
    }

    fn attr_serialize<S: Serializer>(
        &self,
        attr: &Self::Attr<'_>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        attr.serialize_with_ctx(
            &AttrFmtContext {
                package: Some(self.label().pkg().dupe()),
            },
            serializer,
        )
    }
}

impl<'a> LabeledNode for ConfiguredTargetNodeRef<'a> {
    type NodeRef = ConfiguredTargetLabel;

    fn node_ref(&self) -> &Self::NodeRef {
        ConfiguredTargetNodeRef::label(*self)
    }

    fn hashed_node_ref(&self) -> Hashed<&Self::NodeRef> {
        ConfiguredTargetNodeRef::hashed_label(*self)
    }
}
