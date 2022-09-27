/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;

use async_trait::async_trait;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_query::query::environment::LabeledNode;
use buck2_query::query::environment::NodeLabel;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::traversal::AsyncNodeLookup;
use buck2_query::query::traversal::NodeLookup;
use gazebo::dupe::Dupe;
use ref_cast::RefCast;

use crate::attrs::attr_type::attr_config::AttrConfig;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::inspect_options::AttrInspectOptions;
use crate::nodes::configured::ConfiguredTargetNode;

/// `ConfiguredTargetNode` as both `LabeledNode` and `NodeLabel` and also `QueryTarget`.
#[derive(Debug, Dupe, Clone, RefCast)]
#[repr(C)]
pub struct ConfiguredGraphNodeRef(pub ConfiguredTargetNode);

impl ConfiguredGraphNodeRef {
    pub fn label(&self) -> &ConfiguredTargetLabel {
        self.0.name()
    }
}

impl std::fmt::Display for ConfiguredGraphNodeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.label().fmt(f)
    }
}

impl PartialOrd for ConfiguredGraphNodeRef {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.label().partial_cmp(other.label())
    }
}

impl Ord for ConfiguredGraphNodeRef {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.label().cmp(other.label())
    }
}

impl PartialEq for ConfiguredGraphNodeRef {
    fn eq(&self, other: &Self) -> bool {
        self.label().eq(other.label())
    }
}

impl Eq for ConfiguredGraphNodeRef {}

impl std::hash::Hash for ConfiguredGraphNodeRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.label().hash(state)
    }
}

impl NodeLabel for ConfiguredGraphNodeRef {}

impl LabeledNode for ConfiguredGraphNodeRef {
    type NodeRef = ConfiguredGraphNodeRef;

    fn node_ref(&self) -> &Self::NodeRef {
        self
    }
}

impl QueryTarget for ConfiguredGraphNodeRef {
    type Attr = ConfiguredAttr;

    fn rule_type(&self) -> Cow<str> {
        Cow::Borrowed(self.0.rule_type().name())
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        self.0.buildfile_path()
    }

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        box self.0.deps().map(ConfiguredGraphNodeRef::ref_cast)
    }

    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        // TODO(cjhopman): This should return a Result. It should also be implemented.
        unimplemented!("exec_deps() isn't implemented for query attrs")
    }

    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        // TODO(cjhopman): This should return a Result. It should also be implemented.
        unimplemented!("target_deps() isn't implemented for query attrs")
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
        for (name, attr) in self.0.special_attrs() {
            func(name, &attr)?;
        }
        Ok(())
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in self.0.attrs(AttrInspectOptions::All) {
            func(name, &attr)?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr>) -> R>(&self, key: &str, mut func: F) -> R {
        func(self.0.get(key, AttrInspectOptions::All).as_ref())
    }

    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for input in self.0.inputs() {
            func(input)?;
        }
        Ok(())
    }

    fn call_stack(&self) -> Option<String> {
        self.0.call_stack()
    }
}

/// Graph lookup implementation for `ConfiguredGraphNodeRef`.
/// The implementation is trivial because `ConfiguredGraphNodeRef` is both node ref and node.
pub struct ConfiguredGraphNodeRefLookup;

#[async_trait]
impl AsyncNodeLookup<ConfiguredGraphNodeRef> for ConfiguredGraphNodeRefLookup {
    async fn get(&self, label: &ConfiguredGraphNodeRef) -> anyhow::Result<ConfiguredGraphNodeRef> {
        Ok(label.dupe())
    }
}

impl NodeLookup<ConfiguredGraphNodeRef> for ConfiguredGraphNodeRefLookup {
    fn get(&self, label: &ConfiguredGraphNodeRef) -> anyhow::Result<ConfiguredGraphNodeRef> {
        Ok(label.dupe())
    }
}
