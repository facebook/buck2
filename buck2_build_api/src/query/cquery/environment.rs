/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{borrow::Cow, sync::Arc};

use async_trait::async_trait;
use buck2_core::{
    cells::paths::CellPath,
    result::SharedResult,
    target::{ConfiguredTargetLabel, TargetLabel},
};
use buck2_interpreter::common::BuildFilePath;
use buck2_query::query::{
    environment::{QueryEnvironment, QueryTarget, QueryTargetAttr},
    syntax::simple::{
        eval::{file_set::FileSet, set::TargetSet},
        functions::{
            docs::QueryEnvironmentDescription, DefaultQueryFunctionsModule, HasModuleDescription,
        },
    },
    traversal::{
        async_depth_first_postorder_traversal, async_depth_limited_traversal, AsyncNodeLookup,
        AsyncTraversalDelegate,
    },
};
use gazebo::dupe::Dupe;
use tracing::warn;

use crate::{
    attrs::{attr_type::attr_literal::AttrConfig, configured_attr::ConfiguredAttr},
    nodes::{compatibility::MaybeCompatible, configured::ConfiguredTargetNode},
    query::uquery::environment::{QueryLiterals, UqueryDelegate},
};

impl QueryTargetAttr for ConfiguredAttr {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        AttrConfig::any_matches(self, filter)
    }
}

impl QueryTarget for ConfiguredTargetNode {
    type NodeRef = ConfiguredTargetLabel;
    type Attr = ConfiguredAttr;

    fn node_ref(&self) -> &Self::NodeRef {
        ConfiguredTargetNode::name(self)
    }

    fn rule_type(&self) -> Cow<str> {
        Cow::Borrowed(ConfiguredTargetNode::rule_type(self).name())
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        ConfiguredTargetNode::buildfile_path(self)
    }

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a> {
        box ConfiguredTargetNode::deps(self).map(|l| l.target().dupe())
    }

    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a> {
        box ConfiguredTargetNode::execution_deps(self).map(|l| l.target().dupe())
    }

    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a> {
        box ConfiguredTargetNode::target_deps(self).map(|l| l.target().dupe())
    }

    fn tests<'a>(&'a self) -> Option<Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a>> {
        Some(box self.tests().map(|t| t.target().dupe()))
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in ConfiguredTargetNode::special_attrs(self) {
            func(&name, &attr)?;
        }
        Ok(())
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in self.attrs() {
            func(name, &attr)?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr>) -> R>(&self, key: &str, mut func: F) -> R {
        func(self.get(key).as_ref())
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

/// CqueryDelegate resolves information needed by the QueryEnvironment.
#[async_trait]
pub trait CqueryDelegate: Send + Sync {
    fn uquery_delegate(&self) -> &dyn UqueryDelegate;

    async fn get_node_for_target(
        &self,
        target: &TargetLabel,
    ) -> SharedResult<MaybeCompatible<ConfiguredTargetNode>>;

    async fn get_node_for_configured_target(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> SharedResult<ConfiguredTargetNode>;

    async fn get_configured_target(
        &self,
        target: &TargetLabel,
    ) -> SharedResult<ConfiguredTargetLabel>;
}

pub struct CqueryEnvironment<'c> {
    delegate: Arc<dyn CqueryDelegate + 'c>,
    literals: Arc<dyn QueryLiterals<ConfiguredTargetNode> + 'c>,
}

impl<'c> CqueryEnvironment<'c> {
    pub fn new(
        delegate: Arc<dyn CqueryDelegate + 'c>,
        literals: Arc<dyn QueryLiterals<ConfiguredTargetNode> + 'c>,
    ) -> Self {
        Self { delegate, literals }
    }

    pub fn describe() -> QueryEnvironmentDescription {
        QueryEnvironmentDescription {
            name: "Cquery Environment".to_owned(),
            mods: vec![DefaultQueryFunctionsModule::<Self>::describe()],
        }
    }

    async fn get_node(
        &self,
        label: &ConfiguredTargetLabel,
    ) -> anyhow::Result<ConfiguredTargetNode> {
        Ok(self.delegate.get_node_for_configured_target(label).await?)
    }
}

#[async_trait]
impl<'c> QueryEnvironment for CqueryEnvironment<'c> {
    type Target = ConfiguredTargetNode;

    async fn get_node(&self, node_ref: &ConfiguredTargetLabel) -> anyhow::Result<Self::Target> {
        CqueryEnvironment::get_node(self, node_ref).await
    }

    async fn eval_literals(&self, literals: &[&str]) -> anyhow::Result<TargetSet<Self::Target>> {
        self.literals.eval_literals(literals).await
    }

    async fn eval_file_literal(&self, literal: &str) -> anyhow::Result<FileSet> {
        self.delegate
            .uquery_delegate()
            .eval_file_literal(literal)
            .await
    }

    async fn dfs_postorder(
        &self,
        root: &TargetSet<ConfiguredTargetNode>,
        traversal_delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
    ) -> anyhow::Result<()> {
        async_depth_first_postorder_traversal(self, root.iter_names(), traversal_delegate).await
    }

    async fn depth_limited_traversal(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
        depth: u32,
    ) -> anyhow::Result<()> {
        async_depth_limited_traversal(self, root.iter_names(), delegate, depth).await
    }

    async fn owner(&self, paths: &FileSet) -> anyhow::Result<TargetSet<Self::Target>> {
        let mut result = TargetSet::new();

        for path in paths.iter() {
            // need to explicitly track this rather than checking for changes to result set since the owner might
            // already be in the set.
            let mut found_owner = false;
            match self
                .delegate
                .uquery_delegate()
                .get_enclosing_packages(path)
                .await
            {
                Ok(packages) => {
                    let package_futs = packages.iter().map(|package| async move {
                        let mut result: Vec<Self::Target> = Vec::new();

                        // TODO(cjhopman): We should make sure that the file exists.
                        let targets = self
                            .delegate
                            .uquery_delegate()
                            .eval_build_file(package)
                            .await?;

                        for node in targets.targets().values() {
                            match self.delegate.get_node_for_target(node.label()).await? {
                                MaybeCompatible::Compatible(node) => {
                                    for input in node.inputs() {
                                        if &input == path {
                                            result.push(node.dupe());
                                            // this intentionally only breaks out of the inner loop. We don't need to look at the
                                            // other inputs of this target, but it's possible for a single file to be owned by
                                            // multiple targets.
                                            break;
                                        }
                                    }
                                }
                                MaybeCompatible::Incompatible(reason) => {
                                    // TODO(scottcao): Change all skipping messages from eprintln to warn
                                    // TODO(scottcao): Add event for incompatible target skipping
                                    eprintln!(
                                        "{}",
                                        reason.skipping_message(
                                            &self
                                                .delegate
                                                .get_configured_target(node.label())
                                                .await?,
                                        ),
                                    );
                                }
                            }
                        }

                        anyhow::Ok(result)
                    });

                    for nodes in futures::future::join_all(package_futs).await.into_iter() {
                        for node in nodes?.into_iter() {
                            found_owner = true;
                            result.insert(node);
                        }
                    }
                }
                Err(_) => {
                    // we don't consider this an error, it's usually the case that the user
                    // just wants to know the target owning the file if it exists.
                }
            };
            if !found_owner {
                warn!("No owner was found for {}", path);
            }
        }
        Ok(result)
    }
}

#[async_trait]
impl<'a> AsyncNodeLookup<ConfiguredTargetNode> for CqueryEnvironment<'a> {
    async fn get(&self, label: &ConfiguredTargetLabel) -> anyhow::Result<ConfiguredTargetNode> {
        self.get_node(label).await
    }
}
