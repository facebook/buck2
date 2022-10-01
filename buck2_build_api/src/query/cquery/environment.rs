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
use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_node::compatibility::MaybeCompatible;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::functions::docs::QueryEnvironmentDescription;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use buck2_query::query::syntax::simple::functions::HasModuleDescription;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::async_depth_limited_traversal;
use buck2_query::query::traversal::AsyncNodeLookup;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use gazebo::dupe::Dupe;
use tracing::warn;

use crate::query::cquery::universe::CqueryUniverse;
use crate::query::uquery::environment::allbuildfiles;
use crate::query::uquery::environment::rbuildfiles;
use crate::query::uquery::environment::QueryLiterals;
use crate::query::uquery::environment::UqueryDelegate;

#[derive(Debug, thiserror::Error)]
enum CqueryError {
    #[error("Target universe not specified (internal error)")]
    NoUniverse,
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

/// [Context](https://fburl.com/adiagq2f).
#[derive(Copy, Clone, Dupe)]
pub enum CqueryOwnerBehavior {
    Deprecated,
    Correct,
}

pub struct CqueryEnvironment<'c> {
    delegate: Arc<dyn CqueryDelegate + 'c>,
    literals: Arc<dyn QueryLiterals<ConfiguredTargetNode> + 'c>,
    // TODO(nga): BXL `cquery` function does not provides us the universe.
    // TODO(nga): do not compute the universe when we don't need it, because it is not free.
    //   For example, when evaluating
    //   ```
    //   buck2 cquery 'deps(//foo:bar)'
    //   ```
    universe: Option<CqueryUniverse>,
    owner_behavior: CqueryOwnerBehavior,
}

impl<'c> CqueryEnvironment<'c> {
    pub fn new(
        delegate: Arc<dyn CqueryDelegate + 'c>,
        literals: Arc<dyn QueryLiterals<ConfiguredTargetNode> + 'c>,
        universe: Option<CqueryUniverse>,
        owner_behavior: CqueryOwnerBehavior,
    ) -> Self {
        Self {
            delegate,
            literals,
            universe,
            owner_behavior,
        }
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

    /// Deprecated `owner` function implementation.
    /// See [this post](https://fburl.com/0xv7u4bz) for details.
    async fn owner_deprecated(&self, path: &CellPath) -> anyhow::Result<Vec<ConfiguredTargetNode>> {
        // need to explicitly track this rather than checking for changes to result set since the owner might
        // already be in the set.
        let mut owners = Vec::new();
        match self
            .delegate
            .uquery_delegate()
            .get_enclosing_packages(path)
            .await
        {
            Ok(packages) => {
                let package_futs = packages.iter().map(|package| async move {
                    let mut result: Vec<ConfiguredTargetNode> = Vec::new();

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
                                        &self.delegate.get_configured_target(node.label()).await?,
                                    ),
                                );
                            }
                        }
                    }

                    anyhow::Ok(result)
                });

                for nodes in futures::future::join_all(package_futs).await.into_iter() {
                    for node in nodes?.into_iter() {
                        owners.push(node);
                    }
                }
            }
            Err(_) => {
                // we don't consider this an error, it's usually the case that the user
                // just wants to know the target owning the file if it exists.
            }
        };
        Ok(owners)
    }

    fn owner_correct(&self, path: &CellPath) -> anyhow::Result<Vec<ConfiguredTargetNode>> {
        let universe = self.universe.as_ref().context(CqueryError::NoUniverse)?;
        Ok(universe.owners(path))
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

    async fn allbuildfiles(&self, universe: &TargetSet<Self::Target>) -> anyhow::Result<FileSet> {
        return allbuildfiles(universe, self.delegate.uquery_delegate()).await;
    }

    async fn rbuildfiles(&self, universe: &FileSet, argset: &FileSet) -> anyhow::Result<FileSet> {
        return rbuildfiles(universe, argset, self.delegate.uquery_delegate()).await;
    }

    async fn owner(&self, paths: &FileSet) -> anyhow::Result<TargetSet<Self::Target>> {
        let mut result = TargetSet::new();

        for path in paths.iter() {
            let owners = match &self.owner_behavior {
                CqueryOwnerBehavior::Deprecated => self.owner_deprecated(path).await?,
                CqueryOwnerBehavior::Correct => self.owner_correct(path)?,
            };
            if owners.is_empty() {
                warn!("No owner was found for {}", path);
            }
            result.extend(owners);
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
