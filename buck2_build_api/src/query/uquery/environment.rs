/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{borrow::Cow, collections::HashMap, sync::Arc};

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::{
    cells::paths::CellPath,
    package::Package,
    result::{SharedResult, ToSharedResultExt},
    target::{TargetLabel, TargetName},
};
use buck2_interpreter::{common::BuildFilePath, pattern::ResolvedPattern};
use buck2_query::query::{
    environment::{QueryEnvironment, QueryEnvironmentError, QueryTarget, QueryTargetAttr},
    syntax::simple::{
        eval::{file_set::FileSet, set::TargetSet},
        functions::{
            docs::QueryEnvironmentDescription, DefaultQueryFunctions, HasModuleDescription,
        },
    },
    traversal::{
        async_depth_first_postorder_traversal, async_depth_limited_traversal, AsyncNodeLookup,
        AsyncTraversalDelegate,
    },
};
use gazebo::prelude::*;
use thiserror::Error;
use tracing::warn;

use crate::{
    attrs::{attr_type::attr_literal::AttrConfig, coerced_attr::CoercedAttr},
    interpreter::module_internals::EvaluationResult,
    nodes::unconfigured::TargetNode,
};

#[derive(Debug, Error)]
enum QueryLiteralResolutionError {
    #[error("literal `{0}` missing in pre-resolved literals")]
    LiteralMissing(String),
}

impl QueryTargetAttr for CoercedAttr {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        AttrConfig::any_matches(self, filter)
    }
}

pub enum SpecialAttr {
    String(String),
}

impl QueryTarget for TargetNode {
    type NodeRef = TargetLabel;
    type Attr = CoercedAttr;

    fn node_ref(&self) -> &Self::NodeRef {
        TargetNode::label(self)
    }

    fn rule_type(&self) -> Cow<str> {
        Cow::Borrowed(TargetNode::rule_type(self).name())
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        TargetNode::buildfile_path(self)
    }

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a> {
        box TargetNode::deps(self).duped()
    }

    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a> {
        box TargetNode::execution_deps(self).duped()
    }

    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a> {
        box TargetNode::target_deps(self).duped()
    }

    fn tests<'a>(&'a self) -> Option<Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a>> {
        Some(box self.tests().map(|t| t.target().dupe()))
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in TargetNode::special_attrs(self) {
            func(&name, &attr)?;
        }
        Ok(())
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        for (name, attr) in self.attrs() {
            func(name, attr)?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr>) -> R>(&self, key: &str, mut func: F) -> R {
        func(self.attr(key))
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

/// UqueryDelegate resolves information needed by the QueryEnvironment.
#[async_trait]
pub trait UqueryDelegate: Send + Sync {
    /// Returns the EvaluationResult for evaluation of the buildfile.
    async fn eval_build_file(&self, packages: &Package) -> SharedResult<Arc<EvaluationResult>>;

    /// Resolves a target pattern.
    async fn resolve_target_patterns(
        &self,
        pattern: &[&str],
    ) -> anyhow::Result<ResolvedPattern<TargetName>>;

    async fn eval_file_literal(&self, literal: &str) -> anyhow::Result<FileSet>;

    // Get all enclosing packages needed to compute owner function.
    // This always includes the immediate enclosing package of the path but can also include
    // all parent packages if the package matches `project.package_boundary_exceptions` buckconfig.
    async fn get_enclosing_packages(&self, path: &CellPath) -> anyhow::Result<Vec<Package>>;
}

#[async_trait]
pub trait QueryLiterals<T: QueryTarget>: Send + Sync {
    async fn eval_literals(&self, literals: &[&str]) -> anyhow::Result<TargetSet<T>>;
}

pub struct UqueryEnvironment<'c> {
    delegate: Arc<dyn UqueryDelegate + 'c>,
    literals: Arc<dyn QueryLiterals<TargetNode> + 'c>,
}

pub struct PreresolvedQueryLiterals<T: QueryTarget> {
    resolved_literals: HashMap<String, SharedResult<TargetSet<T>>>,
}

impl<T: QueryTarget> PreresolvedQueryLiterals<T> {
    pub fn new(resolved_literals: HashMap<String, SharedResult<TargetSet<T>>>) -> Self {
        Self { resolved_literals }
    }

    pub async fn pre_resolve(base: &dyn QueryLiterals<T>, literals: &[String]) -> Self {
        let futs = literals
            .iter()
            .map(|lit| async move { (lit.to_owned(), base.eval_literals(&[lit]).await) });
        let mut resolved_literals = HashMap::new();
        for (literal, result) in futures::future::join_all(futs).await {
            resolved_literals.insert(literal, result.shared_error());
        }
        Self { resolved_literals }
    }
}

#[async_trait]
impl<'c, T: QueryTarget> QueryLiterals<T> for PreresolvedQueryLiterals<T> {
    async fn eval_literals(&self, literals: &[&str]) -> anyhow::Result<TargetSet<T>> {
        let mut targets = TargetSet::new();
        for lit in literals {
            let resolved = match self
                .resolved_literals
                .get(*lit)
                .ok_or_else(|| QueryLiteralResolutionError::LiteralMissing((*lit).to_owned()))?
            {
                Ok(v) => v,
                Err(e) => return Err(e.dupe().into()),
            };
            for target in resolved.iter() {
                targets.insert(target.dupe());
            }
        }
        Ok(targets)
    }
}

impl<'c> UqueryEnvironment<'c> {
    pub fn new(
        delegate: Arc<dyn UqueryDelegate + 'c>,
        literals: Arc<dyn QueryLiterals<TargetNode> + 'c>,
    ) -> Self {
        Self { delegate, literals }
    }

    pub fn describe() -> QueryEnvironmentDescription {
        QueryEnvironmentDescription {
            name: "Uquery Environment".to_owned(),
            mods: vec![DefaultQueryFunctions::<Self>::describe()],
        }
    }

    async fn get_node(&self, target: &TargetLabel) -> anyhow::Result<TargetNode> {
        let package = self
            .delegate
            .eval_build_file(target.pkg())
            .await
            .with_context(|| format!("when looking up `{}``", target))?;
        let node = match package.targets().get(target.name()) {
            Some(target) => target,
            None => {
                return Err(QueryEnvironmentError::missing_target(
                    target,
                    package.targets().keys(),
                )
                .into());
            }
        };
        Ok(node.dupe())
    }
}

#[async_trait]
impl<'c> QueryEnvironment for UqueryEnvironment<'c> {
    type Target = TargetNode;

    async fn get_node(&self, node_ref: TargetLabel) -> anyhow::Result<Self::Target> {
        UqueryEnvironment::get_node(self, &node_ref).await
    }

    async fn eval_literals(&self, literals: &[&str]) -> anyhow::Result<TargetSet<TargetNode>> {
        self.literals.eval_literals(literals).await
    }

    async fn eval_file_literal(&self, literal: &str) -> anyhow::Result<FileSet> {
        self.delegate.eval_file_literal(literal).await
    }

    async fn dfs_postorder(
        &self,
        root: &TargetSet<TargetNode>,
        traversal_delegate: &mut dyn AsyncTraversalDelegate<TargetNode>,
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
        let mut result: TargetSet<Self::Target> = TargetSet::new();
        for path in paths.iter() {
            // need to explicitly track this rather than checking for changes to result set since the owner might
            // already be in the set.
            let mut found_owner = false;
            match self.delegate.get_enclosing_packages(path).await {
                Ok(packages) => {
                    let package_futs = packages.map(|package| async move {
                        // TODO(cjhopman): We should make sure that the file exists.
                        let targets = self.delegate.eval_build_file(package).await?;

                        let owner_targets: Vec<Self::Target> = targets
                            .targets()
                            .values()
                            .filter_map(|node| {
                                for input in node.inputs() {
                                    if &input == path {
                                        return Some(node.dupe());
                                        // this intentionally breaks out of the loop. We don't need to look at the
                                        // other inputs of this target, but it's possible for a single file to be owned by
                                        // multiple targets.
                                    }
                                }
                                None
                            })
                            .collect();
                        anyhow::Ok(owner_targets)
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
impl<'a> AsyncNodeLookup<TargetNode> for UqueryEnvironment<'a> {
    async fn get(&self, label: &TargetLabel) -> anyhow::Result<TargetNode> {
        self.get_node(label).await
    }
}
