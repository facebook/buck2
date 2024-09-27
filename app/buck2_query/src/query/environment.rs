/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt::Debug;
use std::iter;

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::package::PackageLabel;
use dupe::Dupe;
use dupe::OptionDupedExt;
use futures::stream::FuturesUnordered;
use futures::stream::TryStreamExt;

use crate::query::graph::async_bfs::async_bfs_find_path;
use crate::query::graph::graph::Graph;
use crate::query::graph::node::LabeledNode;
use crate::query::graph::node::NodeKey;
use crate::query::graph::successors::AsyncChildVisitor;
use crate::query::graph::successors::GraphSuccessors;
use crate::query::syntax::simple::eval::error::QueryError;
use crate::query::syntax::simple::eval::file_set::FileSet;
use crate::query::syntax::simple::eval::set::TargetSet;
use crate::query::traversal::AsyncNodeLookup;
use crate::query::traversal::ChildVisitor;
mod tests;

#[derive(buck2_error::Error, Debug)]
pub enum QueryEnvironmentError {
    #[error("Missing target `{}`. Targets in the package: <{}>", .0, .1.join(", "))]
    MissingTargetError(String, Vec<String>),
    #[error("Expected package `{0}` to be available in traversal.")]
    TraversalMissingPackage(PackageLabel),
}

impl QueryEnvironmentError {
    pub fn missing_target<T: NodeKey, S: AsRef<str>, Iter: IntoIterator<Item = S>>(
        target: &T,
        package_targets: Iter,
    ) -> Self {
        let existing = package_targets
            .into_iter()
            .map(|e| format!("`{}`", e.as_ref()))
            .collect();
        Self::MissingTargetError(target.to_string(), existing)
    }
}

pub struct QueryTargets {}

impl QueryTargets {
    /// Used to process all the attrs of a node (both the normal rule attrs and the "special" attrs). Applies
    /// a function to the attrs instead of returning an iterator as some of them are owned and some are refs
    /// into the node.
    pub fn for_all_attrs<E, T: QueryTarget, F: FnMut(&str, &T::Attr<'_>) -> Result<(), E>>(
        target: &T,
        mut func: F,
    ) -> Result<(), E> {
        target.special_attrs_for_each(&mut func)?;
        target.attrs_for_each(&mut func)?;
        Ok(())
    }
}

#[derive(Default, Clone, Dupe)]
pub struct AttrFmtOptions {
    pub exclude_quotes: bool,
}

pub trait QueryTarget: LabeledNode + Dupe + Send + Sync + 'static {
    type Attr<'a>: ?Sized + Debug + 'a;

    /// `filter()` function uses this.
    fn label_for_filter(&self) -> String {
        self.node_key().to_string()
    }

    /// Returns the input files for this node.
    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(&self, func: F) -> Result<(), E>;

    fn rule_type(&self) -> Cow<str>;

    fn name(&self) -> Cow<str>;

    /// Return the path to the buildfile that defines this target, e.g. `fbcode//foo/bar/TARGETS`
    fn buildfile_path(&self) -> &BuildFilePath;

    fn deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a;

    fn exec_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a;

    fn target_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a;

    fn configuration_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a;

    fn toolchain_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a;

    fn tests<'a>(&'a self) -> Option<impl Iterator<Item = Self::Key> + Send + 'a> {
        None::<iter::Empty<Self::Key>>
    }

    fn attr_any_matches(
        attr: &Self::Attr<'_>,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool>;

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        func: F,
    ) -> Result<(), E>;

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        func: F,
    ) -> Result<(), E>;

    fn defined_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        func: F,
    ) -> Result<(), E>;

    fn map_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, key: &str, func: F) -> R;
}

#[async_trait]
pub trait TraversalFilter<T: QueryTarget>: Send + Sync {
    /// Returns a the children that pass this filter.
    async fn get_children(&self, target: &T) -> anyhow::Result<TargetSet<T>>;
}

/// The environment of a Buck query that can evaluate queries to produce a
/// result.
#[async_trait]
pub trait QueryEnvironment: Send + Sync {
    type Target: QueryTarget;

    async fn get_node(
        &self,
        node_ref: &<Self::Target as LabeledNode>::Key,
    ) -> anyhow::Result<Self::Target>;

    async fn get_node_for_default_configured_target(
        &self,
        node_ref: &<Self::Target as LabeledNode>::Key,
    ) -> anyhow::Result<MaybeCompatible<Self::Target>>;

    /// Evaluates a literal target pattern. See buck2_common::pattern
    async fn eval_literals(&self, literal: &[&str]) -> anyhow::Result<TargetSet<Self::Target>>;

    /// Evaluates a file literal
    async fn eval_file_literal(&self, literal: &str) -> anyhow::Result<FileSet>;

    /// Performs a depth first traversal, with a post-order callback. The
    /// delegate defines the traversal and receives the callback.
    async fn dfs_postorder(
        &self,
        root: &TargetSet<Self::Target>,
        successors: impl AsyncChildVisitor<Self::Target>,
        visit: impl FnMut(Self::Target) -> anyhow::Result<()> + Send,
    ) -> anyhow::Result<()>;

    async fn depth_limited_traversal(
        &self,
        root: &TargetSet<Self::Target>,
        successors: impl AsyncChildVisitor<Self::Target>,
        visit: impl FnMut(Self::Target) -> anyhow::Result<()> + Send,
        depth: u32,
    ) -> anyhow::Result<()>;

    async fn allpaths(
        &self,
        from: &TargetSet<Self::Target>,
        to: &TargetSet<Self::Target>,
    ) -> anyhow::Result<TargetSet<Self::Target>> {
        self.rdeps(from, to, None).await
    }

    async fn somepath(
        &self,
        from: &TargetSet<Self::Target>,
        to: &TargetSet<Self::Target>,
    ) -> anyhow::Result<TargetSet<Self::Target>> {
        let path = async_bfs_find_path(
            from.iter(),
            QueryEnvironmentAsNodeLookup { env: self },
            QueryTargetDepsSuccessors,
            |t| to.get(t).duped(),
        )
        .await?
        .unwrap_or_default();

        let target_set = TargetSet::from_iter(path);
        Ok(target_set)
    }

    async fn allbuildfiles(&self, _universe: &TargetSet<Self::Target>) -> anyhow::Result<FileSet> {
        Err(anyhow::anyhow!(QueryError::FunctionUnimplemented(
            "allbuildfiles() is implemented only for uquery and cquery.",
        )))
    }

    async fn rbuildfiles(&self, _universe: &FileSet, _argset: &FileSet) -> anyhow::Result<FileSet> {
        Err(anyhow::anyhow!(QueryError::FunctionUnimplemented(
            "rbuildfiles() is implemented only for uquery and cquery."
        )))
    }

    async fn rdeps(
        &self,
        universe: &TargetSet<Self::Target>,
        from: &TargetSet<Self::Target>,
        depth: Option<i32>,
    ) -> anyhow::Result<TargetSet<Self::Target>> {
        let graph = Graph::build_stable_dfs(
            &QueryEnvironmentAsNodeLookup { env: self },
            universe.iter().map(|n| n.node_key().clone()),
            QueryTargetDepsSuccessors,
        )
        .await?;

        let graph = graph.reverse();

        let mut rdeps = TargetSet::new();

        let mut visit = |target| {
            rdeps.insert_unique_unchecked(target);
            Ok(())
        };

        let roots_in_universe = from.filter(|t| Ok(graph.get(t.node_key()).is_some()))?;

        match depth {
            // For unbounded traversals, buck1 recommends specifying a large value. We'll accept either a negative (like -1) or
            // a large value as unbounded. We can't just call it optional because args are positional only in the query syntax
            // and so to specify a filter you need to specify a depth.
            Some(v) if (0..1_000_000_000).contains(&v) => {
                let graph = graph.take_max_depth(
                    roots_in_universe.iter().map(|t| t.node_key().clone()),
                    v as u32,
                );
                graph.depth_first_postorder_traversal(
                    roots_in_universe.iter().map(|t| t.node_key().clone()),
                    |t| visit(t.clone()),
                )?;
            }
            _ => {
                graph.depth_first_postorder_traversal(
                    roots_in_universe.iter().map(|t| t.node_key().clone()),
                    |t| visit(t.clone()),
                )?;
            }
        }

        Ok(rdeps)
    }

    async fn testsof(
        &self,
        targets: &TargetSet<Self::Target>,
    ) -> anyhow::Result<TargetSet<Self::Target>> {
        let target_tests = targets
            .iter()
            .map(|target| {
                let tests = target
                    .tests()
                    .ok_or(QueryError::FunctionUnimplemented("testsof"))?;

                anyhow::Ok((target, tests))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut futs = target_tests
            .into_iter()
            .flat_map(|(target, tests)| {
                tests.into_iter().map(move |test| async move {
                    let test = self.get_node(&test).await.with_context(|| {
                        format!(
                            "Error getting test of target {}",
                            LabeledNode::node_key(target),
                        )
                    })?;
                    anyhow::Ok(test)
                })
            })
            .collect::<FuturesUnordered<_>>();

        let mut ret = TargetSet::new();
        while let Some(test) = futs.try_next().await? {
            ret.insert(test);
        }

        Ok(ret)
    }

    async fn testsof_with_default_target_platform(
        &self,
        targets: &TargetSet<Self::Target>,
    ) -> anyhow::Result<Vec<MaybeCompatible<Self::Target>>> {
        let target_tests = targets
            .iter()
            .map(|target| {
                let tests = target
                    .tests()
                    .ok_or(QueryError::FunctionUnimplemented("testsof"))?;

                anyhow::Ok((target, tests))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut futs = target_tests
            .into_iter()
            .flat_map(|(target, tests)| {
                tests.into_iter().map(move |test| async move {
                    let test = self
                        .get_node_for_default_configured_target(&test)
                        .await
                        .with_context(|| {
                            format!(
                                "Error getting test of target {}",
                                LabeledNode::node_key(target),
                            )
                        })?;
                    anyhow::Ok(test)
                })
            })
            .collect::<FuturesUnordered<_>>();

        let mut ret = Vec::new();
        while let Some(test) = futs.try_next().await? {
            ret.push(test);
        }

        Ok(ret)
    }

    async fn deps(
        &self,
        targets: &TargetSet<Self::Target>,
        depth: Option<i32>,
        filter: Option<&dyn TraversalFilter<Self::Target>>,
    ) -> anyhow::Result<TargetSet<Self::Target>> {
        deps(self, targets, depth, filter).await
    }

    async fn owner(&self, _paths: &FileSet) -> anyhow::Result<TargetSet<Self::Target>>;

    async fn targets_in_buildfile(
        &self,
        paths: &FileSet,
    ) -> anyhow::Result<TargetSet<Self::Target>>;
}

pub async fn deps<Env: QueryEnvironment + ?Sized>(
    env: &Env,
    targets: &TargetSet<Env::Target>,
    depth: Option<i32>,
    filter: Option<&dyn TraversalFilter<Env::Target>>,
) -> anyhow::Result<TargetSet<Env::Target>> {
    let mut deps = TargetSet::new();

    struct Delegate<'a, Q: QueryTarget> {
        filter: Option<&'a dyn TraversalFilter<Q>>,
    }

    let visit = |target| {
        deps.insert_unique_unchecked(target);
        Ok(())
    };

    impl<'a, Q: QueryTarget> AsyncChildVisitor<Q> for Delegate<'a, Q> {
        async fn for_each_child(
            &self,
            target: &Q,
            mut func: impl ChildVisitor<Q>,
        ) -> anyhow::Result<()> {
            let res: anyhow::Result<_> = try {
                match self.filter {
                    Some(filter) => {
                        for dep in filter.get_children(target).await?.iter() {
                            func.visit(dep.node_key())?;
                        }
                    }
                    None => {
                        for dep in target.deps() {
                            func.visit(dep)?;
                        }
                    }
                }
            };
            res.with_context(|| format!("Error traversing children of `{}`", target.node_key()))
        }
    }

    match depth {
        // For unbounded traversals, buck1 recommends specifying a large value. We'll accept either a negative (like -1) or
        // a large value as unbounded. We can't just call it optional because args are positional only in the query syntax
        // and so to specify a filter you need to specify a depth.
        Some(v) if (0..1_000_000_000).contains(&v) => {
            env.depth_limited_traversal(targets, Delegate { filter }, visit, v as u32)
                .await?;
        }
        _ => {
            env.dfs_postorder(targets, Delegate { filter }, visit)
                .await?;
        }
    }

    Ok(deps)
}

pub struct QueryTargetDepsSuccessors;

impl<T: QueryTarget> AsyncChildVisitor<T> for QueryTargetDepsSuccessors {
    async fn for_each_child(
        &self,
        node: &T,
        mut children: impl ChildVisitor<T>,
    ) -> anyhow::Result<()> {
        for dep in node.deps() {
            children.visit(dep)?;
        }
        Ok(())
    }
}

impl<T> GraphSuccessors<T> for QueryTargetDepsSuccessors
where
    T: QueryTarget<Key = T>,
{
    fn for_each_successor(&self, node: &T, mut cb: impl FnMut(&T)) {
        for dep in node.deps() {
            cb(dep);
        }
    }
}

pub struct QueryEnvironmentAsNodeLookup<'q, Q: QueryEnvironment + ?Sized> {
    pub env: &'q Q,
}

#[async_trait]
impl<'q, Q: QueryEnvironment + ?Sized> AsyncNodeLookup<Q::Target>
    for QueryEnvironmentAsNodeLookup<'q, Q>
{
    async fn get(&self, label: &<Q::Target as LabeledNode>::Key) -> anyhow::Result<Q::Target> {
        self.env.get_node(label).await
    }
}
