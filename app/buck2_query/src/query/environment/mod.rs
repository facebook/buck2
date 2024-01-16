/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::iter;

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::package::PackageLabel;
use buck2_core::target::label::TargetLabel;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::stream::TryStreamExt;
use starlark_map::ordered_set::OrderedSet;
use starlark_map::Hashed;

use crate::query::syntax::simple::eval::error::QueryError;
use crate::query::syntax::simple::eval::file_set::FileSet;
use crate::query::syntax::simple::eval::set::TargetSet;
use crate::query::traversal::AsyncTraversalDelegate;
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
    pub fn missing_target<T: NodeLabel, S: AsRef<str>, Iter: IntoIterator<Item = S>>(
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

pub trait NodeLabel: Clone + Hash + PartialEq + Eq + Debug + Display + Send + Sync {
    /// `filter()` function will use this.
    fn label_for_filter(&self) -> String {
        self.to_string()
    }
}

pub trait ConfiguredOrUnconfiguredTargetLabel: NodeLabel {
    fn unconfigured_label(&self) -> &TargetLabel;
}

pub trait LabeledNode: Dupe + Send + Sync + 'static {
    type NodeRef: NodeLabel;

    fn node_ref(&self) -> &Self::NodeRef;

    fn hashed_node_ref(&self) -> Hashed<&Self::NodeRef> {
        Hashed::new(self.node_ref())
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

pub trait QueryTarget: LabeledNode + Dupe + Send + Sync + 'static {
    type Attr<'a>: ?Sized + Debug + 'a;

    /// Returns the input files for this node.
    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(&self, func: F) -> Result<(), E>;

    fn rule_type(&self) -> Cow<str>;

    /// Return the path to the buildfile that defines this target, e.g. `fbcode//foo/bar/TARGETS`
    fn buildfile_path(&self) -> &BuildFilePath;

    fn deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::NodeRef> + Send + 'a;

    fn exec_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::NodeRef> + Send + 'a;

    fn target_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::NodeRef> + Send + 'a;

    fn tests<'a>(&'a self) -> Option<impl Iterator<Item = Self::NodeRef> + Send + 'a> {
        None::<iter::Empty<Self::NodeRef>>
    }

    fn attr_to_string_alternate(&self, attr: &Self::Attr<'_>) -> String;

    fn attr_serialize<S: serde::Serializer>(
        &self,
        attr: &Self::Attr<'_>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>;

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

    fn map_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, key: &str, func: F) -> R;

    fn call_stack(&self) -> Option<String>;
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
        node_ref: &<Self::Target as LabeledNode>::NodeRef,
    ) -> anyhow::Result<Self::Target>;

    async fn get_node_for_default_configured_target(
        &self,
        node_ref: &<Self::Target as LabeledNode>::NodeRef,
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
        delegate: &mut impl AsyncTraversalDelegate<Self::Target>,
    ) -> anyhow::Result<()>;

    async fn depth_limited_traversal(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: &mut impl AsyncTraversalDelegate<Self::Target>,
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
        struct Delegate<'a, Q: QueryTarget> {
            to: &'a TargetSet<Q>,
            /// Contains targets that were reached starting from `from` that have a path to `to`.
            path: TargetSet<Q>,
        }

        #[async_trait]
        impl<'a, Q: QueryTarget> AsyncTraversalDelegate<Q> for Delegate<'a, Q> {
            fn visit(&mut self, target: Q) -> anyhow::Result<()> {
                // NOTE: It would be better to just only post-order visit our parents, but that is
                // not possible because we push *all* children when visiting a node, so we will not
                // just post-visit all parents when we interrupt the search.
                // NOTE: We assert! around the insertions below because we know each node should
                // only be post-visited once but since we rely on `last()`, it matters so we check
                // it.

                if let Some(head) = self.path.last() {
                    if target.deps().any(|t| t == head.node_ref()) {
                        assert!(self.path.insert(target));
                    }
                    return Ok(());
                }

                if self.to.contains(target.node_ref()) {
                    assert!(self.path.insert(target));
                }

                Ok(())
            }

            async fn for_each_child(
                &mut self,
                target: &Q,
                func: &mut impl ChildVisitor<Q>,
            ) -> anyhow::Result<()> {
                // Stop adding more children if we are putting a path back together.
                if !self.path.is_empty() || self.to.contains(target.node_ref()) {
                    return Ok(());
                }
                let res: anyhow::Result<_> = try {
                    for dep in target.deps() {
                        func.visit(dep.clone())?;
                    }
                };
                res.with_context(|| format!("Error traversing children of `{}`", target.node_ref()))
            }
        }

        let mut delegate = Delegate {
            path: TargetSet::new(),
            to,
        };
        self.dfs_postorder(from, &mut delegate).await?;
        Ok(delegate.path)
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
        // First, we map all deps to their rdeps (parents).
        // This effectively allows traversing the graph later, in reverse (following dependency back-edges).
        struct ParentsCollectorDelegate<Q: QueryTarget> {
            parents: HashMap<Q::NodeRef, OrderedSet<Q::NodeRef>>,
            // Keep track of nodes in-universe so that, if any rdeps are collected out-of-universe,
            // we don't return them.
            nodes_in_universe: TargetSet<Q>,
        }

        #[async_trait]
        impl<Q: QueryTarget> AsyncTraversalDelegate<Q> for ParentsCollectorDelegate<Q> {
            fn visit(&mut self, target: Q) -> anyhow::Result<()> {
                self.nodes_in_universe.insert(target);
                Ok(())
            }

            async fn for_each_child(
                &mut self,
                target: &Q,
                func: &mut impl ChildVisitor<Q>,
            ) -> anyhow::Result<()> {
                for dep in target.deps() {
                    func.visit(dep.clone()).with_context(|| {
                        format!("Error traversing children of `{}`", target.node_ref())
                    })?;
                    self.parents
                        .entry(dep.clone())
                        .or_default()
                        .insert(target.node_ref().clone());
                }
                Ok(())
            }
        }

        let mut parents_collector_delegate = ParentsCollectorDelegate {
            parents: HashMap::new(),
            nodes_in_universe: TargetSet::new(),
        };

        self.dfs_postorder(universe, &mut parents_collector_delegate)
            .await?;

        // Now that we have a mapping of back-edges, traverse deps graph in reverse.
        struct ReverseDelegate<Q: QueryTarget> {
            rdeps: TargetSet<Q>,
            parents: HashMap<Q::NodeRef, OrderedSet<Q::NodeRef>>,
        }

        #[async_trait]
        impl<Q: QueryTarget> AsyncTraversalDelegate<Q> for ReverseDelegate<Q> {
            fn visit(&mut self, target: Q) -> anyhow::Result<()> {
                self.rdeps.insert(target);
                Ok(())
            }

            async fn for_each_child(
                &mut self,
                target: &Q,
                func: &mut impl ChildVisitor<Q>,
            ) -> anyhow::Result<()> {
                if let Some(parents) = self.parents.get(target.node_ref()) {
                    for parent in parents {
                        func.visit(parent.clone()).with_context(|| {
                            format!("Error traversing parents of `{}`", target.node_ref())
                        })?;
                    }
                }
                Ok(())
            }
        }

        let mut delegate = ReverseDelegate {
            rdeps: TargetSet::new(),
            parents: parents_collector_delegate.parents,
        };

        let roots_in_universe = from.intersect(&parents_collector_delegate.nodes_in_universe)?;

        match depth {
            // For unbounded traversals, buck1 recommends specifying a large value. We'll accept either a negative (like -1) or
            // a large value as unbounded. We can't just call it optional because args are positional only in the query syntax
            // and so to specify a filter you need to specify a depth.
            Some(v) if (0..1_000_000_000).contains(&v) => {
                self.depth_limited_traversal(&roots_in_universe, &mut delegate, v as u32)
                    .await?;
            }
            _ => {
                self.dfs_postorder(&roots_in_universe, &mut delegate)
                    .await?;
            }
        }

        Ok(delegate.rdeps)
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
                            LabeledNode::node_ref(target),
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
                                LabeledNode::node_ref(target),
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
}

pub async fn deps<Env: QueryEnvironment + ?Sized>(
    env: &Env,
    targets: &TargetSet<Env::Target>,
    depth: Option<i32>,
    filter: Option<&dyn TraversalFilter<Env::Target>>,
) -> anyhow::Result<TargetSet<Env::Target>> {
    let mut deps = TargetSet::new();

    struct Delegate<'a, Q: QueryTarget> {
        deps: &'a mut TargetSet<Q>,
        filter: Option<&'a dyn TraversalFilter<Q>>,
    }

    #[async_trait]
    impl<'a, Q: QueryTarget> AsyncTraversalDelegate<Q> for Delegate<'a, Q> {
        fn visit(&mut self, target: Q) -> anyhow::Result<()> {
            self.deps.insert(target);
            Ok(())
        }

        async fn for_each_child(
            &mut self,
            target: &Q,
            func: &mut impl ChildVisitor<Q>,
        ) -> anyhow::Result<()> {
            let res: anyhow::Result<_> = try {
                match self.filter {
                    Some(filter) => {
                        for dep in filter.get_children(target).await?.iter() {
                            func.visit(dep.node_ref().clone())?;
                        }
                    }
                    None => {
                        for dep in target.deps() {
                            func.visit(dep.clone())?;
                        }
                    }
                }
            };
            res.with_context(|| format!("Error traversing children of `{}`", target.node_ref()))
        }
    }

    match depth {
        // For unbounded traversals, buck1 recommends specifying a large value. We'll accept either a negative (like -1) or
        // a large value as unbounded. We can't just call it optional because args are positional only in the query syntax
        // and so to specify a filter you need to specify a depth.
        Some(v) if (0..1_000_000_000).contains(&v) => {
            env.depth_limited_traversal(
                targets,
                &mut Delegate {
                    deps: &mut deps,
                    filter,
                },
                v as u32,
            )
            .await?;
        }
        _ => {
            env.dfs_postorder(
                targets,
                &mut Delegate {
                    deps: &mut deps,
                    filter,
                },
            )
            .await?;
        }
    }

    Ok(deps)
}
