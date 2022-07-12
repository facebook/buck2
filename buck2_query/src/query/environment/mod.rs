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

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::Package;
use buck2_core::target::TargetLabel;
use futures::stream::FuturesUnordered;
use futures::stream::TryStreamExt;
use gazebo::prelude::*;
use serde::Serialize;
use thiserror::Error;

use crate::query::syntax::simple::eval::error::QueryError;
use crate::query::syntax::simple::eval::file_set::FileSet;
use crate::query::syntax::simple::eval::set::TargetSet;
use crate::query::traversal::AsyncTraversalDelegate;
use crate::query::traversal::ChildVisitor;

mod tests;

#[derive(Error, Debug)]
pub enum QueryEnvironmentError {
    #[error("Missing target `{}`. Targets in the package: <{}>", .0, .1.join(", "))]
    MissingTargetError(String, Vec<String>),
    #[error("Expected package `{0}` to be available in traversal.")]
    TraversalMissingPackage(Package),
    #[error("Dependency cycle, didn't manage to visit `{0}` which is a dependency of `{1}`")]
    DependencyCycle(String, String),
}

impl QueryEnvironmentError {
    pub fn missing_target<T: NodeLabel, S: AsRef<str>, Iter: Iterator<Item = S>>(
        target: &T,
        package_targets: Iter,
    ) -> Self {
        let existing = package_targets
            .map(|e| format!("`{}`", e.as_ref()))
            .collect();
        Self::MissingTargetError(target.to_string(), existing)
    }
}

pub trait NodeLabel: Clone + Hash + PartialEq + Eq + Debug + Display + Send + Sync {}
pub trait ConfiguredOrUnconfiguredTargetLabel: NodeLabel {
    fn unconfigured_label(&self) -> &TargetLabel;
}

pub trait LabeledNode: Dupe + Send + Sync + 'static {
    type NodeRef: NodeLabel;

    fn node_ref(&self) -> &Self::NodeRef;
}

pub struct QueryTargets {}

impl QueryTargets {
    /// Used to process all the attrs of a node (both the normal rule attrs and the "special" attrs). Applies
    /// a function to the attrs instead of returning an iterator as some of them are owned and some are refs
    /// into the node.
    pub fn for_all_attrs<E, T: QueryTarget, F: FnMut(&str, &T::Attr) -> Result<(), E>>(
        target: &T,
        mut func: F,
    ) -> Result<(), E> {
        target.special_attrs_for_each(&mut func)?;
        target.attrs_for_each(&mut func)?;
        Ok(())
    }
}

pub trait QueryTarget: Dupe + Send + Sync + 'static {
    type NodeRef: NodeLabel;
    type Attr: ?Sized + Display + Debug + Serialize;

    fn node_ref(&self) -> &Self::NodeRef;

    /// Returns the input files for this node.
    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(&self, func: F) -> Result<(), E>;

    fn rule_type(&self) -> Cow<str>;

    /// Return the path to the buildfile that defines this target, e.g. `fbcode//foo/bar/TARGETS`
    fn buildfile_path(&self) -> &BuildFilePath;

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a>;

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a>;

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a>;

    fn tests<'a>(&'a self) -> Option<Box<dyn Iterator<Item = Self::NodeRef> + Send + 'a>> {
        None
    }

    fn attr_any_matches(
        attr: &Self::Attr,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool>;

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        func: F,
    ) -> Result<(), E>;

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        func: F,
    ) -> Result<(), E>;

    fn map_attr<R, F: FnMut(Option<&Self::Attr>) -> R>(&self, key: &str, func: F) -> R;

    fn call_stack(&self) -> Option<String>;
}

impl<T: QueryTarget> LabeledNode for T {
    type NodeRef = T::NodeRef;

    fn node_ref(&self) -> &Self::NodeRef {
        QueryTarget::node_ref(self)
    }
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
        node_ref: &<Self::Target as QueryTarget>::NodeRef,
    ) -> anyhow::Result<Self::Target>;

    /// Evaluates a literal target pattern. See buck2_common::pattern
    async fn eval_literals(&self, literal: &[&str]) -> anyhow::Result<TargetSet<Self::Target>>;

    /// Evaluates a file literal
    async fn eval_file_literal(&self, literal: &str) -> anyhow::Result<FileSet>;

    /// Performs a depth first traversal, with a post-order callback. The
    /// delegate defines the traversal and receives the callback.
    async fn dfs_postorder(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
    ) -> anyhow::Result<()>;

    async fn depth_limited_traversal(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
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
                func: &mut dyn ChildVisitor<Q>,
            ) -> anyhow::Result<()> {
                // Stop adding more children if we are putting a path back together.
                if self.path.len() > 0 || self.to.contains(target.node_ref()) {
                    return Ok(());
                }
                let res: anyhow::Result<_> = try {
                    for dep in target.deps() {
                        func.visit(dep.clone())?;
                    }
                };
                res.with_context(|| format!("When traversing children of `{}`", target.node_ref()))
            }
        }

        let mut delegate = Delegate {
            path: TargetSet::new(),
            to,
        };
        self.dfs_postorder(from, &mut delegate).await?;
        Ok(delegate.path)
    }

    async fn rbuildfiles(&self) -> anyhow::Result<()> {
        Ok(())
    }

    async fn rdeps(
        &self,
        universe: &TargetSet<Self::Target>,
        from: &TargetSet<Self::Target>,
        depth: Option<i32>,
    ) -> anyhow::Result<TargetSet<Self::Target>> {
        let mut deps = TargetSet::new();

        struct Delegate<'a, Q: QueryTarget> {
            from: &'a TargetSet<Q>,
            max_distance: Option<usize>,

            result: &'a mut TargetSet<Q>,
            distance: HashMap<Q::NodeRef, Option<usize>>,
        }

        #[async_trait]
        impl<'a, Q: QueryTarget> AsyncTraversalDelegate<Q> for Delegate<'a, Q> {
            fn visit(&mut self, target: Q) -> anyhow::Result<()> {
                let node_ref = target.node_ref();
                let distance = if self.from.contains(node_ref) {
                    Some(0)
                } else {
                    let mut distance = None;
                    for dep in target.deps() {
                        let dep_distance = *self.distance.get(dep).ok_or_else(|| {
                            QueryEnvironmentError::DependencyCycle(
                                dep.to_string(),
                                node_ref.to_string(),
                            )
                        })?;

                        distance = match (distance, dep_distance) {
                            (None, v) => v,
                            (v, None) => v,
                            (Some(l), Some(r)) => Some(std::cmp::min(l, r)),
                        };
                    }
                    distance.map(|v| v + 1)
                };
                self.distance.insert(node_ref.clone(), distance);
                if let Some(distance) = distance {
                    if self.max_distance.is_none() || distance <= self.max_distance.unwrap() {
                        self.result.insert(target);
                    }
                }
                Ok(())
            }

            async fn for_each_child(
                &mut self,
                target: &Q,
                func: &mut dyn ChildVisitor<Q>,
            ) -> anyhow::Result<()> {
                let res: anyhow::Result<_> = try {
                    for dep in target.deps() {
                        func.visit(dep.clone())?;
                    }
                };
                res.with_context(|| format!("When traversing children of `{}`", target.node_ref()))
            }
        }

        self.dfs_postorder(
            universe,
            &mut Delegate {
                result: &mut deps,
                from,
                max_distance: depth.map(|v| v as usize),
                distance: HashMap::new(),
            },
        )
        .await?;

        Ok(deps)
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
                            QueryTarget::node_ref(target),
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

    async fn deps(
        &self,
        targets: &TargetSet<Self::Target>,
        depth: Option<i32>,
        filter: Option<&dyn TraversalFilter<Self::Target>>,
    ) -> anyhow::Result<TargetSet<Self::Target>> {
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
                func: &mut dyn ChildVisitor<Q>,
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
                res.with_context(|| format!("When traversing children of `{}`", target.node_ref()))
            }
        }

        match depth {
            // For unbounded traversals, buck1 recommends specifying a large value. We'll accept either a negative (like -1) or
            // a large value as unbounded. We can't just call it optional because args are positional only in the query syntax
            // and so to specify a filter you need to specify a depth.
            Some(v) if (0..1_000_000_000).contains(&v) => {
                self.depth_limited_traversal(
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
                self.dfs_postorder(
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

    async fn owner(&self, _paths: &FileSet) -> anyhow::Result<TargetSet<Self::Target>>;
}
