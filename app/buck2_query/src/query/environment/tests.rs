/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![cfg(test)]

use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use buck2_error::internal_error;
use buck2_query::query::traversal::NodeLookup;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::async_depth_limited_traversal;
use derive_more::Display;
use derive_more::From;
use indexmap::IndexSet;

use super::*;

#[derive(Debug, Copy, Clone, Dupe, Eq, PartialEq, Hash, Display, From)]
struct TestTargetId(u64);

impl NodeKey for TestTargetId {}

#[derive(Debug, Copy, Clone, Dupe, Eq, PartialEq, Hash, Display)]
struct TestTargetAttr;

#[derive(Clone, Dupe, Eq, PartialEq)]
struct TestTarget {
    id: TestTargetId,
    deps: Arc<IndexSet<TestTargetId>>,
}

/// Custom debug to make the test output more readable
impl fmt::Debug for TestTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id.0)
    }
}

impl LabeledNode for TestTarget {
    type Key = TestTargetId;

    fn node_key(&self) -> &Self::Key {
        &self.id
    }
}

impl QueryTarget for TestTarget {
    type Attr<'a> = TestTargetAttr;

    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(&self, _func: F) -> Result<(), E> {
        unimplemented!()
    }

    fn rule_type(&self) -> Cow<'_, str> {
        unimplemented!()
    }

    fn name(&self) -> Cow<'_, str> {
        unimplemented!()
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        unimplemented!()
    }

    fn deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        Box::new(self.deps.iter())
    }

    fn exec_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        Box::new(std::iter::empty())
    }

    fn target_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        Box::new(std::iter::empty())
    }

    fn configuration_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        Box::new(std::iter::empty())
    }

    fn toolchain_deps<'a>(&'a self) -> impl Iterator<Item = &'a Self::Key> + Send + 'a {
        Box::new(std::iter::empty())
    }

    fn attr_any_matches(
        _attr: &Self::Attr<'_>,
        _filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        unimplemented!()
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        _func: F,
    ) -> Result<(), E> {
        unimplemented!()
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        _func: F,
    ) -> Result<(), E> {
        unimplemented!()
    }

    fn defined_attrs_for_each<E, F: FnMut(&str, &Self::Attr<'_>) -> Result<(), E>>(
        &self,
        _func: F,
    ) -> Result<(), E> {
        unimplemented!()
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, _key: &str, _func: F) -> R {
        unimplemented!()
    }

    fn map_any_attr<R, F: FnMut(Option<&Self::Attr<'_>>) -> R>(&self, _key: &str, _func: F) -> R {
        unimplemented!()
    }
}

struct TestEnv {
    graph: HashMap<TestTargetId, TestTarget>,
}

impl NodeLookup<TestTarget> for TestEnv {
    fn get(&self, label: &<TestTarget as LabeledNode>::Key) -> buck2_error::Result<TestTarget> {
        self.graph
            .get(label)
            .duped()
            .ok_or_else(|| internal_error!("Invalid node: {label:?}"))
    }
}

#[async_trait]
impl AsyncNodeLookup<TestTarget> for TestEnv {
    async fn get(
        &self,
        label: &<TestTarget as LabeledNode>::Key,
    ) -> buck2_error::Result<TestTarget> {
        self.graph
            .get(label)
            .duped()
            .ok_or_else(|| internal_error!("Invalid node: {label:?}"))
    }
}

#[async_trait]
impl QueryEnvironment for TestEnv {
    type Target = TestTarget;

    async fn get_node(
        &self,
        node_ref: &<Self::Target as LabeledNode>::Key,
    ) -> buck2_error::Result<Self::Target> {
        <Self as NodeLookup<TestTarget>>::get(self, node_ref)
    }

    async fn get_node_for_default_configured_target(
        &self,
        _node_ref: &<Self::Target as LabeledNode>::Key,
    ) -> buck2_error::Result<MaybeCompatible<Self::Target>> {
        unimplemented!()
    }

    async fn eval_literals(
        &self,
        _literal: &[&str],
    ) -> buck2_error::Result<TargetSet<Self::Target>> {
        unimplemented!()
    }

    async fn eval_file_literal(&self, _literal: &str) -> buck2_error::Result<FileSet> {
        unimplemented!()
    }

    async fn dfs_postorder(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: impl AsyncChildVisitor<Self::Target>,
        visit: impl FnMut(Self::Target) -> buck2_error::Result<()> + Send,
    ) -> buck2_error::Result<()> {
        // TODO: Should this be part of QueryEnvironment's default impl?
        async_depth_first_postorder_traversal(self, root.iter_names(), delegate, visit).await
    }

    async fn depth_limited_traversal(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: impl AsyncChildVisitor<Self::Target>,
        visit: impl FnMut(Self::Target) -> buck2_error::Result<()> + Send,
        depth: u32,
    ) -> buck2_error::Result<()> {
        async_depth_limited_traversal(self, root.iter_names(), delegate, visit, depth).await
    }

    async fn owner(&self, _paths: &FileSet) -> buck2_error::Result<TargetSet<Self::Target>> {
        unimplemented!()
    }

    async fn targets_in_buildfile(
        &self,
        _paths: &FileSet,
    ) -> buck2_error::Result<TargetSet<Self::Target>> {
        unimplemented!()
    }
}

impl TestEnv {
    /// A helper to get e.g. stuff like "1,2,3" into a TargetSet.
    fn set(&self, entries: &str) -> buck2_error::Result<TargetSet<TestTarget>> {
        let mut set = TargetSet::new();
        for c in entries.split(',') {
            let id = TestTargetId(c.parse().buck_error_context("Invalid ID")?);
            set.insert(<Self as NodeLookup<TestTarget>>::get(self, &id)?);
        }
        Ok(set)
    }
}

#[derive(Default)]
pub struct TestEnvBuilder {
    graph: HashMap<u64, IndexSet<u64>>,
}

impl TestEnvBuilder {
    fn edge(&mut self, from: u64, to: u64) {
        self.graph.entry(from).or_default().insert(to);
        self.graph.entry(to).or_default();
    }

    fn build(&self) -> TestEnv {
        TestEnv {
            graph: self
                .graph
                .iter()
                .map(|(id, vs)| {
                    let id = TestTargetId(*id);
                    let deps = Arc::new(vs.iter().map(|v| TestTargetId(*v)).collect());
                    (id, TestTarget { id, deps })
                })
                .collect(),
        }
    }
}

#[tokio::test]
async fn test_one_path() -> buck2_error::Result<()> {
    let mut env = TestEnvBuilder::default();
    // The actual path
    env.edge(1, 2);
    env.edge(2, 3);
    // Some unused edges
    env.edge(3, 4);
    env.edge(1, 10);
    env.edge(1, 12);
    let env = env.build();

    let path = env.allpaths(&env.set("1")?, &env.set("3")?, None).await?;
    let expected = env.set("1,2,3")?;
    assert_eq!(path, expected);

    let path = env.somepath(&env.set("1")?, &env.set("3")?, None).await?;
    let expected = env.set("1,2,3")?;
    assert_eq!(path, expected);

    Ok(())
}

#[tokio::test]
async fn test_many_paths() -> buck2_error::Result<()> {
    let mut env = TestEnvBuilder::default();
    env.edge(1, 2);
    env.edge(2, 3);
    env.edge(1, 10);
    env.edge(10, 11);
    env.edge(11, 3);
    // More unused edges
    env.edge(3, 4);
    env.edge(10, 20);
    let env = env.build();

    let path = env.allpaths(&env.set("1")?, &env.set("3")?, None).await?;
    let expected = env.set("1,10,11,2,3")?;
    assert_eq!(path, expected);

    let path = env.somepath(&env.set("1")?, &env.set("3")?, None).await?;
    let expected = env.set("1,2,3")?;
    assert_eq!(path, expected);

    Ok(())
}

#[tokio::test]
async fn test_distinct_paths() -> buck2_error::Result<()> {
    let mut env = TestEnvBuilder::default();
    env.edge(1, 10);
    env.edge(10, 100);
    env.edge(2, 20);
    env.edge(20, 200);
    let env = env.build();

    let path = env
        .allpaths(&env.set("1,2")?, &env.set("100,200")?, None)
        .await?;
    let expected = env.set("2,20,200,1,10,100")?;
    assert_eq!(path, expected);

    // Same as above
    let path = env
        .somepath(&env.set("1,2")?, &env.set("100,200")?, None)
        .await?;
    let expected = env.set("1,10,100")?;
    assert_eq!(path, expected);

    Ok(())
}

#[tokio::test]
async fn test_no_path() -> buck2_error::Result<()> {
    let mut env = TestEnvBuilder::default();
    env.edge(1, 10);
    env.edge(2, 20);
    let env = env.build();

    let path = env.allpaths(&env.set("1")?, &env.set("20")?, None).await?;
    let expected = TargetSet::new();
    assert_eq!(path, expected);

    let path = env.somepath(&env.set("1")?, &env.set("20")?, None).await?;
    let expected = TargetSet::new();
    assert_eq!(path, expected);

    Ok(())
}

#[tokio::test]
async fn test_nested_paths() -> buck2_error::Result<()> {
    let mut env = TestEnvBuilder::default();
    env.edge(1, 2);
    env.edge(2, 3);
    env.edge(3, 4);
    let env = env.build();

    let path = env.allpaths(&env.set("1")?, &env.set("2,4")?, None).await?;
    assert_eq!(path, env.set("1,2,3,4")?);

    let path = env.somepath(&env.set("1")?, &env.set("2,4")?, None).await?;
    assert_eq!(path, env.set("1,2")?);

    Ok(())
}

#[tokio::test]
async fn test_paths_with_cycles_present() -> buck2_error::Result<()> {
    let mut env = TestEnvBuilder::default();
    env.edge(1, 2);
    env.edge(2, 3);
    env.edge(3, 4);
    env.edge(4, 5);
    // Introduce cycles.
    env.edge(4, 1);
    env.edge(4, 3);
    let env = env.build();

    let path = env.allpaths(&env.set("3")?, &env.set("4")?, None).await?;
    assert_eq!(path, env.set("1,2,3,4")?);

    let path = env.allpaths(&env.set("1")?, &env.set("1")?, None).await?;
    assert_eq!(path, env.set("2,3,4,1")?);

    let path = env.allpaths(&env.set("1")?, &env.set("5")?, None).await?;
    assert_eq!(path, env.set("1,2,3,4,5")?);

    let path = env
        .rdeps(&env.set("1")?, &env.set("3")?, Some(2), None)
        .await?;
    assert_eq!(path, env.set("4,1,2,3")?);

    Ok(())
}

#[tokio::test]
async fn test_rdeps() -> buck2_error::Result<()> {
    let mut env = TestEnvBuilder::default();
    env.edge(1, 2);
    env.edge(2, 3);
    env.edge(3, 4); // Dead end.
    env.edge(4, 5);
    env.edge(1, 3); // Shortcut.
    env.edge(3, 6);
    let env = env.build();

    let path = env
        .rdeps(&env.set("1")?, &env.set("6")?, Some(0), None)
        .await?;
    assert_eq!(path, env.set("6")?);

    let path = env
        .rdeps(&env.set("1")?, &env.set("6")?, Some(1), None)
        .await?;
    assert_eq!(path, env.set("3,6")?);

    let path = env
        .rdeps(&env.set("1")?, &env.set("6")?, Some(2), None)
        .await?;
    assert_eq!(path, env.set("1,2,3,6")?);

    let path = env
        .rdeps(&env.set("1")?, &env.set("6")?, Some(3), None)
        .await?;
    assert_eq!(path, env.set("1,2,3,6")?);

    let path = env
        .rdeps(&env.set("1")?, &env.set("6")?, Some(4), None)
        .await?;
    assert_eq!(path, env.set("1,2,3,6")?);

    let path = env
        .rdeps(&env.set("1")?, &env.set("6")?, None, None)
        .await?;
    assert_eq!(path, env.set("1,2,3,6")?);

    Ok(())
}
