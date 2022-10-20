/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(test)]

use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::NodeLookup;
use derive_more::Display;
use derive_more::From;
use indexmap::IndexSet;

use super::*;
use crate::query::traversal::AsyncNodeLookup;

#[derive(Debug, Copy, Clone, Dupe, Eq, PartialEq, Hash, Display, From)]
struct TestTargetId(u64);

impl NodeLabel for TestTargetId {}

#[derive(Debug, Copy, Clone, Dupe, Eq, PartialEq, Hash, Display, Serialize)]
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
    type NodeRef = TestTargetId;

    fn node_ref(&self) -> &Self::NodeRef {
        &self.id
    }
}

impl QueryTarget for TestTarget {
    type Attr = TestTargetAttr;

    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(&self, _func: F) -> Result<(), E> {
        unimplemented!()
    }

    fn rule_type(&self) -> Cow<str> {
        unimplemented!()
    }

    fn buildfile_path(&self) -> &BuildFilePath {
        unimplemented!()
    }

    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        box self.deps.iter()
    }

    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        box std::iter::empty()
    }

    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        box std::iter::empty()
    }

    fn attr_any_matches(
        _attr: &Self::Attr,
        _filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
        unimplemented!()
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        _func: F,
    ) -> Result<(), E> {
        unimplemented!()
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        _func: F,
    ) -> Result<(), E> {
        unimplemented!()
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr>) -> R>(&self, _key: &str, _func: F) -> R {
        unimplemented!()
    }

    fn call_stack(&self) -> Option<String> {
        None
    }
}

struct TestEnv {
    graph: HashMap<TestTargetId, TestTarget>,
}

impl NodeLookup<TestTarget> for TestEnv {
    fn get(&self, label: &<TestTarget as LabeledNode>::NodeRef) -> anyhow::Result<TestTarget> {
        self.graph
            .get(label)
            .duped()
            .with_context(|| format!("Invalid node: {:?}", label))
    }
}

#[async_trait]
impl AsyncNodeLookup<TestTarget> for TestEnv {
    async fn get(
        &self,
        label: &<TestTarget as LabeledNode>::NodeRef,
    ) -> anyhow::Result<TestTarget> {
        self.graph
            .get(label)
            .duped()
            .with_context(|| format!("Invalid node: {:?}", label))
    }
}

#[async_trait]
impl QueryEnvironment for TestEnv {
    type Target = TestTarget;

    async fn get_node(
        &self,
        node_ref: &<Self::Target as LabeledNode>::NodeRef,
    ) -> anyhow::Result<Self::Target> {
        <Self as NodeLookup<TestTarget>>::get(self, node_ref)
    }

    async fn eval_literals(&self, _literal: &[&str]) -> anyhow::Result<TargetSet<Self::Target>> {
        unimplemented!()
    }

    async fn eval_file_literal(&self, _literal: &str) -> anyhow::Result<FileSet> {
        unimplemented!()
    }

    async fn dfs_postorder(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
    ) -> anyhow::Result<()> {
        // TODO: Should this be part of QueryEnvironment's default impl?
        async_depth_first_postorder_traversal(self, root.iter_names(), delegate).await
    }

    async fn depth_limited_traversal(
        &self,
        _root: &TargetSet<Self::Target>,
        _delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
        _depth: u32,
    ) -> anyhow::Result<()> {
        unimplemented!()
    }

    async fn owner(&self, _paths: &FileSet) -> anyhow::Result<TargetSet<Self::Target>> {
        unimplemented!()
    }
}

impl TestEnv {
    /// A helper to get e.g. stuff like "1,2,3" into a TargetSet.
    fn set(&self, entries: &str) -> anyhow::Result<TargetSet<TestTarget>> {
        let mut set = TargetSet::new();
        for c in entries.split(',') {
            let id = TestTargetId(c.parse().context("Invalid ID")?);
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
async fn test_one_path() -> anyhow::Result<()> {
    let mut env = TestEnvBuilder::default();
    // The actual path
    env.edge(1, 2);
    env.edge(2, 3);
    // Some unused edges
    env.edge(3, 4);
    env.edge(1, 10);
    env.edge(1, 12);
    let env = env.build();

    let path = env.allpaths(&env.set("1")?, &env.set("3")?).await?;
    let expected = env.set("3,2,1")?;
    assert_eq!(path, expected);

    let path = env.somepath(&env.set("1")?, &env.set("3")?).await?;
    let expected = env.set("3,2,1")?;
    assert_eq!(path, expected);

    Ok(())
}

#[tokio::test]
async fn test_many_paths() -> anyhow::Result<()> {
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

    let path = env.allpaths(&env.set("1")?, &env.set("3")?).await?;
    let expected = env.set("3,11,10,2,1")?;
    assert_eq!(path, expected);

    // We iterate with a stack so this is why we find this path
    let path = env.somepath(&env.set("1")?, &env.set("3")?).await?;
    let expected = env.set("3,11,10,1")?;
    assert_eq!(path, expected);

    Ok(())
}

#[tokio::test]
async fn test_distinct_paths() -> anyhow::Result<()> {
    let mut env = TestEnvBuilder::default();
    env.edge(1, 10);
    env.edge(10, 100);
    env.edge(2, 20);
    env.edge(20, 200);
    let env = env.build();

    let path = env.allpaths(&env.set("1,2")?, &env.set("100,200")?).await?;
    let expected = env.set("200,20,2,100,10,1")?;
    assert_eq!(path, expected);

    // Same as above
    let path = env.somepath(&env.set("1,2")?, &env.set("100,200")?).await?;
    let expected = env.set("200,20,2")?;
    assert_eq!(path, expected);

    Ok(())
}

#[tokio::test]
async fn test_no_path() -> anyhow::Result<()> {
    let mut env = TestEnvBuilder::default();
    env.edge(1, 10);
    env.edge(2, 20);
    let env = env.build();

    let path = env.allpaths(&env.set("1")?, &env.set("20")?).await?;
    let expected = TargetSet::new();
    assert_eq!(path, expected);

    let path = env.somepath(&env.set("1")?, &env.set("20")?).await?;
    let expected = TargetSet::new();
    assert_eq!(path, expected);

    Ok(())
}

#[tokio::test]
async fn test_nested_paths() -> anyhow::Result<()> {
    let mut env = TestEnvBuilder::default();
    env.edge(1, 2);
    env.edge(2, 3);
    env.edge(3, 4);
    let env = env.build();

    let path = env.allpaths(&env.set("1")?, &env.set("2,4")?).await?;
    assert_eq!(path, env.set("4,3,2,1")?);

    let path = env.somepath(&env.set("1")?, &env.set("2,4")?).await?;
    assert_eq!(path, env.set("2,1")?);

    Ok(())
}
