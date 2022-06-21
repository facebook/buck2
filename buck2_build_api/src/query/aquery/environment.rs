/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::cells::paths::CellPath;
use buck2_interpreter::common::BuildFilePath;
use buck2_query::query::environment::NodeLabel;
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::environment::QueryTargetAttr;
use buck2_query::query::syntax::simple::eval::error::QueryError;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::traversal::async_depth_first_postorder_traversal;
use buck2_query::query::traversal::async_depth_limited_traversal;
use buck2_query::query::traversal::AsyncNodeLookup;
use buck2_query::query::traversal::AsyncTraversalDelegate;
use derivative::Derivative;
use derive_more::Display;
use gazebo::prelude::*;
use gazebo::variants::VariantName;
use indexmap::IndexMap;
use internment::ArcIntern;
use ref_cast::RefCast;
use serde::Serialize;

use crate::actions::artifact::ArtifactFs;
use crate::actions::artifact::ExecutorFs;
use crate::actions::ActionKey;
use crate::actions::RegisteredAction;
use crate::artifact_groups::TransitiveSetProjectionKey;
use crate::query::cquery::environment::CqueryDelegate;
use crate::query::uquery::environment::QueryLiterals;

impl NodeLabel for ActionKey {}

#[derive(Debug, Display, RefCast, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ActionAttr(str);

impl ActionAttr {
    fn new(x: &str) -> &Self {
        ActionAttr::ref_cast(x)
    }
}

impl QueryTargetAttr for ActionAttr {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        filter(&self.0)
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct SetProjectionInputsData {
    key: TransitiveSetProjectionKey,
    direct: Vec<ActionKey>,
    children: Vec<SetProjectionInputs>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SetProjectionInputs {
    node: ArcIntern<SetProjectionInputsData>,
}

// ArcIntern doesn't impl Dupe, but is cheap to copy, so we need to explicitly impl it.
impl Dupe for SetProjectionInputs {}

impl SetProjectionInputs {
    pub fn new(
        key: TransitiveSetProjectionKey,
        direct: Vec<ActionKey>,
        children: Vec<SetProjectionInputs>,
    ) -> Self {
        Self {
            node: ArcIntern::new(SetProjectionInputsData {
                key,
                direct,
                children,
            }),
        }
    }
}

#[derive(Debug)]
pub enum ActionInput {
    ActionKey(ActionKey),
    IndirectInputs(SetProjectionInputs),
}

#[derive(Derivative, Clone, Dupe)]
#[derivative(Debug)]
pub struct ActionQueryNode {
    action: Arc<RegisteredAction>,
    deps: Arc<Vec<ActionInput>>,
    #[derivative(Debug = "ignore")]
    fs: Arc<ArtifactFs>,
}

impl ActionQueryNode {
    pub fn new(action: Arc<RegisteredAction>, deps: Vec<ActionInput>, fs: Arc<ArtifactFs>) -> Self {
        Self {
            action,
            deps: Arc::new(deps),
            fs,
        }
    }

    pub fn attrs(&self) -> IndexMap<String, String> {
        self.action.action().aquery_attributes(&ExecutorFs::new(
            &*self.fs,
            self.action.execution_config().path_separator,
        ))
    }
}

impl QueryTarget for ActionQueryNode {
    type NodeRef = ActionKey;
    type Attr = ActionAttr;

    fn node_ref(&self) -> &Self::NodeRef {
        self.action.key()
    }

    fn rule_type(&self) -> Cow<str> {
        Cow::Owned(self.action.kind().variant_name().to_ascii_lowercase())
    }

    /// Return the path to the buildfile that defines this target, e.g. `fbcode//foo/bar/TARGETS`
    fn buildfile_path(&self) -> &BuildFilePath {
        // TODO(cjhopman): In addition to implementing this, we should be able to return an anyhow::Error here rather than panicing.
        unimplemented!("buildfile not yet implemented in aquery")
    }

    // TODO(cjhopman): Use existential traits to remove the Box<> once they are stabilized.
    fn deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        struct Iter<'a> {
            visited: HashSet<&'a SetProjectionInputs>,
            queue: VecDeque<&'a SetProjectionInputs>,
        }

        impl<'a> Iter<'a> {
            fn new<From: Iterator<Item = &'a SetProjectionInputs>>(iter: From) -> Self {
                let mut visited = HashSet::new();
                let mut queue = VecDeque::new();
                for it in iter {
                    if visited.insert(it) {
                        queue.push_back(it);
                    }
                }
                Self { visited, queue }
            }
        }

        impl<'a> Iterator for Iter<'a> {
            type Item = &'a SetProjectionInputs;

            fn next(&mut self) -> Option<Self::Item> {
                self.queue.pop_front().map(|node| {
                    for child in &*node.node.children {
                        if self.visited.insert(child) {
                            self.queue.push_back(child);
                        }
                    }

                    node
                })
            }
        }

        let direct = self.deps.iter().filter_map(|input| match input {
            ActionInput::ActionKey(action_key) => Some(action_key),
            ActionInput::IndirectInputs(..) => None,
        });

        let indirect = Iter::new(self.deps.iter().filter_map(|input| match input {
            ActionInput::ActionKey(..) => None,
            ActionInput::IndirectInputs(val) => Some(val),
        }));

        let indirect = Iter::new(indirect);

        box direct.chain(indirect.flat_map(|v| v.node.direct.iter()))
    }

    fn exec_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        box std::iter::empty()
    }

    fn target_deps<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::NodeRef> + Send + 'a> {
        self.deps()
    }

    fn special_attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut _func: F,
    ) -> Result<(), E> {
        Ok(())
    }

    fn attrs_for_each<E, F: FnMut(&str, &Self::Attr) -> Result<(), E>>(
        &self,
        mut func: F,
    ) -> Result<(), E> {
        func("kind", ActionAttr::new(&self.rule_type()))?;
        func("category", ActionAttr::new(self.action.category().as_str()))?;
        func(
            "identifier",
            ActionAttr::new(self.action.identifier().unwrap_or("")),
        )?;
        // TODO(cjhopman): impl inputs/outputs for actions in aquery
        func("inputs", ActionAttr::new(""))?;
        func("outputs", ActionAttr::new(""))?;

        for (k, v) in self.attrs() {
            func(&k, ActionAttr::new(&v))?;
        }
        Ok(())
    }

    fn map_attr<R, F: FnMut(Option<&Self::Attr>) -> R>(&self, key: &str, mut func: F) -> R {
        let mut res = None;

        self.attrs_for_each(|k, attr| {
            if k == key {
                res = Some(func(Some(attr)));
            }
            Ok::<(), anyhow::Error>(())
        })
        .unwrap();
        match res {
            Some(v) => v,
            None => func(None),
        }
    }

    fn inputs_for_each<E, F: FnMut(CellPath) -> Result<(), E>>(
        &self,
        mut _func: F,
    ) -> Result<(), E> {
        // TODO(cjhopman): In addition to implementing this, we should be able to return an anyhow::Error here rather than panicing.
        unimplemented!("inputs not yet implemented in aquery")
    }

    fn call_stack(&self) -> Option<String> {
        None
    }
}

/// CqueryDelegate resolves information needed by the QueryEnvironment.
#[async_trait]
pub trait AqueryDelegate: Send + Sync {
    fn cquery_delegate(&self) -> &dyn CqueryDelegate;

    async fn get_node(&self, key: &ActionKey) -> anyhow::Result<ActionQueryNode>;
}

pub struct AqueryEnvironment<'c> {
    delegate: Arc<dyn AqueryDelegate + 'c>,
    literals: Arc<dyn QueryLiterals<ActionQueryNode> + 'c>,
}

impl<'c> AqueryEnvironment<'c> {
    pub fn new(
        delegate: Arc<dyn AqueryDelegate + 'c>,
        literals: Arc<dyn QueryLiterals<ActionQueryNode> + 'c>,
    ) -> Self {
        Self { delegate, literals }
    }

    async fn get_node(&self, label: &ActionKey) -> anyhow::Result<ActionQueryNode> {
        self.delegate.get_node(label).await
    }
}

#[async_trait]
impl<'a> AsyncNodeLookup<ActionQueryNode> for AqueryEnvironment<'a> {
    async fn get(&self, label: &ActionKey) -> anyhow::Result<ActionQueryNode> {
        self.get_node(label).await
    }
}

#[async_trait]
impl<'c> QueryEnvironment for AqueryEnvironment<'c> {
    type Target = ActionQueryNode;

    async fn get_node(&self, node_ref: &ActionKey) -> anyhow::Result<Self::Target> {
        AqueryEnvironment::get_node(self, node_ref).await
    }

    async fn eval_literals(&self, literals: &[&str]) -> anyhow::Result<TargetSet<Self::Target>> {
        self.literals.eval_literals(literals).await
    }

    async fn eval_file_literal(&self, literal: &str) -> anyhow::Result<FileSet> {
        self.delegate
            .cquery_delegate()
            .uquery_delegate()
            .eval_file_literal(literal)
            .await
    }

    async fn dfs_postorder(
        &self,
        root: &TargetSet<Self::Target>,
        traversal_delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
    ) -> anyhow::Result<()> {
        // TODO(cjhopman): The query nodes deps are going to flatten the tset structure for its deps. In a typical
        // build graph, a traversal over just the graph of ActionQueryNode ends up being an `O(n)` operation at each
        // node and ends up with an `O(n^2)` cost. If instead we were to not flatten the structure and traverse the
        // mixed graph of action nodes and tset nodes, we'd get closer to `O(n + e)` which in practice is much better
        // (hence the whole point of tsets). While we can't change the ActionQueryNode deps() function to not flatten
        // the tset, we aren't required to do these traversal's using that function.
        async_depth_first_postorder_traversal(self, root.iter_names(), traversal_delegate).await
    }

    async fn depth_limited_traversal(
        &self,
        root: &TargetSet<Self::Target>,
        delegate: &mut dyn AsyncTraversalDelegate<Self::Target>,
        depth: u32,
    ) -> anyhow::Result<()> {
        // TODO(cjhopman): See above.
        async_depth_limited_traversal(self, root.iter_names(), delegate, depth).await
    }

    async fn owner(&self, _paths: &FileSet) -> anyhow::Result<TargetSet<Self::Target>> {
        Err(QueryError::NotAvailableInContext("owner").into())
    }
}
