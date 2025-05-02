/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_error::internal_error;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dupe::Dupe;
use starlark::values::FrozenValueTyped;
use starlark::values::OwnedFrozenValueTyped;

use crate::actions::RegisteredAction;
use crate::artifact_groups::ArtifactGroup;
use crate::deferred::calculation::ActionLookup;
use crate::deferred::calculation::DeferredHolder;
use crate::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash)]
enum Key {
    Action(ActionKey),
    ResolvedTransitiveSetProjection(ResolvedTransitiveSetProjection),
    MissingAnalysis(DeferredHolderKey),
}

impl Eq for ResolvedTransitiveSetProjection {}

impl PartialEq for ResolvedTransitiveSetProjection {
    fn eq(&self, other: &Self) -> bool {
        self.projection == other.projection && self.tset.key() == other.tset.key()
    }
}

impl std::hash::Hash for ResolvedTransitiveSetProjection {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.tset.get_projection_key(self.projection).hash(state);
    }
}

/// Once we look up a transitive set by key once, we have a reference ot the tset itself. That tset contains direct
/// references to its children, so we can use that to look up the children rather than needing to look up through
/// the key again.
#[derive(Clone, Dupe, Debug)]
struct ResolvedTransitiveSetProjection {
    projection: usize,
    tset: OwnedFrozenValueTyped<FrozenTransitiveSet>,
}

#[derive(Clone, Dupe, Debug)]
enum Node {
    Action(Arc<RegisteredAction>),
    /// Used when we try to look up something from an analysis that is missing. This should only
    /// happen for dynamic nodes (which would be missing if the inputs failed or the analysis of
    /// node itself failed). We wouldn't encounter this for other nodes because we wouldn't have
    /// any actions/etc that could refer to them if they are missing.
    Missing(DeferredHolderKey),
    TransitiveSetProjection(ResolvedTransitiveSetProjection),
}

impl Node {
    fn is_missing(&self) -> bool {
        matches!(self, Node::Missing(..))
    }
}

struct Graph<'a>(&'a fxhash::FxHashMap<DeferredHolderKey, DeferredHolder>);

impl<'a> Graph<'a> {
    pub(crate) fn lookup_deferred(
        &self,
        holder_key: &DeferredHolderKey,
    ) -> Option<&DeferredHolder> {
        self.0.get(holder_key)
    }

    fn lookup_node(&self, key: Key) -> buck2_error::Result<Node> {
        match key {
            Key::Action(action_key) => match self.lookup_deferred(action_key.holder_key()) {
                Some(v) => match v.lookup_action(&action_key)? {
                    ActionLookup::Action(registered_action) => Ok(Node::Action(registered_action)),
                    ActionLookup::Deferred(action_key) => self.lookup_node(Key::Action(action_key)),
                },
                None => Ok(Node::Missing(action_key.holder_key().dupe())),
            },
            Key::ResolvedTransitiveSetProjection(proj) => {
                Ok(Node::TransitiveSetProjection(proj.dupe()))
            }
            Key::MissingAnalysis(key) => Ok(Node::Missing(key)),
        }
    }

    fn lookup_artifact(&self, artifact: &ArtifactGroup) -> buck2_error::Result<Option<Key>> {
        match artifact {
            ArtifactGroup::Artifact(artifact) => {
                Ok(artifact.action_key().map(|key| Key::Action(key.dupe())))
            }
            ArtifactGroup::TransitiveSetProjection(key) => {
                let holder_key = key.key.holder_key();
                match self.lookup_deferred(holder_key) {
                    Some(v) => {
                        let tset = v.lookup_transitive_set(&key.key)?;
                        Ok(Some(Key::ResolvedTransitiveSetProjection(
                            ResolvedTransitiveSetProjection {
                                projection: key.projection,
                                tset,
                            },
                        )))
                    }
                    None => Ok(Some(Key::MissingAnalysis(holder_key.dupe()))),
                }
            }
            ArtifactGroup::Promise(promise_artifact) => Ok(promise_artifact
                .unwrap()
                .action_key()
                .map(|key| Key::Action(key.dupe()))),
        }
    }

    fn visit_deps(&self, node: &Node, mut visit_dep: impl FnMut(Key)) -> buck2_error::Result<()> {
        match node {
            Node::Action(action) => {
                for artifact in action.inputs()?.iter() {
                    if let Some(v) = self.lookup_artifact(artifact)? {
                        visit_dep(v);
                    }
                }
                if action.key().holder_key().is_dynamic() {
                    // TODO(cjhopman): Should traverse to the dynamic node's inputs and parent dynamic node.
                }
            }
            Node::TransitiveSetProjection(proj_node) => {
                for idx in 0..proj_node.tset.children.len() {
                    visit_dep(Key::ResolvedTransitiveSetProjection(
                        ResolvedTransitiveSetProjection {
                            projection: proj_node.projection,
                            tset: proj_node.tset.try_map(|v| {
                                FrozenValueTyped::new(v.children[idx])
                                    .ok_or_else(|| internal_error!("tset children should be tsets"))
                            })?,
                        },
                    ))
                }
                struct Visitor<'a, 'b, F: FnMut(Key)>(F, &'a Graph<'b>, buck2_error::Result<()>);
                impl<F: FnMut(Key)> CommandLineArtifactVisitor for Visitor<'_, '_, F> {
                    fn visit_input(&mut self, input: ArtifactGroup, _tag: Option<&ArtifactTag>) {
                        if self.2.is_err() {
                            return;
                        }

                        match self.1.lookup_artifact(&input) {
                            Ok(Some(v)) => {
                                self.0(v);
                            }
                            Ok(None) => {}
                            Err(e) => {
                                self.2 = Err(e);
                            }
                        }
                    }

                    fn visit_output(
                        &mut self,
                        _artifact: OutputArtifact,
                        _tag: Option<&ArtifactTag>,
                    ) {
                        // nothing to do
                    }
                }
                let mut visitor = Visitor(&mut visit_dep, self, Ok(()));
                proj_node
                    .tset
                    .visit_projection_direct_inputs(proj_node.projection, &mut visitor)?;
                visitor.2?;
            }
            Node::Missing(_holder_key) => {
                // TODO(cjhopman): Should traverse to the dynamic node's inputs and parent dynamic node.
            }
        }
        Ok(())
    }
}

struct TraversalState {
    visited: fxhash::FxHashSet<Key>,
    queue: Vec<Key>,
}

impl TraversalState {
    fn new() -> Self {
        Self {
            visited: fxhash::FxHashSet::default(),
            queue: Vec::new(),
        }
    }

    fn enqueue(&mut self, item: Key) {
        if !self.visited.contains(&item) {
            self.visited.insert(item.dupe());
            self.queue.push(item);
        }
    }

    fn pop(&mut self) -> Option<Key> {
        self.queue.pop()
    }
}

pub fn traverse_partial_action_graph<'a>(
    root_artifacts: impl IntoIterator<Item = &'a ArtifactGroup>,
    state: &fxhash::FxHashMap<DeferredHolderKey, DeferredHolder>,
) -> buck2_error::Result<(bool, fxhash::FxHashSet<ActionKey>)> {
    let mut traversal_state = TraversalState::new();
    let mut actions = fxhash::FxHashSet::default();

    let graph = Graph(state);

    let mut complete = true;

    for artifact in root_artifacts.into_iter() {
        if let Some(v) = graph.lookup_artifact(artifact)? {
            traversal_state.enqueue(v)
        }
    }

    while let Some(item) = traversal_state.pop() {
        let node = graph.lookup_node(item)?;
        // It's required that we add this on visiting an action node rather than an action key. dynamic outputs produces action keys that point
        // to other action keys, and we don't want to add those.
        if let Node::Action(action) = &node {
            actions.insert(action.key().dupe());
        }
        graph.visit_deps(&node, |dep| traversal_state.enqueue(dep))?;
        complete = complete && !node.is_missing();
    }

    Ok((complete, actions))
}

pub fn traverse_target_graph(
    root: &ConfiguredTargetNode,
    mut visitor: impl FnMut(&ConfiguredTargetLabel),
) {
    let mut visited = fxhash::FxHashSet::default();
    let mut queue = Vec::new();
    visited.insert(root.label());
    queue.push(root);
    while let Some(v) = queue.pop() {
        visitor(v.label());
        for dep in v.deps() {
            if visited.insert(dep.label()) {
                queue.push(dep);
            }
        }
    }
}
