/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{hash::Hash, sync::Arc};

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::{provider::ProvidersLabel, result::SharedResult, target::TargetLabel};
use buck2_interpreter::pattern::ParsedPattern;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use dashmap::DashMap;
use dice::{DiceComputations, DiceTransaction};
use futures::{
    future::{BoxFuture, Shared},
    stream::FuturesOrdered,
    Future, FutureExt, StreamExt,
};
use gazebo::prelude::*;
use itertools::{Either, Itertools};
use thiserror::Error;

use crate::{
    actions::{
        artifact::{ArtifactFs, ArtifactKind},
        calculation::ActionCalculation,
        ActionKey,
    },
    artifact_groups::{ArtifactGroup, TransitiveSetProjectionKey},
    calculation::Calculation,
    nodes::compatibility::MaybeCompatible,
    query::{
        aquery::environment::{ActionInput, ActionQueryNode, AqueryDelegate, SetProjectionInputs},
        cquery::environment::CqueryDelegate,
        dice::DiceQueryDelegate,
        uquery::environment::QueryLiterals,
    },
};

#[derive(Debug, Error)]
enum ActionQueryError {
    #[error(
        "`aquery` currently only supports literal target patterns, not package or recursive (got `{0}`)"
    )]
    UnsupportedTargetPattern(String),
}

/// A simple concurrent map with a `get_or_compute()` function
struct NodeCache<K: Hash + Eq + PartialEq + Dupe, V: Dupe> {
    map: DashMap<K, Shared<BoxFuture<'static, V>>>,
}

impl<K: Hash + Eq + PartialEq + Dupe, V: Dupe> NodeCache<K, V> {
    fn new() -> Self {
        Self {
            map: DashMap::new(),
        }
    }

    /// Gets the value or computes it with the provided function. The function is called while holding
    /// a lock on the map and so should not do much work. The future returned by that function isn't
    /// polled until later so it's fine for it to do more work.
    fn get_or_compute<Fut: Future<Output = V> + Send + 'static, F: FnOnce(K) -> Fut>(
        self: Arc<Self>,
        key: K,
        f: F,
    ) -> Shared<BoxFuture<'static, V>> {
        if let Some(v) = self.map.get(&key) {
            return v.clone();
        }

        let entry = self.map.entry(key.dupe());
        entry
            .or_insert_with(move || f(key).boxed().shared())
            .clone()
    }
}

/// Cache for ActionQueryNode and things needed to construct it. This is required because
/// QueryTarget::deps() requires that deps are synchronously available and so we need to
/// be able to iterate the tset structure synchronously.
#[derive(Clone, Dupe)]
struct DiceAqueryNodesCache {
    action_nodes: Arc<NodeCache<ActionKey, SharedResult<ActionQueryNode>>>,
    tset_nodes: Arc<NodeCache<TransitiveSetProjectionKey, SharedResult<SetProjectionInputs>>>,
}

impl DiceAqueryNodesCache {
    fn new() -> Self {
        Self {
            action_nodes: Arc::new(NodeCache::new()),
            tset_nodes: Arc::new(NodeCache::new()),
        }
    }
}

pub(crate) struct DiceAqueryDelegate<'c> {
    base_delegate: DiceQueryDelegate<'c>,
    nodes_cache: DiceAqueryNodesCache,
    artifact_fs: Arc<ArtifactFs>,
}

/// Converts artifact inputs into aquery's ActionInput. This is mostly a matter of resolving the indirect
/// `TransitiveSetProjectionKey` to our direct shadow tset graph node `SetProjectionInputs`.
// TODO(cjhopman): I think we should change ArtifactGroup to hold a `(TransitiveSet, ProjectionIndex)` rather
// than `(TransitiveSetKey, ProjectionIndex)`. We already have that information when constructing it and the
// artifact side of it holds a starlark ref. That would allow someone with an ArtifactGroup to synchronously
// traverse the tset graph rather than needing to asynchronously resolve a TransitiveSetKey.
async fn convert_inputs<'a, Iter: Iterator<Item = &'a ArtifactGroup>>(
    ctx: &DiceComputations,
    node_cache: DiceAqueryNodesCache,
    inputs: Iter,
) -> anyhow::Result<Vec<ActionInput>> {
    let (artifacts, projections): (Vec<_>, Vec<_>) = Itertools::partition_map(
        inputs.filter_map(|input| match input {
            ArtifactGroup::Artifact(a) => match a.0.as_ref() {
                ArtifactKind::Source(_) => None,
                ArtifactKind::Build(a) => Some(Either::Left(a)),
            },
            ArtifactGroup::TransitiveSetProjection(key) => Some(Either::Right(key)),
        }),
        |v| v,
    );
    let mut deps = artifacts.into_map(|a| ActionInput::BuildArtifact(a.dupe()));
    let mut projection_deps: FuturesOrdered<_> = projections
        .into_iter()
        .map(|key| {
            let key = key.dupe();
            let node_cache = node_cache.dupe();
            get_tset_node(node_cache, ctx, key)
        })
        .collect();

    while let Some(node) = tokio::task::unconstrained(projection_deps.next()).await {
        deps.push(ActionInput::IndirectInputs(node?));
    }
    Ok(deps)
}

fn compute_tset_node(
    node_cache: DiceAqueryNodesCache,
    ctx: DiceTransaction,
    key: TransitiveSetProjectionKey,
) -> BoxFuture<'static, SharedResult<SetProjectionInputs>> {
    async move {
        let set = ctx
            .compute_deferred_data(&key.key)
            .await
            .context("Failed to compute deferred")?;

        let sub_inputs = set
            .as_transitive_set()?
            .get_projection_sub_inputs(key.projection)?;

        let inputs = convert_inputs(&*ctx, node_cache, sub_inputs.iter()).await?;

        let (direct, children) = inputs.into_iter().partition_map(|v| match v {
            ActionInput::BuildArtifact(artifact) => Either::Left(artifact),
            ActionInput::IndirectInputs(projection) => Either::Right(projection),
        });

        Ok(SetProjectionInputs::new(key.dupe(), direct, children))
    }
    .boxed()
}

async fn get_tset_node(
    node_cache: DiceAqueryNodesCache,
    ctx: &DiceComputations,
    key: TransitiveSetProjectionKey,
) -> anyhow::Result<SetProjectionInputs> {
    let copied_node_cache = node_cache.dupe();
    Ok(node_cache
        .tset_nodes
        .get_or_compute(key, move |key| {
            ctx.temporary_spawn(move |ctx| compute_tset_node(copied_node_cache, ctx, key))
        })
        .await?)
}

fn compute_action_node(
    node_cache: DiceAqueryNodesCache,
    ctx: DiceTransaction,
    key: ActionKey,
    fs: Arc<ArtifactFs>,
) -> BoxFuture<'static, SharedResult<ActionQueryNode>> {
    async move {
        let action = ctx.get_action(&key).await?;
        let deps = convert_inputs(&*ctx, node_cache, action.inputs()?.iter()).await?;
        Ok(ActionQueryNode::new(action, deps, fs))
    }
    .boxed()
}

async fn get_action_node(
    node_cache: DiceAqueryNodesCache,
    ctx: &DiceComputations,
    key: ActionKey,
    fs: Arc<ArtifactFs>,
) -> anyhow::Result<ActionQueryNode> {
    let copied_node_cache = node_cache.dupe();
    Ok(node_cache
        .action_nodes
        .get_or_compute(key, move |key| {
            ctx.temporary_spawn(move |ctx| compute_action_node(copied_node_cache, ctx, key, fs))
        })
        .await?)
}

impl<'c> DiceAqueryDelegate<'c> {
    pub(crate) async fn new<'a>(base_delegate: DiceQueryDelegate<'a>) -> DiceAqueryDelegate<'a> {
        let artifact_fs = Arc::new(base_delegate.ctx().get_artifact_fs().await);
        DiceAqueryDelegate {
            base_delegate,
            nodes_cache: DiceAqueryNodesCache::new(),
            artifact_fs,
        }
    }

    async fn get_action_node(&self, key: &ActionKey) -> anyhow::Result<ActionQueryNode> {
        get_action_node(
            self.nodes_cache.dupe(),
            self.base_delegate.ctx(),
            key.dupe(),
            self.artifact_fs.dupe(),
        )
        .await
    }
}

#[async_trait]
impl<'c> AqueryDelegate for DiceAqueryDelegate<'c> {
    fn cquery_delegate(&self) -> &dyn CqueryDelegate {
        &self.base_delegate
    }

    async fn get_node(&self, key: &ActionKey) -> anyhow::Result<ActionQueryNode> {
        self.get_action_node(key).await
    }
}

#[async_trait]
impl<'c> QueryLiterals<ActionQueryNode> for DiceAqueryDelegate<'c> {
    async fn eval_literals(&self, literals: &[&str]) -> anyhow::Result<TargetSet<ActionQueryNode>> {
        // For literal evaluation, we resolve the providers pattern to the analysis result, pull out
        // the default outputs and look up the corresponding actions.
        // TODO(cjhopman): This is a common pattern and we should probably pull it out to a common
        // place, maybe with streaming results like we have for `target pattern -> target node`
        let mut result = TargetSet::new();
        for literal in literals {
            let label = self
                .base_delegate
                .literal_parser()
                .parse_providers_pattern(literal)?;
            match label {
                ParsedPattern::Target(package, (target_name, providers_name)) => {
                    let target_label = TargetLabel::new(package, target_name);
                    let label = ProvidersLabel::new(target_label.dupe(), providers_name);
                    let configured_label = self
                        .base_delegate
                        .ctx()
                        .get_configured_target(&label, self.base_delegate.global_target_platform())
                        .await?;

                    match self
                        .base_delegate
                        .ctx()
                        .get_providers(&configured_label)
                        .await?
                    {
                        MaybeCompatible::Incompatible(_) => {
                            // ignored
                        }
                        MaybeCompatible::Compatible(providers) => {
                            for output in providers
                                .provider_collection()
                                .default_info()
                                .default_outputs()
                            {
                                match output.artifact().0.as_ref() {
                                    ArtifactKind::Build(artifact) => {
                                        result.insert(self.get_action_node(artifact.key()).await?);
                                    }
                                    ArtifactKind::Source(_) => {
                                        // ignored
                                    }
                                }
                            }
                        }
                    }
                }
                ParsedPattern::Package(_) | ParsedPattern::Recursive(_) => {
                    return Err(
                        ActionQueryError::UnsupportedTargetPattern((*literal).to_owned()).into(),
                    );
                }
            }
        }
        Ok(result)
    }
}
