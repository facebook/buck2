/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_artifact::actions::key::ActionKey;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::actions::calculation::ActionCalculation;
use buck2_build_api::actions::query::iter_action_inputs;
use buck2_build_api::actions::query::ActionInput;
use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::actions::query::ActionQueryNodeRef;
use buck2_build_api::actions::query::SetProjectionInputs;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ResolvedArtifactGroup;
use buck2_build_api::artifact_groups::TransitiveSetProjectionKey;
use buck2_build_api::deferred::calculation::DeferredCalculation;
use buck2_common::result::SharedResult;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::pattern::ParsedPattern;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use dashmap::DashMap;
use dice::DiceComputations;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::FutureExt;
use futures::future::Shared;
use futures::stream::FuturesOrdered;
use futures::StreamExt;
use gazebo::prelude::*;
use itertools::Either;
use itertools::Itertools;
use thiserror::Error;

use crate::aquery::environment::AqueryDelegate;
use crate::cquery::environment::CqueryDelegate;
use crate::dice::DiceQueryDelegate;
use crate::uquery::environment::QueryLiterals;

#[derive(Debug, Error)]
enum ActionQueryError {
    #[error(
        "`aquery` currently only supports literal target patterns, not package or recursive (got `{0}`)"
    )]
    UnsupportedTargetPattern(String),
}

/// A simple concurrent map with a `get_or_compute()` function
struct NodeCache<'c, K: Hash + Eq + PartialEq + Dupe, V: Dupe> {
    map: DashMap<K, Shared<BoxFuture<'c, V>>>,
}

impl<'c, K: Hash + Eq + PartialEq + Dupe, V: Dupe> NodeCache<'c, K, V> {
    fn new() -> Self {
        Self {
            map: DashMap::new(),
        }
    }

    /// Gets the value or computes it with the provided function. The function is called while holding
    /// a lock on the map and so should not do much work. The future returned by that function isn't
    /// polled until later so it's fine for it to do more work.
    fn get_or_compute<F>(self: Arc<Self>, key: K, f: F) -> Shared<BoxFuture<'c, V>>
    where
        F: FnOnce(K) -> BoxFuture<'c, V>,
    {
        if let Some(v) = self.map.get(&key) {
            return v.clone();
        }

        let entry = self.map.entry(key.dupe());
        entry.or_insert_with(move || f(key).shared()).clone()
    }
}

/// Cache for ActionQueryNode and things needed to construct it. This is required because
/// QueryTarget::deps() requires that deps are synchronously available and so we need to
/// be able to iterate the tset structure synchronously.
#[derive(Clone, Dupe)]
struct DiceAqueryNodesCache<'c> {
    action_nodes: Arc<NodeCache<'c, ActionKey, SharedResult<ActionQueryNode>>>,
    tset_nodes: Arc<NodeCache<'c, TransitiveSetProjectionKey, SharedResult<SetProjectionInputs>>>,
}

impl<'c> DiceAqueryNodesCache<'c> {
    fn new() -> Self {
        Self {
            action_nodes: Arc::new(NodeCache::new()),
            tset_nodes: Arc::new(NodeCache::new()),
        }
    }
}

pub(crate) struct DiceAqueryDelegate<'c> {
    base_delegate: DiceQueryDelegate<'c>,
    nodes_cache: DiceAqueryNodesCache<'c>,
    artifact_fs: Arc<ArtifactFs>,
}

/// Converts artifact inputs into aquery's ActionInput. This is mostly a matter of resolving the indirect
/// `TransitiveSetProjectionKey` to our direct shadow tset graph node `SetProjectionInputs`.
// TODO(cjhopman): I think we should change ArtifactGroup to hold a `(TransitiveSet, ProjectionIndex)` rather
// than `(TransitiveSetKey, ProjectionIndex)`. We already have that information when constructing it and the
// artifact side of it holds a starlark ref. That would allow someone with an ArtifactGroup to synchronously
// traverse the tset graph rather than needing to asynchronously resolve a TransitiveSetKey.
async fn convert_inputs<'c, 'a, Iter: IntoIterator<Item = &'a ArtifactGroup>>(
    ctx: &'c DiceComputations,
    node_cache: DiceAqueryNodesCache<'c>,
    inputs: Iter,
) -> anyhow::Result<Vec<ActionInput>> {
    let (artifacts, projections): (Vec<_>, Vec<_>) = Itertools::partition_map(
        inputs
            .into_iter()
            .filter_map(|input| match input.assert_resolved() {
                ResolvedArtifactGroup::Artifact(a) => a.action_key().map(Either::Left),
                ResolvedArtifactGroup::TransitiveSetProjection(key) => Some(Either::Right(key)),
            }),
        |v| v,
    );
    let mut deps =
        artifacts.into_map(|a| ActionInput::ActionKey(ActionQueryNodeRef::Action(a.dupe())));
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

fn compute_tset_node<'c>(
    node_cache: DiceAqueryNodesCache<'c>,
    ctx: &'c DiceComputations,
    key: TransitiveSetProjectionKey,
) -> BoxFuture<'c, SharedResult<SetProjectionInputs>> {
    async move {
        let set = ctx
            .compute_deferred_data(&key.key)
            .await
            .context("Failed to compute deferred")?;

        let sub_inputs = set
            .as_transitive_set()
            .get_projection_sub_inputs(key.projection)?;

        let inputs = convert_inputs(ctx, node_cache, sub_inputs.iter()).await?;

        let (direct, children) = inputs.into_iter().partition_map(|v| match v {
            ActionInput::ActionKey(action_key) => Either::Left(action_key),
            ActionInput::IndirectInputs(projection) => Either::Right(projection),
        });

        Ok(SetProjectionInputs::new(key.dupe(), direct, children))
    }
    .boxed()
}

async fn get_tset_node<'c>(
    node_cache: DiceAqueryNodesCache<'c>,
    ctx: &'c DiceComputations,
    key: TransitiveSetProjectionKey,
) -> anyhow::Result<SetProjectionInputs> {
    let copied_node_cache = node_cache.dupe();
    Ok(node_cache
        .tset_nodes
        .get_or_compute(key, move |key| {
            compute_tset_node(copied_node_cache, ctx, key)
        })
        .await?)
}

fn compute_action_node<'c>(
    node_cache: DiceAqueryNodesCache<'c>,
    ctx: &'c DiceComputations,
    key: ActionKey,
    fs: Arc<ArtifactFs>,
) -> BoxFuture<'c, SharedResult<ActionQueryNode>> {
    async move {
        let action = ActionCalculation::get_action(ctx, &key).await?;
        let deps = convert_inputs(ctx, node_cache, action.inputs()?.iter()).await?;
        Ok(ActionQueryNode::new_action(action, deps, fs))
    }
    .boxed()
}

async fn get_action_node<'c>(
    node_cache: DiceAqueryNodesCache<'c>,
    ctx: &'c DiceComputations,
    key: ActionKey,
    fs: Arc<ArtifactFs>,
) -> anyhow::Result<ActionQueryNode> {
    let copied_node_cache = node_cache.dupe();
    Ok(node_cache
        .action_nodes
        .get_or_compute(key, move |key| {
            compute_action_node(copied_node_cache, ctx, key, fs)
        })
        .await?)
}

impl<'c> DiceAqueryDelegate<'c> {
    pub(crate) async fn new(
        base_delegate: DiceQueryDelegate<'c>,
    ) -> anyhow::Result<DiceAqueryDelegate<'c>> {
        let artifact_fs = Arc::new(base_delegate.ctx().get_artifact_fs().await?);
        Ok(DiceAqueryDelegate {
            base_delegate,
            nodes_cache: DiceAqueryNodesCache::new(),
            artifact_fs,
        })
    }

    pub async fn get_action_node(&self, key: &ActionKey) -> anyhow::Result<ActionQueryNode> {
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

    async fn expand_artifacts(
        &self,
        artifacts: &[ArtifactGroup],
    ) -> anyhow::Result<Vec<ActionQueryNode>> {
        let inputs =
            convert_inputs(self.base_delegate.ctx(), self.nodes_cache.dupe(), artifacts).await?;

        let refs = iter_action_inputs(&inputs)
            .map(|i| i.require_action())
            .collect::<Result<Vec<_>, _>>()?;

        futures::future::try_join_all(refs.iter().map(|n| self.get_node(n))).await
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
                ParsedPattern::Target(package, target_name, providers) => {
                    let label = providers.into_providers_label(package, target_name.as_ref());
                    let configured_label = self
                        .base_delegate
                        .ctx()
                        .get_configured_provider_label(
                            &label,
                            self.base_delegate.global_target_platform(),
                        )
                        .await?;

                    match self
                        .base_delegate
                        .ctx()
                        .get_analysis_result(configured_label.target())
                        .await?
                    {
                        MaybeCompatible::Incompatible(_) => {
                            // ignored
                        }
                        MaybeCompatible::Compatible(analysis) => {
                            let providers = analysis.lookup_inner(&configured_label)?;

                            for output in providers
                                .provider_collection()
                                .default_info()
                                .default_outputs()
                            {
                                if let Some(action_key) = output.artifact().action_key() {
                                    result.insert(self.get_action_node(action_key).await?);
                                }
                            }

                            result
                                .insert(ActionQueryNode::new_analysis(configured_label, analysis));
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
