/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;
use std::hash::Hash;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_artifact::actions::key::ActionKey;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::actions::calculation::ActionCalculation;
use buck2_build_api::actions::query::ActionInput;
use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::actions::query::ActionQueryNodeRef;
use buck2_build_api::actions::query::SetProjectionInputs;
use buck2_build_api::actions::query::iter_action_inputs;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ResolvedArtifactGroup;
use buck2_build_api::artifact_groups::TransitiveSetProjectionKey;
use buck2_build_api::keep_going::KeepGoing;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use dashmap::DashMap;
use dashmap::mapref::entry::Entry;
use dice::DiceComputations;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::FutureExt;
use futures::future::Shared;
use gazebo::prelude::*;
use itertools::Either;
use itertools::Itertools;
use tokio::sync::oneshot;

use crate::aquery::environment::AqueryDelegate;
use crate::cquery::environment::CqueryDelegate;
use crate::dice::DiceQueryData;
use crate::dice::DiceQueryDelegate;
use crate::uquery::environment::QueryLiterals;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum ActionQueryError {
    #[error(
        "`aquery` currently only supports literal target patterns, not package or recursive (got `{0}`)"
    )]
    UnsupportedTargetPattern(String),
}

/// A simple concurrent map with a `get_or_compute()` function
struct NodeCache<K: Hash + Eq + PartialEq + Dupe, V: Dupe> {
    map: DashMap<K, Shared<oneshot::Receiver<V>>>,
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
    async fn get_or_compute<Fut: Future<Output = V> + Send, F: FnOnce(K) -> Fut>(
        self: Arc<Self>,
        key: K,
        f: F,
    ) -> V {
        if let Some(v) = self.map.get(&key).map(|entry| entry.value().clone())
            && let Ok(v) = v.await
        {
            return v;
        }

        loop {
            // Loop until we get a successful result from polling the channel
            // This is because only the original task that inserted the channel is responsible for
            // completing the channel, and since tasks may be cancelled (which would result in a
            // drop), the channel can return 'Err', indicating that no one will now complete the
            // task. In this case, all threads race to insert a task themselves, and the rest
            // will now await the new channel.
            let entry = self.map.entry(key.dupe());
            match entry {
                Entry::Occupied(occ) => {
                    let fut = occ.get().clone();
                    drop(occ);
                    if let Ok(v) = fut.await {
                        return v;
                    }
                }
                Entry::Vacant(vacant) => {
                    let (tx, rx) = oneshot::channel();
                    let key = vacant.key().dupe();
                    vacant.insert(rx.shared());

                    let v = f(key).await;
                    let _ignore = tx.send(v.dupe());

                    return v;
                }
            }
        }
    }
}

/// Cache for ActionQueryNode and things needed to construct it. This is required because
/// QueryTarget::deps() requires that deps are synchronously available and so we need to
/// be able to iterate the tset structure synchronously.
#[derive(Clone, Dupe)]
struct DiceAqueryNodesCache {
    action_nodes: Arc<NodeCache<ActionKey, buck2_error::Result<ActionQueryNode>>>,
    tset_nodes:
        Arc<NodeCache<TransitiveSetProjectionKey, buck2_error::Result<SetProjectionInputs>>>,
}

impl DiceAqueryNodesCache {
    fn new() -> Self {
        Self {
            action_nodes: Arc::new(NodeCache::new()),
            tset_nodes: Arc::new(NodeCache::new()),
        }
    }
}

pub(crate) struct DiceAqueryDelegate<'c, 'd> {
    base_delegate: DiceQueryDelegate<'c, 'd>,
    query_data: Arc<AqueryData>,
}

pub(crate) struct AqueryData {
    artifact_fs: Arc<ArtifactFs>,
    delegate_query_data: Arc<DiceQueryData>,
    nodes_cache: DiceAqueryNodesCache,
}

/// Converts artifact inputs into aquery's ActionInput. This is mostly a matter of resolving the indirect
/// `TransitiveSetProjectionKey` to our direct shadow tset graph node `SetProjectionInputs`.
// TODO(cjhopman): I think we should change ArtifactGroup to hold a `(TransitiveSet, ProjectionIndex)` rather
// than `(TransitiveSetKey, ProjectionIndex)`. We already have that information when constructing it and the
// artifact side of it holds a starlark ref. That would allow someone with an ArtifactGroup to synchronously
// traverse the tset graph rather than needing to asynchronously resolve a TransitiveSetKey.
async fn convert_inputs<'c, 'a, Iter: IntoIterator<Item = &'a ArtifactGroup>>(
    ctx: &'c mut DiceComputations<'_>,
    node_cache: DiceAqueryNodesCache,
    inputs: Iter,
) -> buck2_error::Result<Vec<ActionInput>> {
    let resolved_artifacts: Vec<_> = tokio::task::unconstrained(KeepGoing::try_compute_join_all(
        ctx,
        inputs,
        |ctx, input| async move { input.resolved_artifact(ctx).await }.boxed(),
    ))
    .await?;

    let (artifacts, projections): (Vec<_>, Vec<_>) = Itertools::partition_map(
        resolved_artifacts
            .into_iter()
            .filter_map(|resolved_artifact| match resolved_artifact {
                ResolvedArtifactGroup::Artifact(a) => {
                    a.action_key().map(|a| Either::Left(a.clone()))
                }
                ResolvedArtifactGroup::TransitiveSetProjection(key) => Some(Either::Right(key)),
            }),
        |v| v,
    );
    let mut deps =
        artifacts.into_map(|a| ActionInput::ActionKey(ActionQueryNodeRef::Action(a.dupe())));
    let projection_deps = ctx
        .try_compute_join(projections, |ctx, key| {
            let key = key.dupe();
            let node_cache = node_cache.dupe();
            async move { get_tset_node(node_cache, ctx, key).await }.boxed()
        })
        .await?;

    for node in projection_deps {
        deps.push(ActionInput::IndirectInputs(node));
    }
    Ok(deps)
}

fn compute_tset_node<'c>(
    node_cache: DiceAqueryNodesCache,
    ctx: &'c mut DiceComputations<'_>,
    key: TransitiveSetProjectionKey,
) -> BoxFuture<'c, buck2_error::Result<SetProjectionInputs>> {
    async move {
        let set = key.key.lookup(ctx).await?;

        let sub_inputs = set.get_projection_sub_inputs(key.projection)?;

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
    node_cache: DiceAqueryNodesCache,
    ctx: &'c mut DiceComputations<'_>,
    key: TransitiveSetProjectionKey,
) -> buck2_error::Result<SetProjectionInputs> {
    let copied_node_cache = node_cache.dupe();
    node_cache
        .tset_nodes
        .get_or_compute(key, move |key| {
            compute_tset_node(copied_node_cache, ctx, key)
        })
        .await
}

fn compute_action_node<'c>(
    node_cache: DiceAqueryNodesCache,
    ctx: &'c mut DiceComputations<'_>,
    key: ActionKey,
    fs: Arc<ArtifactFs>,
) -> BoxFuture<'c, buck2_error::Result<ActionQueryNode>> {
    async move {
        let action = ActionCalculation::get_action(ctx, &key).await?;
        let deps = convert_inputs(ctx, node_cache, action.inputs()?.iter()).await?;
        Ok(ActionQueryNode::new_action(action, deps, fs))
    }
    .boxed()
}

async fn get_action_node<'c>(
    node_cache: DiceAqueryNodesCache,
    ctx: &'c mut DiceComputations<'_>,
    key: ActionKey,
    fs: Arc<ArtifactFs>,
) -> buck2_error::Result<ActionQueryNode> {
    let copied_node_cache = node_cache.dupe();
    node_cache
        .action_nodes
        .get_or_compute(key, move |key| {
            compute_action_node(copied_node_cache, ctx, key, fs)
        })
        .await
}

impl<'c, 'd> DiceAqueryDelegate<'c, 'd> {
    pub(crate) async fn new(
        base_delegate: DiceQueryDelegate<'c, 'd>,
    ) -> buck2_error::Result<DiceAqueryDelegate<'c, 'd>> {
        let artifact_fs = Arc::new(base_delegate.ctx().get_artifact_fs().await?);
        let query_data = Arc::new(AqueryData {
            artifact_fs,
            delegate_query_data: base_delegate.query_data().dupe(),
            nodes_cache: DiceAqueryNodesCache::new(),
        });
        Ok(DiceAqueryDelegate {
            base_delegate,
            query_data,
        })
    }

    pub(crate) fn query_data(&self) -> &Arc<AqueryData> {
        &self.query_data
    }

    pub(crate) async fn get_action_node(
        &self,
        key: &ActionKey,
    ) -> buck2_error::Result<ActionQueryNode> {
        get_action_node(
            self.query_data.nodes_cache.dupe(),
            &mut self.base_delegate.ctx(),
            key.dupe(),
            self.query_data.artifact_fs.dupe(),
        )
        .await
    }
}

#[async_trait]
impl AqueryDelegate for DiceAqueryDelegate<'_, '_> {
    fn cquery_delegate(&self) -> &dyn CqueryDelegate {
        &self.base_delegate
    }

    fn ctx(&self) -> DiceComputations<'_> {
        self.base_delegate.ctx()
    }

    async fn get_node(&self, key: &ActionKey) -> buck2_error::Result<ActionQueryNode> {
        self.get_action_node(key).await
    }

    async fn expand_artifacts(
        &self,
        artifacts: &[ArtifactGroup],
    ) -> buck2_error::Result<Vec<ActionQueryNode>> {
        let inputs = convert_inputs(
            &mut self.base_delegate.ctx(),
            self.query_data.nodes_cache.dupe(),
            artifacts,
        )
        .await?;

        let refs = iter_action_inputs(&inputs)
            .map(|i| i.require_action())
            .collect::<Result<Vec<_>, _>>()?;

        buck2_util::future::try_join_all(refs.iter().map(|n| self.get_node(n))).await
    }

    async fn get_target_set_from_analysis(
        &self,
        configured_label: &ConfiguredProvidersLabel,
        analysis: AnalysisResult,
    ) -> buck2_error::Result<TargetSet<ActionQueryNode>> {
        get_target_set_from_analysis_inner(
            self.query_data().as_ref(),
            configured_label,
            analysis,
            &mut self.ctx(),
        )
        .await
    }
}

async fn get_target_set_from_analysis_inner(
    query_data: &AqueryData,
    configured_label: &ConfiguredProvidersLabel,
    analysis: AnalysisResult,
    dice: &mut DiceComputations<'_>,
) -> buck2_error::Result<TargetSet<ActionQueryNode>> {
    let mut result = TargetSet::new();

    let providers = analysis.lookup_inner(configured_label)?;

    for output in providers
        .provider_collection()
        .default_info()?
        .default_outputs()
    {
        if let Some(action_key) = output.artifact().action_key() {
            result.insert(
                get_action_node(
                    query_data.nodes_cache.dupe(),
                    dice,
                    action_key.dupe(),
                    query_data.artifact_fs.dupe(),
                )
                .await?,
            );
        }
    }

    result.insert(ActionQueryNode::new_analysis(
        configured_label.dupe(),
        analysis,
    ));

    Ok(result)
}

#[async_trait]
impl QueryLiterals<ActionQueryNode> for AqueryData {
    async fn eval_literals(
        &self,
        literals: &[&str],
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<TargetSet<ActionQueryNode>> {
        // For literal evaluation, we resolve the providers pattern to the analysis result, pull out
        // the default outputs and look up the corresponding actions.
        // TODO(cjhopman): This is a common pattern and we should probably pull it out to a common
        // place, maybe with streaming results like we have for `target pattern -> target node`
        let mut result = TargetSet::new();
        for literal in literals {
            let label = self
                .delegate_query_data
                .literal_parser()
                .parse_providers_pattern(literal)?;
            match label {
                ParsedPattern::Target(package, target_name, providers) => {
                    let label = providers.into_providers_label(package, target_name.as_ref());
                    let configured_label = dice
                        .get_configured_provider_label(
                            &label,
                            self.delegate_query_data.global_cfg_options(),
                        )
                        .await?;

                    match dice.get_analysis_result(configured_label.target()).await? {
                        MaybeCompatible::Incompatible(_) => {
                            // ignored
                        }
                        MaybeCompatible::Compatible(analysis) => {
                            let target_set = get_target_set_from_analysis_inner(
                                self,
                                &configured_label,
                                analysis,
                                dice,
                            )
                            .await?;
                            result.extend(&target_set);
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::task::Poll;

    use assert_matches::assert_matches;
    use dupe::Dupe;
    use futures::pin_mut;
    use futures::poll;

    use crate::dice::aquery::NodeCache;

    #[tokio::test]
    async fn test_node_cache() {
        let cache = Arc::new(NodeCache::new());

        let fut1 = cache.dupe().get_or_compute(1, |k| async move {
            tokio::task::yield_now().await;

            k
        });

        let fut2 = cache
            .dupe()
            .get_or_compute(1, |_k| async move { panic!("shouldn't run") });

        pin_mut!(fut1);
        pin_mut!(fut2);

        assert_matches!(poll!(&mut fut1), Poll::Pending);
        assert_matches!(poll!(&mut fut2), Poll::Pending);
        assert_matches!(poll!(&mut fut2), Poll::Pending);

        assert_matches!(poll!(&mut fut1), Poll::Ready(1));
        assert_matches!(poll!(&mut fut2), Poll::Ready(1));
    }
}
