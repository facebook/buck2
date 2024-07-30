/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Dice calculations relating to deferreds

use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;

use allocative::Allocative;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::deferred::key::DeferredHolderKey;
use buck2_artifact::dynamic::DynamicLambdaResultsKey;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::base_deferred_key::BaseDeferredKeyDyn;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::internal_error;
use buck2_events::dispatch::create_span;
use buck2_events::dispatch::Span;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_futures::cancellation::CancellationContext;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::Future;
use futures::FutureExt;
use futures::TryStreamExt;
use once_cell::sync::Lazy;
use starlark::values::OwnedFrozenValueTyped;

use crate::actions::artifact::get_artifact_fs::GetArtifactFs;
use crate::actions::RegisteredAction;
use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::analysis::registry::RecordedAnalysisValues;
use crate::analysis::AnalysisResult;
use crate::artifact_groups::calculation::ArtifactGroupCalculation;
use crate::artifact_groups::deferred::TransitiveSetKey;
use crate::artifact_groups::promise::PromiseArtifact;
use crate::artifact_groups::ArtifactGroup;
use crate::bxl::calculation::BXL_CALCULATION_IMPL;
use crate::bxl::result::BxlResult;
use crate::deferred::arc_borrow::ArcBorrow;
use crate::deferred::types::Deferred;
use crate::deferred::types::DeferredInput;
use crate::dynamic::calculation::compute_dynamic_lambda;
use crate::dynamic::calculation::DynamicLambdaResult;
use crate::dynamic::lambda::DynamicLambda;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;

pub static EVAL_ANON_TARGET: LateBinding<
    for<'c> fn(
        &'c mut DiceComputations,
        Arc<dyn BaseDeferredKeyDyn>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<AnalysisResult>> + Send + 'c>>,
> = LateBinding::new("EVAL_ANON_TARGET");

pub static GET_PROMISED_ARTIFACT: LateBinding<
    for<'c> fn(
        &'c PromiseArtifact,
        &'c mut DiceComputations,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<Artifact>> + Send + 'c>>,
> = LateBinding::new("GET_PROMISED_ARTIFACT");

async fn lookup_deferred_inner(
    key: &BaseDeferredKey,
    dice: &mut DiceComputations<'_>,
) -> anyhow::Result<DeferredHolder> {
    match key {
        BaseDeferredKey::TargetLabel(target) => {
            let analysis = dice
                .get_analysis_result(target)
                .await?
                .require_compatible()?;

            Ok(DeferredHolder::Analysis(analysis))
        }
        BaseDeferredKey::BxlLabel(bxl) => {
            let bxl_result = BXL_CALCULATION_IMPL
                .get()?
                .eval_bxl(dice, bxl.dupe())
                .await?
                .bxl_result;

            Ok(DeferredHolder::Bxl(bxl_result))
        }
        BaseDeferredKey::AnonTarget(target) => Ok(DeferredHolder::Analysis(
            (EVAL_ANON_TARGET.get()?)(dice, target.dupe()).await?,
        )),
    }
}

pub async fn lookup_deferred_holder(
    dice: &mut DiceComputations<'_>,
    key: &DeferredHolderKey,
) -> anyhow::Result<DeferredHolder> {
    Ok(match key {
        DeferredHolderKey::Base(key) => lookup_deferred_inner(key, dice).await?,
        DeferredHolderKey::DynamicLambda(lambda) => {
            DeferredHolder::DynamicLambda(compute_dynamic_lambda(dice, lambda).await?)
        }
    })
}

pub async fn prepare_and_execute_deferred(
    ctx: &mut DiceComputations<'_>,
    cancellation: &CancellationContext<'_>,
    deferred: ArcBorrow<'_, dyn Deferred>,
    self_holder_key: DeferredHolderKey,
    action_key: String,
) -> buck2_error::Result<RecordedAnalysisValues> {
    // We'll create the Span lazily when materialization hits it.
    let span = Lazy::new(|| deferred.span().map(create_span));

    let mut target_deps = Vec::new();
    let mut materialized_artifacts = Vec::new();

    for input in deferred.inputs() {
        match input {
            DeferredInput::ConfiguredTarget(target) => target_deps.push(target),
            DeferredInput::MaterializedArtifact(artifact) => {
                materialized_artifacts.push(ArtifactGroup::Artifact(artifact.dupe()))
            }
        }
    }

    let (targets, materialized_artifacts) = {
        // don't move span
        let span = &span;
        ctx.try_compute2(
            |ctx| {
                async move {
                    ctx.try_compute_join(target_deps, |ctx, target| {
                        async move {
                            let res = ctx
                                .get_configured_target_node(target)
                                .await?
                                .require_compatible()?;
                            anyhow::Ok((target.dupe(), res))
                        }
                        .boxed()
                    })
                    .await
                }
                .boxed()
            },
            |ctx| {
                async move { create_materializer_futs(&materialized_artifacts, ctx, span).await }
                    .boxed()
            },
        )
        .await?
    };

    cancellation
        .with_structured_cancellation(|observer| {
            async move {
                let execute = deferred.execute(
                    ctx,
                    self_holder_key,
                    action_key,
                    targets.into_iter().collect(),
                    materialized_artifacts,
                    ctx.global_data().get_io_provider().project_root().dupe(),
                    ctx.global_data().get_digest_config(),
                    observer,
                );

                let recorded_values = match Lazy::into_value(span).unwrap_or_else(|init| init()) {
                    Some(span) => {
                        span.wrap_future(async {
                            (execute.await, buck2_data::DeferredEvaluationEnd {})
                        })
                        .await
                    }
                    None => execute.await,
                }?;

                Ok(recorded_values)
            }
            .boxed()
        })
        .await
}

async fn create_materializer_futs(
    materialized_artifacts: &[ArtifactGroup],
    ctx: &mut DiceComputations<'_>,
    span: &Lazy<Option<Span>, impl FnOnce() -> Option<Span>>,
) -> anyhow::Result<HashMap<Artifact, ProjectRelativePathBuf>> {
    if materialized_artifacts.is_empty() {
        return Ok(HashMap::new());
    }
    // This is a bit suboptimal: we wait for all artifacts to be ready in order to
    // materialize any of them. However that is how we execute *all* local actions so in
    // the grand scheme of things that's probably not a huge deal.

    ctx.try_compute_join(materialized_artifacts, |ctx, artifact| {
        ctx.ensure_artifact_group(artifact).boxed()
    })
    .await?;

    let materializer = ctx.per_transaction_data().get_materializer();
    let artifact_fs = ctx.get_artifact_fs().await?;

    let fut = materialized_artifacts
        .iter()
        .map(|artifact| async {
            let artifact = artifact
                .unpack_artifact()
                .expect("we only put Artifacts into this list")
                .dupe();
            let path = artifact.resolve_path(&artifact_fs)?;
            materializer.ensure_materialized(vec![path.clone()]).await?;

            anyhow::Ok((artifact, path))
        })
        .collect::<FuturesUnordered<_>>()
        .try_collect::<HashMap<_, _>>();

    match span.as_ref() {
        Some(span) => {
            span.create_child(buck2_data::DeferredPreparationStageStart {
                stage: Some(buck2_data::MaterializedArtifacts {}.into()),
            })
            .wrap_future(fut.map(|r| (r, buck2_data::DeferredPreparationStageEnd {})))
            .await
        }
        None => fut.await,
    }
}

/// Represents an Analysis or Deferred result. Technically, we can treat analysis as a 'Deferred'
/// and get rid of this enum
pub enum DeferredHolder {
    Analysis(AnalysisResult),
    Bxl(Arc<BxlResult>),
    DynamicLambda(Arc<DynamicLambdaResult>),
}

impl DeferredHolder {
    pub(crate) fn lookup_transitive_set(
        &self,
        key: &TransitiveSetKey,
    ) -> anyhow::Result<OwnedFrozenValueTyped<FrozenTransitiveSet>> {
        self.analysis_values()
            .lookup_transitive_set(key)
            .ok_or_else(|| internal_error!("Missing transitive set `{}`", key))
    }

    pub(crate) fn lookup_action(&self, key: &ActionKey) -> anyhow::Result<ActionLookup> {
        self.analysis_values().lookup_action(key)
    }

    pub fn lookup_lambda(
        &self,
        key: &DynamicLambdaResultsKey,
    ) -> anyhow::Result<Arc<DynamicLambda>> {
        self.analysis_values().lookup_lambda(key)
    }

    fn analysis_values(&self) -> &RecordedAnalysisValues {
        match self {
            DeferredHolder::Analysis(result) => result.analysis_values(),
            DeferredHolder::Bxl(result) => result.analysis_values(),
            DeferredHolder::DynamicLambda(result) => result.analysis_values(),
        }
    }
}

#[derive(Debug, Allocative, Clone, Dupe)]
pub enum ActionLookup {
    Action(Arc<RegisteredAction>),
    Deferred(ActionKey),
}
