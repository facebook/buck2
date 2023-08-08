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
use std::hash::Hash;
use std::pin::Pin;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context as _;
use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::deferred::data::DeferredData;
use buck2_artifact::deferred::id::DeferredId;
use buck2_artifact::deferred::key::DeferredKey;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_common::result::ToUnsharedResultExt;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::base_deferred_key::BaseDeferredKeyDyn;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_events::dispatch::create_span;
use buck2_events::dispatch::Span;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_util::late_binding::LateBinding;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::Future;
use futures::FutureExt;
use futures::StreamExt;
use futures::TryFutureExt;
use futures::TryStreamExt;
use more_futures::cancellation::CancellationContext;
use once_cell::sync::Lazy;

use crate::actions::artifact::get_artifact_fs::GetArtifactFs;
use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::analysis::AnalysisResult;
use crate::artifact_groups::calculation::ArtifactGroupCalculation;
use crate::artifact_groups::ArtifactGroup;
use crate::bxl::calculation::BXL_CALCULATION_IMPL;
use crate::bxl::result::BxlResult;
use crate::deferred::types::BaseKey;
use crate::deferred::types::DeferredInput;
use crate::deferred::types::DeferredLookup;
use crate::deferred::types::DeferredRegistry;
use crate::deferred::types::DeferredResult;
use crate::deferred::types::DeferredValueAny;
use crate::deferred::types::DeferredValueAnyReady;
use crate::deferred::types::DeferredValueReady;
use crate::deferred::types::ResolveDeferredCtx;

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "ResolveDeferred({})", _0)]
pub struct DeferredResolve(pub DeferredKey);

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "ComputeDeferred({})", _0)]
pub struct DeferredCompute(pub DeferredKey);

#[async_trait]
pub trait DeferredCalculation {
    /// Computes and returns the evaluated value of an 'DeferredData'
    async fn compute_deferred_data<T: Send + Sync + 'static>(
        &self,
        data: &DeferredData<T>,
    ) -> anyhow::Result<DeferredValueReady<T>>;
}

#[async_trait]
impl DeferredCalculation for DiceComputations {
    async fn compute_deferred_data<T: Send + Sync + 'static>(
        &self,
        data: &DeferredData<T>,
    ) -> anyhow::Result<DeferredValueReady<T>> {
        if data.deferred_key().id().is_trivial() {
            let deferred = lookup_deferred(self, data.deferred_key()).await?;
            let deferred = deferred
                .get()?
                .as_trivial()
                .context("Invalid deferred")?
                .dupe();
            return DeferredValueAnyReady::TrivialDeferred(deferred).resolve(data);
        }

        let deferred = resolve_deferred(self, data.deferred_key()).await?;
        deferred.resolve(data)
    }
}

pub static EVAL_ANON_TARGET: LateBinding<
    for<'c> fn(
        &'c DiceComputations,
        Arc<dyn BaseDeferredKeyDyn>,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<AnalysisResult>> + Send + 'c>>,
> = LateBinding::new("EVAL_ANON_TARGET");

async fn lookup_deferred_inner(
    key: &BaseDeferredKey,
    dice: &DiceComputations,
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

struct PartialLookup {
    holder: DeferredHolder,
    id: DeferredId,
}

impl PartialLookup {
    fn get(&self) -> anyhow::Result<DeferredLookup<'_>> {
        self.holder.lookup_deferred(self.id)
    }
}

/// looks up an deferred
async fn lookup_deferred(
    dice: &DiceComputations,
    key: &DeferredKey,
) -> anyhow::Result<PartialLookup> {
    Ok(match key {
        DeferredKey::Base(target, id) => {
            let holder = lookup_deferred_inner(target, dice).await?;
            PartialLookup { holder, id: *id }
        }
        DeferredKey::Deferred(key, id) => {
            let deferred = compute_deferred(dice, key).await?;
            PartialLookup {
                holder: DeferredHolder::Deferred(deferred),
                id: *id,
            }
        }
    })
}

/// Fully resolve the deferred, including any deferreds it might have return when attempting
/// to calculate it.
async fn resolve_deferred(
    dice: &DiceComputations,
    deferred: &DeferredKey,
) -> anyhow::Result<DeferredValueAnyReady> {
    #[async_trait]
    impl Key for DeferredResolve {
        type Value = SharedResult<DeferredValueAnyReady>;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellation: &CancellationContext,
        ) -> Self::Value {
            let result = compute_deferred(ctx, &self.0).await?;
            match result.value() {
                DeferredValueAny::Ready(value) => Ok(value.dupe()),
                DeferredValueAny::Deferred(key) => resolve_deferred(ctx, key).await.shared_error(),
            }
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            // TODO(bobyf) consider if we want deferreds to be eq
            false
        }
    }

    dice.compute(&DeferredResolve(deferred.dupe()))
        .await?
        .unshared_error()
}

/// Computes and returns the untyped deferred at the given key. This does not fully resolve
/// the deferred as another deferred may be returned.
async fn compute_deferred(
    dice: &DiceComputations,
    deferred: &DeferredKey,
) -> anyhow::Result<DeferredResult> {
    #[async_trait]
    impl Key for DeferredCompute {
        type Value = SharedResult<DeferredResult>;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            cancellation: &CancellationContext,
        ) -> Self::Value {
            let deferred = lookup_deferred(ctx, &self.0).await?;
            let deferred = deferred.get()?.as_complex();

            // We'll create the Span lazily when materialization hits it.
            let span = Lazy::new(|| deferred.span().map(create_span));

            let target_node_futs = FuturesUnordered::new();
            let deferreds_futs = FuturesUnordered::new();
            let mut materialized_artifacts = Vec::new();

            deferred.inputs().iter().for_each(|input| match input {
                DeferredInput::ConfiguredTarget(target) => target_node_futs.push(
                    ctx.get_configured_target_node(target)
                        .map(|res| anyhow::Ok((target.dupe(), res?.require_compatible()?))),
                ),
                DeferredInput::Deferred(deferred_key) => deferreds_futs.push(
                    resolve_deferred(ctx, deferred_key)
                        .map(|res| anyhow::Ok((deferred_key.dupe(), res?))),
                ),
                DeferredInput::MaterializedArtifact(artifact) => {
                    materialized_artifacts.push(ArtifactGroup::Artifact(artifact.dupe()));
                }
            });

            let materialized_artifacts_fut =
                self.create_materializer_futs(&materialized_artifacts, ctx, &span);

            // TODO(nga): do we need to compute artifacts?
            let (targets, deferreds, materialized_artifacts) = futures::future::join3(
                futures_pair_to_map(target_node_futs),
                futures_pair_to_map(deferreds_futs),
                materialized_artifacts_fut,
            )
            .await;

            let mut registry = DeferredRegistry::new(BaseKey::Deferred(Arc::new(self.0.dupe())));

            cancellation
                .with_structured_cancellation(|observer| {
                    async move {
                        let mut deferred_ctx = ResolveDeferredCtx::new(
                            self.0.dupe(),
                            targets?,
                            deferreds?,
                            materialized_artifacts?,
                            &mut registry,
                            ctx.global_data().get_io_provider().project_root().dupe(),
                            ctx.global_data().get_digest_config(),
                            observer,
                        );

                        let execute = deferred.execute(&mut deferred_ctx, ctx);

                        let res = match Lazy::into_value(span).unwrap_or_else(|init| init()) {
                            Some(span) => {
                                span.wrap_future(async {
                                    (execute.await, buck2_data::DeferredEvaluationEnd {})
                                })
                                .await
                            }
                            None => execute.await,
                        };

                        // TODO populate the deferred map
                        Ok(DeferredResult::new(res?, registry.take_result()?))
                    }
                    .boxed()
                })
                .await
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            false
        }

        fn validity(x: &Self::Value) -> bool {
            x.is_ok()
        }
    }

    impl DeferredCompute {
        fn create_materializer_futs<'a>(
            &'a self,
            materialized_artifacts: &'a Vec<ArtifactGroup>,
            ctx: &'a DiceComputations,
            span: &'a Lazy<Option<Span>, impl FnOnce() -> Option<Span>>,
        ) -> impl Future<Output = anyhow::Result<HashMap<Artifact, ProjectRelativePathBuf>>> + 'a
        {
            if materialized_artifacts.is_empty() {
                return async move { Ok(HashMap::new()) }.left_future();
            }
            // This is a bit suboptimal: we wait for all artifacts to be ready in order to
            // materialize any of them. However that is how we execute *all* local actions so in
            // the grand scheme of things that's probably not a huge deal.
            let materialized_artifacts_fut = {
                let artifact_futs = futures::future::try_join_all(
                    materialized_artifacts
                        .iter()
                        .map(|artifact| ctx.ensure_artifact_group(artifact)),
                );

                artifact_futs.and_then(move |_| async move {
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
                            .wrap_future(
                                fut.map(|r| (r, buck2_data::DeferredPreparationStageEnd {})),
                            )
                            .await
                        }
                        None => fut.await,
                    }
                })
            };
            materialized_artifacts_fut.right_future()
        }
    }

    Ok(dice.compute(&DeferredCompute(deferred.dupe())).await??)
}

async fn futures_pair_to_map<K: Eq + Hash, V>(
    mut futs: FuturesUnordered<impl Future<Output = anyhow::Result<(K, V)>>>,
) -> anyhow::Result<HashMap<K, V>> {
    let mut res = HashMap::with_capacity(futs.len());
    while let Some(p) = futs.next().await {
        // terminate immediately once a fail occurs
        // TODO support keep going?

        let (key, value) = p?;
        res.insert(key, value);
    }
    Ok(res)
}

/// Represents an Analysis or Deferred result. Technically, we can treat analysis as a 'Deferred'
/// and get rid of this enum
enum DeferredHolder {
    Analysis(AnalysisResult),
    Bxl(Arc<BxlResult>),
    Deferred(DeferredResult),
}

impl DeferredHolder {
    fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<DeferredLookup<'_>> {
        match self {
            DeferredHolder::Analysis(result) => result.lookup_deferred(id),
            DeferredHolder::Deferred(result) => result.lookup_deferred(id),
            DeferredHolder::Bxl(result) => result.lookup_deferred(id),
        }
    }
}
