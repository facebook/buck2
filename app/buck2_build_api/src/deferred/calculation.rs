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
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context as _;
use async_trait::async_trait;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_common::result::ToUnsharedResultExt;
use buck2_events::dispatch::create_span;
use buck2_execute::digest_config::HasDigestConfig;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::Future;
use futures::FutureExt;
use futures::StreamExt;
use futures::TryStreamExt;
use gazebo::prelude::*;
use more_futures::cancellation::CancellationContext;
use once_cell::sync::Lazy;

use crate::actions::artifact::materializer::ArtifactMaterializer;
use crate::analysis::anon_targets::eval_anon_target;
use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::analysis::AnalysisResult;
use crate::artifact_groups::calculation::ArtifactGroupCalculation;
use crate::artifact_groups::ArtifactGroup;
use crate::bxl::calculation::BxlCalculation;
use crate::bxl::result::BxlResult;
use crate::deferred::base_deferred_key::BaseDeferredKey;
use crate::deferred::types::BaseKey;
use crate::deferred::types::DeferredData;
use crate::deferred::types::DeferredId;
use crate::deferred::types::DeferredInput;
use crate::deferred::types::DeferredKey;
use crate::deferred::types::DeferredLookup;
use crate::deferred::types::DeferredRegistry;
use crate::deferred::types::DeferredResult;
use crate::deferred::types::DeferredValueAny;
use crate::deferred::types::DeferredValueAnyReady;
use crate::deferred::types::DeferredValueReady;
use crate::deferred::types::ResolveDeferredCtx;
use crate::nodes::calculation::NodeCalculation;

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
            return data.resolve(DeferredValueAnyReady::TrivialDeferred(deferred));
        }

        let deferred = resolve_deferred(self, data.deferred_key()).await?;
        Ok(data.resolve(deferred)?)
    }
}

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
            let bxl_result = dice.eval_bxl(bxl.dupe()).await?.bxl_result;

            Ok(DeferredHolder::Bxl(bxl_result))
        }
        BaseDeferredKey::AnonTarget(target) => Ok(DeferredHolder::Analysis(
            eval_anon_target(dice, target).await?,
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
            ctx: &DiceComputations,
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
            ctx: &DiceComputations,
            _cancellation: &CancellationContext,
        ) -> Self::Value {
            let deferred = lookup_deferred(ctx, &self.0).await?;
            let deferred = deferred.get()?.as_complex();

            let target_node_futs = FuturesUnordered::new();
            let deferreds_futs = FuturesUnordered::new();
            let artifacts_futs = FuturesUnordered::new();
            let materialized_artifacts = FuturesUnordered::new();

            deferred.inputs().iter().for_each(|input| match input {
                DeferredInput::ConfiguredTarget(target) => target_node_futs.push(async move {
                    Ok((
                        target.dupe(),
                        ctx.get_configured_target_node(target)
                            .await?
                            .require_compatible()?,
                    ))
                }),
                DeferredInput::Deferred(deferred_key) => {
                    let deferred_key = deferred_key.dupe();
                    deferreds_futs.push(async move {
                        Ok((
                            deferred_key.dupe(),
                            resolve_deferred(ctx, &deferred_key).await?,
                        ))
                    })
                }
                DeferredInput::Artifact(artifact) => {
                    // TODO ():
                    let artifact = artifact.dupe();
                    artifacts_futs.push(async move {
                        Ok((
                            artifact.dupe(),
                            // TODO(bobyf) import artifact calculation
                            ctx.ensure_artifact_group(&ArtifactGroup::Artifact(artifact))
                                .await?
                                .iter()
                                .into_singleton()
                                .context("Expected Artifact to yield a single value")?
                                .1
                                .dupe(),
                        ))
                    })
                }
                DeferredInput::MaterializedArtifact(artifact) => {
                    materialized_artifacts.push(artifact.dupe());
                }
            });

            // We'll create the Span lazily when materialization hits it.
            let span = Lazy::new(|| deferred.span().map(create_span));

            // This is a bit suboptimal: we wait for all artifacts to be ready in order to
            // materialize any of them. However that is how we execute *all* local actions so in
            // the grand scheme of things that's probably not a huge deal.
            let materialized_artifacts_fut = {
                let span = &span;
                async move {
                    futures::future::try_join_all(materialized_artifacts.iter().map(
                        |artifact| async {
                            ctx.ensure_artifact_group(&ArtifactGroup::Artifact(artifact.dupe()))
                                .await
                        },
                    ))
                    .await?;

                    let fut = materialized_artifacts
                        .into_iter()
                        .map(|artifact| async {
                            let path = ArtifactMaterializer::materialize(ctx, &artifact).await?;
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
                }
            };

            // TODO(nga): do we need to compute artifacts?
            let (targets, deferreds, _artifacts, materialized_artifacts) = futures::future::join4(
                futures_pair_to_map(target_node_futs),
                futures_pair_to_map(deferreds_futs),
                futures_pair_to_map(artifacts_futs),
                materialized_artifacts_fut,
            )
            .await;

            let mut registry = DeferredRegistry::new(BaseKey::Deferred(Arc::new(self.0.dupe())));
            let mut ctx = ResolveDeferredCtx::new(
                self.0.dupe(),
                targets?,
                deferreds?,
                materialized_artifacts?,
                &mut registry,
                ctx.global_data().get_io_provider().project_root().dupe(),
                ctx.global_data().get_digest_config(),
            );

            let mut execute = move || deferred.execute(&mut ctx);

            let res = match Lazy::into_value(span).unwrap_or_else(|init| init()) {
                Some(span) => {
                    span.wrap_closure(|| (execute(), buck2_data::DeferredEvaluationEnd {}))
                }
                None => execute(),
            };

            // TODO populate the deferred map
            Ok(DeferredResult::new(res?, registry.take_result()?))
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            false
        }

        fn validity(x: &Self::Value) -> bool {
            x.is_ok()
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

#[cfg(test)]
mod tests {
    use std::any;
    use std::any::Demand;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;

    use allocative::Allocative;
    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_common::executor_config::CommandExecutorConfig;
    use buck2_common::result::ToSharedResultExt;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::target::label::TargetLabel;
    use buck2_execute::digest_config::DigestConfig;
    use buck2_execute::digest_config::SetDigestConfig;
    use buck2_execute::execute::dice_data::set_fallback_executor_config;
    use buck2_node::nodes::configured::ConfiguredTargetNode;
    use buck2_query::query::compatibility::MaybeCompatible;
    use dice::testing::DiceBuilder;
    use dice::UserComputationData;
    use dupe::Dupe;
    use indexmap::IndexSet;
    use indoc::indoc;

    use crate::analysis::calculation::AnalysisKey;
    use crate::analysis::AnalysisResult;
    use crate::deferred::base_deferred_key::BaseDeferredKey;
    use crate::deferred::calculation::DeferredCalculation;
    use crate::deferred::types::BaseKey;
    use crate::deferred::types::Deferred;
    use crate::deferred::types::DeferredCtx;
    use crate::deferred::types::DeferredInput;
    use crate::deferred::types::DeferredRegistry;
    use crate::deferred::types::DeferredTable;
    use crate::deferred::types::DeferredValue;
    use crate::interpreter::rule_defs::provider::testing::FrozenProviderCollectionValueExt;
    use crate::nodes::calculation::ConfiguredTargetNodeKey;

    #[derive(Allocative)]
    struct FakeDeferred(usize, IndexSet<DeferredInput>, Arc<AtomicBool>);

    impl any::Provider for FakeDeferred {
        fn provide<'a>(&'a self, _demand: &mut Demand<'a>) {}
    }

    impl Deferred for FakeDeferred {
        type Output = usize;

        fn inputs(&self) -> &IndexSet<DeferredInput> {
            &self.1
        }

        fn execute(
            &self,
            _ctx: &mut dyn DeferredCtx,
        ) -> anyhow::Result<DeferredValue<Self::Output>> {
            self.2.store(true, Ordering::SeqCst);
            Ok(DeferredValue::Ready(self.0))
        }
    }

    #[tokio::test]
    async fn lookup_deferred_from_analysis() -> anyhow::Result<()> {
        let target =
            TargetLabel::testing_parse("cell//pkg:foo").configure(ConfigurationData::testing_new());
        let analysis_key = AnalysisKey(target.dupe());
        let configured_node_key = ConfiguredTargetNodeKey(target.dupe());

        let provider_collection = FrozenProviderCollectionValueExt::testing_new(indoc!(
            r#"
            Foo = provider(fields=["x"])
            [DefaultInfo(default_outputs=[]), Foo(x=1)]
            "#
        ));

        let mut deferred =
            DeferredRegistry::new(BaseKey::Base(BaseDeferredKey::TargetLabel(target.dupe())));

        let executed0 = Arc::new(AtomicBool::new(false));
        let executed1 = Arc::new(AtomicBool::new(false));
        let data0 = deferred.defer(FakeDeferred(1, IndexSet::new(), executed0.dupe()));
        let data1 = deferred.defer(FakeDeferred(5, IndexSet::new(), executed1.dupe()));
        let deferred_result = DeferredTable::new(deferred.take_result()?);

        let fs = ProjectRootTemp::new()?;
        let dice = DiceBuilder::new()
            .set_data(|data| {
                data.set_testing_io_provider(&fs);
                data.set_digest_config(DigestConfig::testing_default());
            })
            .mock_and_return(
                analysis_key,
                anyhow::Ok(MaybeCompatible::Compatible(AnalysisResult::new(
                    provider_collection,
                    deferred_result,
                    None,
                )))
                .shared_error(),
            )
            .mock_and_return(
                configured_node_key,
                Ok(MaybeCompatible::Compatible(
                    ConfiguredTargetNode::testing_new(target.dupe(), "foo_lib"),
                )),
            );

        let mut dice_data = UserComputationData::new();
        set_fallback_executor_config(&mut dice_data.data, CommandExecutorConfig::testing_local());

        let dice = dice.build(dice_data)?.commit().await;
        let deferred_result = dice.compute_deferred_data(&data0).await?;
        assert_eq!(*deferred_result, 1);
        assert_eq!(executed0.load(Ordering::SeqCst), true);
        // we should cache deferred execution
        executed0.store(false, Ordering::SeqCst);
        let deferred_result = dice.compute_deferred_data(&data0).await?;
        assert_eq!(*deferred_result, 1);
        assert_eq!(executed0.load(Ordering::SeqCst), false);

        let deferred_result = dice.compute_deferred_data(&data1).await?;
        assert_eq!(*deferred_result, 5);
        assert_eq!(executed1.load(Ordering::SeqCst), true);
        // we should cache deferred execution
        executed1.store(false, Ordering::SeqCst);
        assert_eq!(*deferred_result, 5);
        assert_eq!(executed1.load(Ordering::SeqCst), false);

        Ok(())
    }

    #[tokio::test]
    async fn lookup_deferred_that_has_deferreds() -> anyhow::Result<()> {
        #[derive(Allocative)]
        struct DeferringDeferred(usize, IndexSet<DeferredInput>, Arc<AtomicBool>);

        impl any::Provider for DeferringDeferred {
            fn provide<'a>(&'a self, _demand: &mut Demand<'a>) {}
        }

        impl Deferred for DeferringDeferred {
            type Output = usize;

            fn inputs(&self) -> &IndexSet<DeferredInput> {
                &self.1
            }

            fn execute(
                &self,
                ctx: &mut dyn DeferredCtx,
            ) -> anyhow::Result<DeferredValue<Self::Output>> {
                let data =
                    ctx.registry()
                        .defer(FakeDeferred(self.0, self.1.clone(), self.2.dupe()));
                Ok(DeferredValue::Deferred(data))
            }
        }

        let target =
            TargetLabel::testing_parse("cell//pkg:foo").configure(ConfigurationData::testing_new());
        let analysis_key = AnalysisKey(target.dupe());
        let configured_node_key = ConfiguredTargetNodeKey(target.dupe());

        let provider_collection = FrozenProviderCollectionValueExt::testing_new(indoc!(
            r#"
            Foo = provider(fields=["x"])
            [DefaultInfo(default_outputs=[]), Foo(x=1)]
            "#
        ));

        let mut deferred =
            DeferredRegistry::new(BaseKey::Base(BaseDeferredKey::TargetLabel(target.dupe())));

        let executed = Arc::new(AtomicBool::new(false));
        let data = deferred.defer(DeferringDeferred(8, IndexSet::new(), executed.dupe()));
        let deferred_result = DeferredTable::new(deferred.take_result()?);

        let fs = ProjectRootTemp::new()?;
        let dice = DiceBuilder::new()
            .set_data(|data| {
                data.set_testing_io_provider(&fs);
                data.set_digest_config(DigestConfig::testing_default());
            })
            .mock_and_return(
                analysis_key,
                anyhow::Ok(MaybeCompatible::Compatible(AnalysisResult::new(
                    provider_collection,
                    deferred_result,
                    None,
                )))
                .shared_error(),
            )
            .mock_and_return(
                configured_node_key,
                Ok(MaybeCompatible::Compatible(
                    ConfiguredTargetNode::testing_new(target.dupe(), "foo_lib"),
                )),
            );

        let mut dice_data = UserComputationData::new();
        set_fallback_executor_config(&mut dice_data.data, CommandExecutorConfig::testing_local());

        let dice = dice.build(dice_data)?.commit().await;
        let deferred_result = dice.compute_deferred_data(&data).await?;
        assert_eq!(*deferred_result, 8);
        assert_eq!(executed.load(Ordering::SeqCst), true);
        // we should cache deferred execution
        executed.store(false, Ordering::SeqCst);
        let deferred_result = dice.compute_deferred_data(&data).await?;
        assert_eq!(*deferred_result, 8);
        assert_eq!(executed.load(Ordering::SeqCst), false);

        Ok(())
    }
}
