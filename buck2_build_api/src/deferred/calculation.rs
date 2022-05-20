/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Dice calculations relating to deferreds

use std::{collections::HashMap, hash::Hash, sync::Arc};

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::result::SharedResult;
use derive_more::Display;
use dice::{DiceComputations, Key};
use futures::{stream::FuturesUnordered, Future, StreamExt};
use gazebo::prelude::*;
use owning_ref::{ArcRef, BoxRef};

use crate::{
    analysis::{calculation::RuleAnalysisCalculation, AnalysisResult},
    artifact_groups::ArtifactGroup,
    bxl::{calculation::BxlCalculation, result::BxlResult},
    deferred::{
        calculation::keys::DeferredResolve, AnyValue, BaseDeferredKey, BaseKey, DeferredAny,
        DeferredData, DeferredId, DeferredInput, DeferredKey, DeferredRegistry, DeferredResult,
        DeferredValueAny, ResolveDeferredCtx,
    },
};

#[async_trait]
pub(crate) trait DeferredCalculation {
    /// Computes and returns the evaluated value of an 'DeferredData'
    async fn compute_deferred_data<T: Send + Sync + 'static>(
        &self,
        data: &DeferredData<T>,
    ) -> SharedResult<ArcRef<dyn AnyValue, T>>;
}

#[async_trait]
impl DeferredCalculation for DiceComputations {
    async fn compute_deferred_data<T: Send + Sync + 'static>(
        &self,
        data: &DeferredData<T>,
    ) -> SharedResult<ArcRef<dyn AnyValue, T>> {
        let deferred = resolve_deferred(self, data.deferred_key()).await?;
        Ok(data.resolve(deferred)?)
    }
}

impl BaseDeferredKey {
    async fn lookup_deferred(&self, dice: &DiceComputations) -> SharedResult<DeferredHolder> {
        match self {
            BaseDeferredKey::TargetLabel(target) => {
                let analysis = dice
                    .get_analysis_result(target)
                    .await?
                    .require_compatible()?;

                Ok(DeferredHolder::Analysis(analysis))
            }
            BaseDeferredKey::BxlLabel(bxl) => {
                let bxl_result = dice.eval_bxl(bxl.dupe()).await?;

                Ok(DeferredHolder::Bxl(bxl_result))
            }
        }
    }
}

/// looks up an deferred
async fn lookup_deferred(
    dice: &DiceComputations,
    key: &DeferredKey,
) -> SharedResult<BoxRef<DeferredHolder, dyn DeferredAny>> {
    Ok(match key {
        DeferredKey::Base(target, id) => {
            let deferred_or_analysis = target.lookup_deferred(dice).await?;

            BoxRef::new(box deferred_or_analysis).try_map(|a| a.lookup_deferred(*id))
        }
        DeferredKey::Deferred(key, id) => {
            let deferred = compute_deferred(dice, key).await?;
            BoxRef::new(box DeferredHolder::Deferred(deferred)).try_map(|a| a.lookup_deferred(*id))
        }
    }?)
}

/// Fully resolve the deferred, including any deferreds it might have return when attempting
/// to calculate it.
async fn resolve_deferred(
    dice: &DiceComputations,
    deferred: &DeferredKey,
) -> SharedResult<Arc<dyn AnyValue>> {
    #[async_trait]
    impl Key for DeferredResolve {
        type Value = SharedResult<Arc<dyn AnyValue>>;

        async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
            let result = compute_deferred(ctx, &self.0).await?;
            match result.value() {
                DeferredValueAny::Ready(value) => Ok(value.dupe()),
                DeferredValueAny::Deferred(key) => resolve_deferred(ctx, key).await,
            }
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            // TODO(bobyf) consider if we want deferreds to be eq
            false
        }
    }

    dice.compute(&DeferredResolve(deferred.dupe())).await
}

/// Computes and returns the untyped deferred at the given key. This does not fully resolve
/// the deferred as another deferred may be returned.
async fn compute_deferred(
    dice: &DiceComputations,
    deferred: &DeferredKey,
) -> SharedResult<DeferredResult> {
    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
    #[display(fmt = "ComputeDeferred({})", _0)]
    struct DeferredCompute(DeferredKey);

    #[async_trait]
    impl Key for DeferredCompute {
        type Value = SharedResult<DeferredResult>;

        async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
            let deferred = lookup_deferred(ctx, &self.0).await?;

            let target_node_futs = FuturesUnordered::new();
            let providers_futs = FuturesUnordered::new();
            let deferreds_futs = FuturesUnordered::new();
            let artifacts_futs = FuturesUnordered::new();
            let materialized_artifacts_futs = FuturesUnordered::new();

            deferred.inputs().iter().for_each(|input| match input {
                DeferredInput::ConfiguredTarget(target) => target_node_futs.push(async move {
                    Ok((
                        target.dupe(),
                        crate::calculation::Calculation::get_configured_target_node(ctx, target)
                            .await?
                            .require_compatible()?,
                    ))
                }),
                DeferredInput::Provider(provider_label) => {
                    let provider_label = provider_label.clone();
                    providers_futs.push(async move {
                        let res = ctx
                            .get_analysis_result(provider_label.target())
                            .await?
                            .require_compatible()?;
                        // TODO: This module needs to be passed into `deferred.execute()`
                        let inner = res.lookup_inner(&provider_label)?;
                        Ok((provider_label.clone(), inner))
                    })
                }
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
                            crate::calculation::Calculation::ensure_artifact_group(
                                ctx,
                                &ArtifactGroup::Artifact(artifact),
                            )
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
                    let artifact = artifact.dupe();
                    materialized_artifacts_futs.push(async move {
                        crate::calculation::Calculation::ensure_artifact_group(
                            ctx,
                            &ArtifactGroup::Artifact(artifact.dupe()),
                        )
                        .await?;
                        let path = crate::execute::materializer::ArtifactMaterializer::materialize(
                            ctx, &artifact,
                        )
                        .await?;
                        Ok((artifact.dupe(), path))
                    })
                }
            });

            let (targets, providers, deferreds, artifacts, materialized_artifacts) =
                futures::future::join5(
                    futures_pair_to_map(target_node_futs),
                    futures_pair_to_map(providers_futs),
                    futures_pair_to_map(deferreds_futs),
                    futures_pair_to_map(artifacts_futs),
                    futures_pair_to_map(materialized_artifacts_futs),
                )
                .await;

            let mut registry = DeferredRegistry::new(BaseKey::Deferred(Arc::new(self.0.dupe())));
            let mut ctx = ResolveDeferredCtx::new(
                self.0.dupe(),
                targets?,
                providers?,
                deferreds?,
                artifacts?,
                materialized_artifacts?,
                &mut registry,
                ctx.global_data().get_io_provider().fs().dupe(),
            );
            // TODO populate the deferred map
            Ok(DeferredResult::new(
                deferred.execute(&mut ctx)?,
                registry.take_result()?,
            ))
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            false
        }
    }

    dice.compute(&DeferredCompute(deferred.dupe())).await
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
    fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<&(dyn DeferredAny + 'static)> {
        match self {
            DeferredHolder::Analysis(result) => result.lookup_deferred(id),
            DeferredHolder::Deferred(result) => result.lookup_deferred(id),
            DeferredHolder::Bxl(result) => result.lookup_deferred(id),
        }
    }
}

mod keys {
    use derive_more::Display;
    use gazebo::prelude::*;

    use crate::deferred::DeferredKey;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
    #[display(fmt = "ResolveDeferred({})", _0)]
    pub struct DeferredResolve(pub DeferredKey);
}

#[cfg(test)]
pub mod testing {
    // re-exports for testing
    pub use super::keys::DeferredResolve;
}

#[cfg(test)]
mod tests {
    use std::sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    };

    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_core::{
        configuration::Configuration,
        fs::project::ProjectFilesystemTemp,
        result::ToSharedResultExt,
        target::{testing::TargetLabelExt, TargetLabel},
    };
    use buck2_interpreter::common::{BxlFilePath, ImportPath};
    use dice::{testing::DiceBuilder, UserComputationData};
    use gazebo::prelude::*;
    use indexmap::IndexSet;
    use indoc::indoc;
    use starlark::collections::SmallMap;

    use crate::{
        analysis::{calculation::testing::AnalysisKey, AnalysisResult},
        bxl::{calculation::testing::BxlComputeKey, result::BxlResult, BxlFunctionLabel, BxlKey},
        configuration::execution::ExecutionPlatformResolution,
        deferred::{
            calculation::DeferredCalculation, BaseDeferredKey, BaseKey, Deferred, DeferredCtx,
            DeferredInput, DeferredRegistry, DeferredTable, DeferredValue,
        },
        execute::{commands::dice_data::set_fallback_executor_config, ActionExecutorConfig},
        interpreter::rule_defs::provider::testing::FrozenProviderCollectionValueExt,
        nodes::{
            calculation::ConfiguredNodeKey, compatibility::MaybeCompatible,
            configured::ConfiguredTargetNode, RuleType, StarlarkRuleType,
        },
    };

    struct FakeDeferred(usize, IndexSet<DeferredInput>, Arc<AtomicBool>);

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
            TargetLabel::testing_parse("cell//pkg:foo").configure(Configuration::testing_new());
        let analysis_key = AnalysisKey(target.dupe());
        let configured_node_key = ConfiguredNodeKey(target.dupe());

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

        let fs = ProjectFilesystemTemp::new()?;
        let dice = DiceBuilder::new()
            .set_data(|data| data.set_testing_io_provider(&fs))
            .mock_and_return(
                analysis_key,
                anyhow::Ok(MaybeCompatible::Compatible(AnalysisResult::new(
                    provider_collection,
                    deferred_result,
                )))
                .shared_error(),
            )
            .mock_and_return(
                configured_node_key,
                Ok(MaybeCompatible::Compatible(
                    ConfiguredTargetNode::testing_new(
                        target.dupe(),
                        RuleType::Starlark(Arc::new(StarlarkRuleType {
                            import_path: ImportPath::unchecked_new("cell", "pkg", "rules.bzl"),
                            name: "foo_lib".to_owned(),
                        })),
                        Vec::new(),
                        ExecutionPlatformResolution::new(None, Vec::new()),
                    ),
                )),
            );

        let mut dice_data = UserComputationData::new();
        set_fallback_executor_config(&mut dice_data.data, ActionExecutorConfig::testing_local());

        let dice = dice.build(dice_data);
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
    async fn lookup_deferred_from_bxl() -> anyhow::Result<()> {
        let bxl = BxlKey::new(
            BxlFunctionLabel {
                bxl_path: BxlFilePath::unchecked_new("cell", "dir"),
                name: "foo".to_owned(),
            },
            Arc::new(SmallMap::new()),
        );

        let mut deferred =
            DeferredRegistry::new(BaseKey::Base(BaseDeferredKey::BxlLabel(bxl.dupe())));

        let executed0 = Arc::new(AtomicBool::new(false));
        let executed1 = Arc::new(AtomicBool::new(false));
        let data0 = deferred.defer(FakeDeferred(1, IndexSet::new(), executed0.dupe()));
        let data1 = deferred.defer(FakeDeferred(5, IndexSet::new(), executed1.dupe()));
        let deferred_result = DeferredTable::new(deferred.take_result()?);

        let fs = ProjectFilesystemTemp::new()?;
        let dice = DiceBuilder::new()
            .set_data(|data| data.set_testing_io_provider(&fs))
            .mock_and_return(
                BxlComputeKey(bxl.dupe()),
                anyhow::Ok(Arc::new(BxlResult::BuildsArtifacts {
                    has_print: false,
                    built: vec![],
                    artifacts: vec![],
                    deferred: deferred_result,
                }))
                .shared_error(),
            );

        let mut dice_data = UserComputationData::new();
        set_fallback_executor_config(&mut dice_data.data, ActionExecutorConfig::testing_local());

        let dice = dice.build(dice_data);
        let deferred_result = dice.compute_deferred_data(&data0).await?;
        assert_eq!(*deferred_result, 1);
        assert!(executed0.load(Ordering::SeqCst));
        // we should cache deferred execution
        executed0.store(false, Ordering::SeqCst);
        let deferred_result = dice.compute_deferred_data(&data0).await?;
        assert_eq!(*deferred_result, 1);
        assert!(!executed0.load(Ordering::SeqCst));

        let deferred_result = dice.compute_deferred_data(&data1).await?;
        assert_eq!(*deferred_result, 5);
        assert!(executed1.load(Ordering::SeqCst));
        // we should cache deferred execution
        executed1.store(false, Ordering::SeqCst);
        assert_eq!(*deferred_result, 5);
        assert!(!executed1.load(Ordering::SeqCst));

        Ok(())
    }

    #[tokio::test]
    async fn lookup_deferred_that_has_deferreds() -> anyhow::Result<()> {
        struct DeferringDeferred(usize, IndexSet<DeferredInput>, Arc<AtomicBool>);

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
            TargetLabel::testing_parse("cell//pkg:foo").configure(Configuration::testing_new());
        let analysis_key = AnalysisKey(target.dupe());
        let configured_node_key = ConfiguredNodeKey(target.dupe());

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

        let fs = ProjectFilesystemTemp::new()?;
        let dice = DiceBuilder::new()
            .set_data(|data| data.set_testing_io_provider(&fs))
            .mock_and_return(
                analysis_key,
                anyhow::Ok(MaybeCompatible::Compatible(AnalysisResult::new(
                    provider_collection,
                    deferred_result,
                )))
                .shared_error(),
            )
            .mock_and_return(
                configured_node_key,
                Ok(MaybeCompatible::Compatible(
                    ConfiguredTargetNode::testing_new(
                        target.dupe(),
                        RuleType::Starlark(Arc::new(StarlarkRuleType {
                            import_path: ImportPath::unchecked_new("cell", "pkg", "rules.bzl"),
                            name: "foo_lib".to_owned(),
                        })),
                        Vec::new(),
                        ExecutionPlatformResolution::new(None, Vec::new()),
                    ),
                )),
            );

        let mut dice_data = UserComputationData::new();
        set_fallback_executor_config(&mut dice_data.data, ActionExecutorConfig::testing_local());

        let dice = dice.build(dice_data);
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
