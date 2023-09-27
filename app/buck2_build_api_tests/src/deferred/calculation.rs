/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_analysis::analysis::calculation::AnalysisKey;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::deferred::calculation::DeferredCalculation;
use buck2_build_api::deferred::types::BaseKey;
use buck2_build_api::deferred::types::Deferred;
use buck2_build_api::deferred::types::DeferredCtx;
use buck2_build_api::deferred::types::DeferredInput;
use buck2_build_api::deferred::types::DeferredRegistry;
use buck2_build_api::deferred::types::DeferredTable;
use buck2_build_api::deferred::types::DeferredValue;
use buck2_common::dice::data::testing::SetTestingIoProvider;
use buck2_common::result::ToSharedResultExt;
use buck2_configured::nodes::calculation::ConfiguredTargetNodeKey;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::target::label::TargetLabel;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::SetDigestConfig;
use buck2_execute::execute::dice_data::set_fallback_executor_config;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dice::testing::DiceBuilder;
use dice::DiceComputations;
use dice::UserComputationData;
use dupe::Dupe;
use indexmap::IndexSet;
use indoc::indoc;

use crate::interpreter::rule_defs::provider::testing::FrozenProviderCollectionValueExt;

#[derive(Allocative)]
struct FakeDeferred(usize, IndexSet<DeferredInput>, Arc<AtomicBool>);

impl provider::Provider for FakeDeferred {
    fn provide<'a>(&'a self, _demand: &mut provider::Demand<'a>) {}
}

#[async_trait]
impl Deferred for FakeDeferred {
    type Output = usize;

    fn inputs(&self) -> &IndexSet<DeferredInput> {
        &self.1
    }

    async fn execute(
        &self,
        _ctx: &mut dyn DeferredCtx,
        _dice: &mut DiceComputations,
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

    impl provider::Provider for DeferringDeferred {
        fn provide<'a>(&'a self, _demand: &mut provider::Demand<'a>) {}
    }

    #[async_trait]
    impl Deferred for DeferringDeferred {
        type Output = usize;

        fn inputs(&self) -> &IndexSet<DeferredInput> {
            &self.1
        }

        async fn execute(
            &self,
            ctx: &mut dyn DeferredCtx,
            _dice: &mut DiceComputations,
        ) -> anyhow::Result<DeferredValue<Self::Output>> {
            let data = ctx
                .registry()
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
