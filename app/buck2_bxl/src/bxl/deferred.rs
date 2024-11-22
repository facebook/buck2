/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(test)]
mod tests {

    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;

    use allocative::Allocative;
    use buck2_artifact::deferred::key::DeferredHolderKey;
    use buck2_build_api::actions::execute::dice_data::set_fallback_executor_config;
    use buck2_build_api::bxl::calculation::BxlComputeResult;
    use buck2_build_api::bxl::result::BxlResult;
    use buck2_build_api::bxl::types::BxlFunctionLabel;
    use buck2_build_api::deferred::calculation::DeferredCalculation;
    use buck2_build_api::deferred::types::Deferred;
    use buck2_build_api::deferred::types::DeferredCtx;
    use buck2_build_api::deferred::types::DeferredInput;
    use buck2_build_api::deferred::types::DeferredInputsRef;
    use buck2_build_api::deferred::types::DeferredOutput;
    use buck2_build_api::deferred::types::DeferredRegistry;
    use buck2_build_api::deferred::types::DeferredValue;
    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_core::base_deferred_key::BaseDeferredKey;
    use buck2_core::bxl::BxlFilePath;
    use buck2_core::execution_types::execution::ExecutionPlatformResolution;
    use buck2_core::execution_types::executor_config::CommandExecutorConfig;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::global_cfg_options::GlobalCfgOptions;
    use buck2_execute::digest_config::DigestConfig;
    use buck2_execute::digest_config::SetDigestConfig;
    use dice::testing::DiceBuilder;
    use dice::DiceComputations;
    use dice::UserComputationData;
    use dupe::Dupe;
    use indexmap::IndexSet;
    use starlark_map::ordered_map::OrderedMap;

    use crate::bxl::calculation::testing::BxlComputeKey;
    use crate::bxl::eval::mk_stream_cache;
    use crate::bxl::key::BxlKey;

    #[derive(Allocative, Clone, Debug, Eq, PartialEq)]
    struct FakeDeferredOutput(usize);

    impl DeferredOutput for FakeDeferredOutput {}

    #[derive(Debug, Allocative)]
    struct FakeDeferred(usize, IndexSet<DeferredInput>, Arc<AtomicBool>);

    impl provider::Provider for FakeDeferred {
        fn provide<'a>(&'a self, _demand: &mut provider::Demand<'a>) {}
    }

    impl Deferred for FakeDeferred {
        type Output = FakeDeferredOutput;

        fn inputs(&self) -> DeferredInputsRef<'_> {
            DeferredInputsRef::IndexSet(&self.1)
        }

        async fn execute(
            &self,
            _ctx: &mut dyn DeferredCtx,
            _dice: &mut DiceComputations<'_>,
        ) -> buck2_error::Result<DeferredValue<Self::Output>> {
            self.2.store(true, Ordering::SeqCst);
            Ok(DeferredValue::Ready(FakeDeferredOutput(self.0)))
        }
    }

    #[tokio::test]
    async fn lookup_deferred_from_bxl() -> buck2_error::Result<()> {
        let bxl = BxlKey::new(
            BxlFunctionLabel {
                bxl_path: BxlFilePath::testing_new("cell", "dir"),
                name: "foo".to_owned(),
            },
            Arc::new(OrderedMap::new()),
            false,
            GlobalCfgOptions::default(),
        );

        let mut deferred = DeferredRegistry::new(DeferredHolderKey::Base(
            BaseDeferredKey::BxlLabel(bxl.dupe().into_base_deferred_key_dyn_impl(
                ExecutionPlatformResolution::unspecified(),
                Vec::new(),
                Vec::new(),
            )),
        ));

        let executed0 = Arc::new(AtomicBool::new(false));
        let executed1 = Arc::new(AtomicBool::new(false));
        let data0 = deferred.defer(FakeDeferred(1, IndexSet::new(), executed0.dupe()));
        let data1 = deferred.defer(FakeDeferred(5, IndexSet::new(), executed1.dupe()));
        let (deferred_result, analysis_values) = deferred.take_result()?;

        let fs = ProjectRootTemp::new()?;
        let dice = DiceBuilder::new()
            .set_data(|data| {
                data.set_testing_io_provider(&fs);
                data.set_digest_config(DigestConfig::testing_default());
            })
            .mock_and_return(
                BxlComputeKey(bxl.dupe()),
                buck2_error::Ok(BxlComputeResult(Arc::new(BxlResult::BuildsArtifacts {
                    output_loc: mk_stream_cache("test", &bxl),
                    error_loc: mk_stream_cache("errortest", &bxl),
                    built: vec![],
                    artifacts: vec![],
                    deferred: deferred_result,
                    analysis_values,
                }))),
            );

        let mut dice_data = UserComputationData::new();
        set_fallback_executor_config(&mut dice_data.data, CommandExecutorConfig::testing_local());

        let mut dice = dice.build(dice_data)?.commit().await;
        let deferred_result = dice.compute_deferred_data(&data0).await?;
        assert_eq!(deferred_result.0, 1);
        assert!(executed0.load(Ordering::SeqCst));
        // we should cache deferred execution
        executed0.store(false, Ordering::SeqCst);
        let deferred_result = dice.compute_deferred_data(&data0).await?;
        assert_eq!(deferred_result.0, 1);
        assert!(!executed0.load(Ordering::SeqCst));

        let deferred_result = dice.compute_deferred_data(&data1).await?;
        assert_eq!(deferred_result.0, 5);
        assert!(executed1.load(Ordering::SeqCst));
        // we should cache deferred execution
        executed1.store(false, Ordering::SeqCst);
        assert_eq!(deferred_result.0, 5);
        assert!(!executed1.load(Ordering::SeqCst));

        Ok(())
    }
}
