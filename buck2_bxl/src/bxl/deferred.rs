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
    use std::sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    };

    use buck2_build_api::{
        bxl::{calculation::BxlCalculationDyn, result::BxlResult, BxlFunctionLabel, BxlKey},
        calculation::Calculation,
        deferred::{
            BaseDeferredKey, BaseKey, Deferred, DeferredCtx, DeferredInput, DeferredRegistry,
            DeferredTable, DeferredValue,
        },
        execute::{commands::dice_data::set_fallback_executor_config, CommandExecutorConfig},
        path::BuckOutPath,
    };
    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_core::{
        fs::{paths::ForwardRelativePathBuf, project::ProjectFilesystemTemp},
        result::ToSharedResultExt,
    };
    use buck2_interpreter::common::BxlFilePath;
    use dice::{testing::DiceBuilder, UserComputationData};
    use gazebo::dupe::Dupe;
    use indexmap::IndexSet;
    use starlark::collections::SmallMap;

    use crate::bxl::calculation::{testing::BxlComputeKey, BxlCalculationImpl};

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
            .set_data(|data| data.set::<&dyn BxlCalculationDyn>(&BxlCalculationImpl))
            .mock_and_return(
                BxlComputeKey(bxl.dupe()),
                anyhow::Ok(Arc::new(BxlResult::BuildsArtifacts {
                    output_loc: BuckOutPath::new(
                        BaseDeferredKey::BxlLabel(bxl.dupe()),
                        ForwardRelativePathBuf::unchecked_new("test".to_owned()),
                    ),
                    built: vec![],
                    artifacts: vec![],
                    deferred: deferred_result,
                }))
                .shared_error(),
            );

        let mut dice_data = UserComputationData::new();
        set_fallback_executor_config(&mut dice_data.data, CommandExecutorConfig::testing_local());

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
}
