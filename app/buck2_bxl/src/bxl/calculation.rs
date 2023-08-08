/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_build_api::bxl::calculation::BxlCalculationDyn;
use buck2_build_api::bxl::calculation::BxlComputeResult;
use buck2_build_api::bxl::calculation::BXL_CALCULATION_IMPL;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_common::result::ToUnsharedResultExt;
use buck2_core::base_deferred_key::BaseDeferredKeyDyn;
use buck2_interpreter::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::future::FutureExt;
use more_futures::cancellation::CancellationContext;

use crate::bxl::eval::eval;
use crate::bxl::key::BxlKey;

#[derive(Debug)]
struct BxlCalculationImpl;

#[async_trait]
impl BxlCalculationDyn for BxlCalculationImpl {
    async fn eval_bxl(
        &self,
        ctx: &DiceComputations,
        bxl: Arc<dyn BaseDeferredKeyDyn>,
    ) -> anyhow::Result<BxlComputeResult> {
        eval_bxl(ctx, BxlKey::from_base_deferred_key_dyn_impl_err(bxl)?).await
    }
}

pub(crate) fn init_bxl_calculation_impl() {
    BXL_CALCULATION_IMPL.init(&BxlCalculationImpl);
}

pub(crate) async fn eval_bxl(
    ctx: &DiceComputations,
    bxl: BxlKey,
) -> anyhow::Result<BxlComputeResult> {
    ctx.compute(&internal::BxlComputeKey(bxl))
        .await?
        .unshared_error()
}

#[async_trait]
impl Key for internal::BxlComputeKey {
    type Value = SharedResult<BxlComputeResult>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        cancellation: &CancellationContext,
    ) -> Self::Value {
        let key = self.0.dupe();

        let profiler = ctx.get_profile_mode_for_intermediate_analysis().await?;

        cancellation
            .with_structured_cancellation(|observer| {
                async move {
                    eval(ctx, key, profiler, observer).await.shared_error().map(
                        |(result, _, materializations)| BxlComputeResult {
                            bxl_result: Arc::new(result),
                            materializations,
                        },
                    )
                }
                .boxed()
            })
            .await
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }
}

mod internal {
    use allocative::Allocative;
    use derive_more::Display;
    use dupe::Dupe;

    use crate::bxl::key::BxlKey;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    pub(crate) struct BxlComputeKey(pub(crate) BxlKey);
}

#[cfg(test)]
pub(crate) mod testing {
    pub(crate) use crate::bxl::calculation::internal::BxlComputeKey;
}
