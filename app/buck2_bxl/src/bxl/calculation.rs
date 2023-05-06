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
use buck2_build_api::bxl::types::BxlKey;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_common::result::ToUnsharedResultExt;
use buck2_interpreter::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use ctor::ctor;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::future::FutureExt;
use more_futures::cancellation::CancellationContext;

use crate::bxl::eval::eval;

#[derive(Debug)]
struct BxlCalculationImpl;

#[async_trait]
impl BxlCalculationDyn for BxlCalculationImpl {
    async fn eval_bxl(
        &self,
        ctx: &DiceComputations,
        bxl: BxlKey,
    ) -> anyhow::Result<BxlComputeResult> {
        ctx.compute(&internal::BxlComputeKey(bxl))
            .await?
            .unshared_error()
    }
}

#[ctor]
fn set_bxl_calculation_impl() {
    BXL_CALCULATION_IMPL.init(&BxlCalculationImpl);
}

#[async_trait]
impl Key for internal::BxlComputeKey {
    type Value = SharedResult<BxlComputeResult>;

    async fn compute(
        &self,
        ctx: &DiceComputations,
        _cancellation: &CancellationContext,
    ) -> Self::Value {
        let key = self.0.dupe();
        let future_and_cancellation = ctx.temporary_spawn(move |ctx, cancellation| {
            async move {
                {
                    let profiler = ctx.get_profile_mode_for_intermediate_analysis().await?;
                    eval(ctx, key, profiler, cancellation)
                        .await
                        .shared_error()
                        .map(|(result, _, materializations)| BxlComputeResult {
                            bxl_result: Arc::new(result),
                            materializations,
                        })
                }
            }
            .boxed()
        });

        // TODO(bobyf) can use cancellation context to interact with the temporary spawn cancellation better
        future_and_cancellation.await
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }
}

mod internal {
    use allocative::Allocative;
    use buck2_build_api::bxl::types::BxlKey;
    use derive_more::Display;
    use dupe::Dupe;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    pub struct BxlComputeKey(pub BxlKey);
}

#[cfg(test)]
pub mod testing {
    pub use crate::bxl::calculation::internal::BxlComputeKey;
}
