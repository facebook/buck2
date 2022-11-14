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
use buck2_build_api::bxl::result::BxlResult;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_execute::bxl::types::BxlKey;
use buck2_interpreter::dice::starlark_profiler::GetStarlarkProfilerInstrumentation;
use dice::DiceComputations;
use dice::Key;
use gazebo::dupe::Dupe;

use crate::bxl::eval::eval;

pub struct BxlCalculationImpl;

#[async_trait]
impl BxlCalculationDyn for BxlCalculationImpl {
    async fn eval_bxl(&self, ctx: &DiceComputations, bxl: BxlKey) -> SharedResult<Arc<BxlResult>> {
        ctx.compute(&internal::BxlComputeKey(bxl)).await?
    }
}

#[async_trait]
impl Key for internal::BxlComputeKey {
    type Value = SharedResult<Arc<BxlResult>>;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        let key = self.0.dupe();
        ctx.temporary_spawn(async move |ctx| {
            let profiler = ctx.get_profile_mode_for_intermediate_analysis().await?;
            eval(ctx, key, profiler)
                .await
                .shared_error()
                .map(|(result, _)| Arc::new(result))
        })
        .await
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }
}

mod internal {
    use allocative::Allocative;
    use buck2_execute::bxl::types::BxlKey;
    use derive_more::Display;
    use gazebo::prelude::*;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
    pub struct BxlComputeKey(pub BxlKey);
}

#[cfg(test)]
pub mod testing {
    pub use crate::bxl::calculation::internal::BxlComputeKey;
}
