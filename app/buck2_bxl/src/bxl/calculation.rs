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
use buck2_core::deferred::base_deferred_key::BaseDeferredKeyBxl;
use buck2_futures::cancellation::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::future::FutureExt;

use crate::bxl::eval::eval;
use crate::bxl::key::BxlKey;

#[derive(Debug)]
struct BxlCalculationImpl;

#[async_trait]
impl BxlCalculationDyn for BxlCalculationImpl {
    async fn eval_bxl(
        &self,
        ctx: &mut DiceComputations<'_>,
        bxl: BaseDeferredKeyBxl,
    ) -> buck2_error::Result<BxlComputeResult> {
        eval_bxl(ctx, BxlKey::from_base_deferred_key_dyn_impl_err(bxl)?).await
    }
}

pub(crate) fn init_bxl_calculation_impl() {
    BXL_CALCULATION_IMPL.init(&BxlCalculationImpl);
}

pub(crate) async fn eval_bxl(
    ctx: &mut DiceComputations<'_>,
    bxl: BxlKey,
) -> buck2_error::Result<BxlComputeResult> {
    ctx.compute(&internal::BxlComputeKey(bxl))
        .await?
        .map_err(buck2_error::Error::from)
}

#[async_trait]
impl Key for internal::BxlComputeKey {
    type Value = buck2_error::Result<BxlComputeResult>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        cancellation: &CancellationContext,
    ) -> Self::Value {
        let key = self.0.dupe();

        cancellation
            .with_structured_cancellation(|observer| {
                async move {
                    eval(ctx, key, observer)
                        .await
                        .map_err(buck2_error::Error::from)
                        .map(|(result, _)| BxlComputeResult(Arc::new(result)))
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
