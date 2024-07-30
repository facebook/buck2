/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::deferred::key::DeferredHolderKey;
use buck2_artifact::dynamic::DynamicLambdaResultsKey;
use buck2_build_api::deferred::calculation::lookup_deferred_holder;
use buck2_build_api::dynamic::calculation::DynamicLambdaCalculation;
use buck2_build_api::dynamic::calculation::DynamicLambdaResult;
use buck2_build_api::dynamic::calculation::DYNAMIC_LAMBDA_CALCULATION_IMPL;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;

use crate::dynamic::deferred::prepare_and_execute_lambda;

struct DynamicLambdaCalculationImpl;

#[async_trait]
impl DynamicLambdaCalculation for DynamicLambdaCalculationImpl {
    async fn compute_dynamic_lambda(
        &self,
        dice: &mut DiceComputations<'_>,
        key: &DynamicLambdaResultsKey,
    ) -> anyhow::Result<Arc<DynamicLambdaResult>> {
        #[derive(
            Debug,
            derive_more::Display,
            Dupe,
            Clone,
            Allocative,
            Hash,
            Eq,
            PartialEq
        )]
        struct DynamicLambdaDiceKey(DynamicLambdaResultsKey);

        #[async_trait]
        impl Key for DynamicLambdaDiceKey {
            type Value = buck2_error::Result<Arc<DynamicLambdaResult>>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                cancellation: &CancellationContext,
            ) -> Self::Value {
                let lambda = lookup_deferred_holder(ctx, self.0.holder_key())
                    .await?
                    .lookup_lambda(&self.0)?;

                let analysis_values = prepare_and_execute_lambda(
                    ctx,
                    cancellation,
                    &lambda,
                    DeferredHolderKey::DynamicLambda(Arc::new(self.0.dupe())),
                    self.0.action_key(),
                )
                .await?;
                Ok(Arc::new(DynamicLambdaResult { analysis_values }))
            }

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                false
            }

            fn validity(x: &Self::Value) -> bool {
                x.is_ok()
            }
        }

        Ok(dice.compute(&DynamicLambdaDiceKey(key.dupe())).await??)
    }
}

pub(crate) fn init_dynamic_lambda_calculation() {
    DYNAMIC_LAMBDA_CALCULATION_IMPL.init(&DynamicLambdaCalculationImpl)
}
