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
use buck2_build_api::deferred::calculation::lookup_deferred_holder;
use buck2_build_api::dynamic::calculation::DynamicLambdaCalculation;
use buck2_build_api::dynamic::calculation::DynamicLambdaResult;
use buck2_build_api::dynamic::calculation::DYNAMIC_LAMBDA_CALCULATION_IMPL;
use buck2_build_signals::node_key::BuildSignalsNodeKey;
use buck2_build_signals::node_key::BuildSignalsNodeKeyImpl;
use buck2_core::deferred::dynamic::DynamicLambdaResultsKey;
use dice::CancellationContext;
use dice::Demand;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;

use crate::dynamic::deferred::prepare_and_execute_lambda;
use crate::dynamic::storage::FrozenDynamicLambdaParamsStorageImpl;

struct DynamicLambdaCalculationImpl;

#[async_trait]
impl DynamicLambdaCalculation for DynamicLambdaCalculationImpl {
    async fn dynamic_lambda_result(
        &self,
        dice: &mut DiceComputations<'_>,
        key: &DynamicLambdaResultsKey,
    ) -> buck2_error::Result<Arc<DynamicLambdaResult>> {
        Ok(dice.compute(&DynamicLambdaDiceKey(key.dupe())).await??)
    }
}

pub(crate) fn init_dynamic_lambda_calculation() {
    DYNAMIC_LAMBDA_CALCULATION_IMPL.init(&DynamicLambdaCalculationImpl)
}

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
pub struct DynamicLambdaDiceKey(DynamicLambdaResultsKey);

#[async_trait]
impl Key for DynamicLambdaDiceKey {
    type Value = buck2_error::Result<Arc<DynamicLambdaResult>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        cancellation: &CancellationContext,
    ) -> Self::Value {
        let deferred_holder = lookup_deferred_holder(ctx, self.0.holder_key()).await?;
        let lambda = FrozenDynamicLambdaParamsStorageImpl::lookup_lambda(
            deferred_holder.analysis_values().analysis_storage()?,
            &self.0,
        )?;

        let analysis_values =
            prepare_and_execute_lambda(ctx, cancellation, lambda, self.0.dupe()).await?;
        Ok(Arc::new(DynamicLambdaResult { analysis_values }))
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        false
    }

    fn validity(x: &Self::Value) -> bool {
        x.is_ok()
    }

    fn provide<'a>(&'a self, demand: &mut Demand<'a>) {
        demand.provide_value_with(|| BuildSignalsNodeKey::new(self.dupe()));
    }
}

impl BuildSignalsNodeKeyImpl for DynamicLambdaDiceKey {}
