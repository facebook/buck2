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
use buck2_core::deferred::dynamic::DynamicLambdaResultsKey;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

use crate::analysis::registry::RecordedAnalysisValues;

#[derive(Allocative)]
pub struct DynamicLambdaResult {
    pub analysis_values: RecordedAnalysisValues,
}

impl DynamicLambdaResult {
    pub(crate) fn analysis_values(&self) -> &crate::analysis::registry::RecordedAnalysisValues {
        &self.analysis_values
    }
}

#[async_trait]
pub trait DynamicLambdaCalculation: Sync + 'static {
    async fn dynamic_lambda_result(
        &self,
        dice: &mut DiceComputations<'_>,
        key: &DynamicLambdaResultsKey,
    ) -> buck2_error::Result<Arc<DynamicLambdaResult>>;
}

pub static DYNAMIC_LAMBDA_CALCULATION_IMPL: LateBinding<&'static dyn DynamicLambdaCalculation> =
    LateBinding::new("DYNAMIC_LAMBDA_CALCULATION_IMPL");

pub async fn dynamic_lambda_result(
    dice: &mut DiceComputations<'_>,
    key: &DynamicLambdaResultsKey,
) -> buck2_error::Result<Arc<DynamicLambdaResult>> {
    DYNAMIC_LAMBDA_CALCULATION_IMPL
        .get()?
        .dynamic_lambda_result(dice, key)
        .await
}
