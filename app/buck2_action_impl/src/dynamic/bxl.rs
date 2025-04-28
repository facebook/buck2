/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::pin::Pin;

use buck2_build_api::analysis::registry::RecordedAnalysisValues;
use buck2_build_api::dynamic_value::DynamicValue;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_core::deferred::base_deferred_key::BaseDeferredKeyBxl;
use buck2_core::deferred::dynamic::DynamicLambdaResultsKey;
use buck2_execute::digest_config::DigestConfig;
use buck2_futures::cancellation::CancellationObserver;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use futures::Future;
use starlark::values::OwnedRefFrozenRef;

use crate::dynamic::deferred::InputArtifactsMaterialized;
use crate::dynamic::params::FrozenDynamicLambdaParams;

pub static EVAL_BXL_FOR_DYNAMIC_OUTPUT: LateBinding<
    for<'v> fn(
        &'v BaseDeferredKeyBxl,
        DynamicLambdaResultsKey,
        OwnedRefFrozenRef<'v, FrozenDynamicLambdaParams>,
        &'v mut DiceComputations,
        InputArtifactsMaterialized,
        HashMap<DynamicValue, FrozenProviderCollectionValue>,
        DigestConfig,
        CancellationObserver,
    ) -> Pin<
        Box<dyn Future<Output = buck2_error::Result<RecordedAnalysisValues>> + Send + 'v>,
    >,
> = LateBinding::new("EVAL_BXL_FOR_DYNAMIC_OUTPUT");

pub(crate) async fn eval_bxl_for_dynamic_output<'v>(
    base_deferred_key: &'v BaseDeferredKeyBxl,
    self_key: DynamicLambdaResultsKey,
    dynamic_lambda: OwnedRefFrozenRef<'_, FrozenDynamicLambdaParams>,
    dice_ctx: &'v mut DiceComputations<'_>,
    input_artifacts_materialized: InputArtifactsMaterialized,
    resolved_dynamic_values: HashMap<DynamicValue, FrozenProviderCollectionValue>,
    digest_config: DigestConfig,
    liveness: CancellationObserver,
) -> buck2_error::Result<RecordedAnalysisValues> {
    (EVAL_BXL_FOR_DYNAMIC_OUTPUT.get()?)(
        base_deferred_key,
        self_key,
        dynamic_lambda,
        dice_ctx,
        input_artifacts_materialized,
        resolved_dynamic_values,
        digest_config,
        liveness,
    )
    .await
}
