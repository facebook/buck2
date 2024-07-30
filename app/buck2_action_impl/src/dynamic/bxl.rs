/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::pin::Pin;
use std::sync::Arc;

use buck2_build_api::deferred::types::DeferredCtx;
use buck2_build_api::dynamic::lambda::DynamicLambda;
use buck2_core::base_deferred_key::BaseDeferredKeyDyn;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use futures::Future;

pub static EVAL_BXL_FOR_DYNAMIC_OUTPUT: LateBinding<
    for<'v> fn(
        &'v Arc<dyn BaseDeferredKeyDyn>,
        &'v DynamicLambda,
        &'v mut dyn DeferredCtx,
        &'v mut DiceComputations,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<()>> + Send + 'v>>,
> = LateBinding::new("EVAL_BXL_FOR_DYNAMIC_OUTPUT");

pub(crate) async fn eval_bxl_for_dynamic_output<'v>(
    base_deferred_key: &'v Arc<dyn BaseDeferredKeyDyn>,
    dynamic_lambda: &'v DynamicLambda,
    deferred_ctx: &'v mut dyn DeferredCtx,
    dice_ctx: &'v mut DiceComputations<'_>,
) -> anyhow::Result<()> {
    (EVAL_BXL_FOR_DYNAMIC_OUTPUT.get()?)(base_deferred_key, dynamic_lambda, deferred_ctx, dice_ctx)
        .await
}
