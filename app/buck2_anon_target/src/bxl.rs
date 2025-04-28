/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::anon_target::AnonTargetDependentAnalysisResults;
use buck2_build_api::anon_target::AnonTargetDyn;
use buck2_build_api::bxl::anon_target::EVAL_BXL_FOR_ANON_TARGET;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_futures::cancellation::CancellationObserver;
use dice::DiceComputations;

pub(crate) async fn eval_bxl_for_anon_target(
    dice: &mut DiceComputations<'_>,
    anon_target: Arc<dyn AnonTargetDyn>,
    global_cfg_options: GlobalCfgOptions,
    dependents_analyses: AnonTargetDependentAnalysisResults<'_>,
    execution_platform: ExecutionPlatformResolution,
    liveness: CancellationObserver,
) -> buck2_error::Result<AnalysisResult> {
    (EVAL_BXL_FOR_ANON_TARGET.get()?)(
        dice,
        anon_target,
        global_cfg_options,
        dependents_analyses,
        execution_platform,
        liveness,
    )
    .await
}
