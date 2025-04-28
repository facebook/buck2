/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_futures::cancellation::CancellationObserver;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;

use crate::analysis::AnalysisResult;
use crate::anon_target::AnonTargetDependentAnalysisResults;
use crate::anon_target::AnonTargetDyn;

pub static EVAL_BXL_FOR_ANON_TARGET: LateBinding<
    for<'v> fn(
        dice: &'v mut DiceComputations,
        anon_target: Arc<dyn AnonTargetDyn>,
        global_cfg_options: GlobalCfgOptions,
        dependents_analyses: AnonTargetDependentAnalysisResults<'v>,
        execution_platform: ExecutionPlatformResolution,
        liveness: CancellationObserver,
    ) -> Pin<Box<dyn Future<Output = buck2_error::Result<AnalysisResult>> + 'v>>,
> = LateBinding::new("EVAL_BXL_FOR_ANON_TARGET");
