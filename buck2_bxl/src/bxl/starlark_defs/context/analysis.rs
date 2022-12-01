/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use dice::DiceComputations;
use either::Either;
use gazebo::prelude::*;

use crate::bxl::starlark_defs::analysis_result::StarlarkAnalysisResult;
use crate::bxl::starlark_defs::providers_expr::ProvidersExpr;

pub(crate) async fn analysis(
    ctx: &DiceComputations,
    expr: ProvidersExpr,
    skip_incompatible: bool,
) -> anyhow::Result<
    Either<StarlarkAnalysisResult, Vec<(ConfiguredProvidersLabel, StarlarkAnalysisResult)>>,
> {
    let analysis = futures::future::join_all(expr.labels().map(async move |label| {
        let maybe_result = ctx
            .get_analysis_result(label.target())
            .await?
            .require_compatible();

        if skip_incompatible && maybe_result.is_err() {
            Ok(None)
        } else {
            Ok(Some((
                label.clone(),
                StarlarkAnalysisResult::new(maybe_result?, label.clone()),
            )))
        }
    }))
    .await
    .into_iter()
    .filter_map(|r| match r {
        Ok(r) => r.map(Ok),
        Err(e) => Some(Err(e)),
    })
    .collect::<anyhow::Result<Vec<_>>>()?;

    match expr {
        ProvidersExpr::Literal(_) => {
            if analysis.len() != 1 {
                Err(anyhow::anyhow!(
                    "Expected exactly 1 analysis result when requesting a single target"
                ))
            } else {
                Ok(Either::Left(
                    analysis.into_iter().into_singleton().unwrap().1,
                ))
            }
        }
        ProvidersExpr::Iterable(_) => Ok(Either::Right(analysis)),
    }
}
