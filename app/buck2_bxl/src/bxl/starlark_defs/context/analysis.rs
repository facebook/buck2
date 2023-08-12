/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_core::configuration::compatibility::IncompatiblePlatformReason;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use dice::DiceComputations;
use either::Either;
use gazebo::prelude::*;

use super::BxlContext;
use crate::bxl::starlark_defs::analysis_result::StarlarkAnalysisResult;
use crate::bxl::starlark_defs::providers_expr::ProvidersExpr;

pub(crate) async fn analysis<'v>(
    dice: &DiceComputations,
    ctx: &BxlContext<'v>,
    expr: ProvidersExpr<ConfiguredProvidersLabel>,
    skip_incompatible: bool,
) -> anyhow::Result<
    Either<Option<StarlarkAnalysisResult>, Vec<(ConfiguredProvidersLabel, StarlarkAnalysisResult)>>,
> {
    let analysis = futures::future::join_all(expr.labels().map(async move |label| {
        let maybe_result = dice.get_analysis_result(label.target()).await?;

        match maybe_result {
            MaybeCompatible::Incompatible(reason) => {
                if skip_incompatible {
                    ctx.print_to_error_stream(IncompatiblePlatformReason::skipping_message(
                        &reason,
                        label.target(),
                    ))?;
                    Ok(None)
                } else {
                    Err(reason.to_err())
                }
            }
            MaybeCompatible::Compatible(result) => Ok(Some((
                label.clone(),
                StarlarkAnalysisResult::new(result, label.clone()),
            ))),
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
            if analysis.is_empty() {
                // If empty, then it is because we skipped an incompatible target
                Ok(Either::Left(None))
            } else {
                Ok(Either::Left(Some(
                    analysis.into_iter().into_singleton().unwrap().1,
                )))
            }
        }
        ProvidersExpr::Iterable(_) => Ok(Either::Right(analysis)),
    }
}
