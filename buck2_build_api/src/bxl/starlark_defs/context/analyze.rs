use buck2_core::provider::ConfiguredProvidersLabel;
use dice::DiceComputations;

use crate::{
    analysis::calculation::RuleAnalysisCalculation,
    bxl::starlark_defs::analysis_result::StarlarkAnalysisResult,
};

pub(crate) async fn analysis(
    ctx: &DiceComputations,
    labels: impl Iterator<Item = &ConfiguredProvidersLabel>,
    skip_incompatible: bool,
) -> anyhow::Result<Vec<StarlarkAnalysisResult>> {
    futures::future::join_all(labels.map(async move |label| {
        let maybe_result = ctx
            .get_analysis_result(label.target())
            .await?
            .require_compatible();

        if skip_incompatible && maybe_result.is_err() {
            Ok(None)
        } else {
            Ok(Some(StarlarkAnalysisResult::new(
                maybe_result?,
                label.clone(),
            )))
        }
    }))
    .await
    .into_iter()
    .filter_map(|r| match r {
        Ok(r) => r.map(Ok),
        Err(e) => Some(Err(e)),
    })
    .collect::<anyhow::Result<_>>()
}
