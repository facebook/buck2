use buck2_core::provider::ConfiguredProvidersLabel;
use derive_more::Display;
use gazebo::any::AnyLifetime;
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{FrozenValue, NoSerialize, StarlarkValue},
};

use crate::analysis::AnalysisResult;

#[derive(AnyLifetime, Debug, Display, NoSerialize)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkAnalysisResult {
    analysis: AnalysisResult,
    label: ConfiguredProvidersLabel,
}

impl StarlarkAnalysisResult {
    pub(crate) fn new(analysis: AnalysisResult, label: ConfiguredProvidersLabel) -> Self {
        Self { analysis, label }
    }
}

starlark_simple_value!(StarlarkAnalysisResult);

impl<'v> StarlarkValue<'v> for StarlarkAnalysisResult {
    starlark_type!("analysis result");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_analysis_result_methods)
    }
}

#[starlark_module]
fn starlark_analysis_result_methods(builder: &mut MethodsBuilder) {
    fn providers<'v>(this: &'v StarlarkAnalysisResult) -> anyhow::Result<FrozenValue> {
        unsafe {
            // SAFETY:: this actually just returns a FrozenValue from in the StarlarkAnalysisResult
            // which is kept alive for 'v
            Ok(this
                .analysis
                .lookup_inner(&this.label)?
                .value()
                .to_frozen_value())
        }
    }
}
