/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_build_api::analysis::AnalysisResult;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::FrozenValue;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::StarlarkDocs;

#[derive(
    ProvidesStaticType,
    Debug,
    Display,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[display(fmt = "{:?}", self)]
#[starlark_docs_attrs(directory = "bxl")]
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
    starlark_type!("analysis_result");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_analysis_result_methods)
    }
}

#[starlark_module]
fn starlark_analysis_result_methods(builder: &mut MethodsBuilder) {
    /// Access the providers of the rule. Returns a 'ProviderCollection' the same as accessing
    /// providers of dependencies within a rule implementation.
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
