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
use buck2_build_api::interpreter::rule_defs::provider::dependency::Dependency;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::starlark_value;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
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
#[starlark_docs(directory = "bxl")]
pub(crate) struct StarlarkAnalysisResult {
    analysis: AnalysisResult,
    label: ConfiguredProvidersLabel,
}

impl StarlarkAnalysisResult {
    pub(crate) fn new(analysis: AnalysisResult, label: ConfiguredProvidersLabel) -> Self {
        Self { analysis, label }
    }
}

starlark_simple_value!(StarlarkAnalysisResult);

#[starlark_value(type = "analysis_result")]
impl<'v> StarlarkValue<'v> for StarlarkAnalysisResult {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_analysis_result_methods)
    }
}

/// The result of running an analysis in bxl.
#[starlark_module]
fn starlark_analysis_result_methods(builder: &mut MethodsBuilder) {
    /// Access the providers of the rule. Returns a `[ProviderCollection]` the same as accessing
    /// providers of dependencies within a rule implementation.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_providers(ctx):
    ///     node = ctx.configured_targets("root//bin:the_binary")
    ///     providers = ctx.analysis(node).providers()
    ///     ctx.output.print(providers[FooInfo])
    ///     providers = ctx.analysis("//:bin").providers()
    ///     ctx.output.print(providers[FooInfo])
    /// ```
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

    /// Converts the analysis result into a `dependency`. Currently, you can only get a `dependency` without any
    /// transitions. This means that you cannot create an exec dep or toolchain from an analysis result.

    /// We may support other dependency transition types in the future.
    ///

    /// This is useful for passing in the results of `ctx.analysis()` into anon targets.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_dependency(ctx):
    ///     node = ctx.configured_targets("root//bin:the_binary")
    ///     dependency = ctx.analysis(node).as_dependency()
    /// ```
    fn as_dependency<'v>(
        this: &'v StarlarkAnalysisResult,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        unsafe {
            // SAFETY:: this actually just returns a FrozenValue from in the StarlarkAnalysisResult
            // which is kept alive for 'v
            Ok(heap.alloc(Dependency::new(
                heap,
                this.label.clone(),
                this.analysis
                    .lookup_inner(&this.label)?
                    .value()
                    .to_frozen_value()
                    .to_value(),
                None,
            )))
        }
    }
}
