/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use allocative::Allocative;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::interpreter::rule_defs::provider::dependency::Dependency;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::starlark_value;
use starlark::values::FrozenValue;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::ValueTyped;

#[derive(ProvidesStaticType, Debug, NoSerialize, Allocative)]
pub(crate) struct StarlarkAnalysisResult {
    // Invariant: The subtarget specified on the label is present in the analysis result.
    analysis: AnalysisResult,
    label: ConfiguredProvidersLabel,
}

impl fmt::Display for StarlarkAnalysisResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "AnalysisResult(")?;
        fmt::Display::fmt(
            self.analysis
                .lookup_inner(&self.label)
                .unwrap()
                .provider_collection(),
            f,
        )?;
        write!(f, ")")
    }
}

impl StarlarkAnalysisResult {
    pub(crate) fn new(
        analysis: AnalysisResult,
        label: ConfiguredProvidersLabel,
    ) -> buck2_error::Result<Self> {
        // Check that the specified subtarget actually exists
        drop(analysis.lookup_inner(&label)?);
        Ok(Self { analysis, label })
    }
}

starlark_simple_value!(StarlarkAnalysisResult);

#[starlark_value(type = "bxl.AnalysisResult")]
impl<'v> StarlarkValue<'v> for StarlarkAnalysisResult {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_analysis_result_methods)
    }
}

/// The result of running an analysis in bxl.
#[starlark_module]
fn starlark_analysis_result_methods(builder: &mut MethodsBuilder) {
    /// Access the providers of the rule. Returns a `provider_collection` the same as accessing
    /// providers of dependencies within a rule implementation.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_providers(ctx):
    ///     node = ctx.configured_targets("root//bin:the_binary")
    ///     providers = ctx.analysis(node).providers()
    ///     ctx.output.print(providers[FooInfo])
    ///     providers = ctx.analysis("//:bin").providers()
    ///     ctx.output.print(providers[FooInfo])
    /// ```
    fn providers<'v>(this: &'v StarlarkAnalysisResult) -> starlark::Result<FrozenValue> {
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
    ///
    /// We may support other dependency transition types in the future.
    ///
    /// This is useful for passing in the results of `ctx.analysis()` into anon targets.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_dependency(ctx):
    ///     node = ctx.configured_targets("root//bin:the_binary")
    ///     dependency = ctx.analysis(node).as_dependency()
    /// ```
    fn as_dependency<'v>(
        this: &'v StarlarkAnalysisResult,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, Dependency<'v>>> {
        Ok(eval.heap().alloc_typed(Dependency::new(
            eval.heap(),
            this.label.dupe(),
            this.analysis
                .lookup_inner(&this.label)?
                .value()
                .owned_frozen_value_typed(eval.frozen_heap()),
            None,
        )))
    }
}
