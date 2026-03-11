/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;

use allocative::Allocative;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use buck2_build_api::interpreter::rule_defs::provider::dependency::Dependency;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::FrozenValueTyped;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueTyped;
use starlark::values::starlark_value;

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
        RES.methods_for_type::<Self::Canonical>(starlark_analysis_result_methods)
    }
}

/// The result of running an analysis in bxl.
#[starlark_module]
fn starlark_analysis_result_methods(builder: &mut MethodsBuilder) {
    /// Access the providers of the rule. Returns a `ProviderCollection` the same as accessing
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
    fn providers<'v>(
        this: &'v StarlarkAnalysisResult,
    ) -> starlark::Result<FrozenValueTyped<'v, FrozenProviderCollection>> {
        unsafe {
            // SAFETY: this actually just returns a FrozenValue from in the StarlarkAnalysisResult
            // which is kept alive for 'v
            Ok(this
                .analysis
                .lookup_inner(&this.label)?
                .value()
                .value_typed())
        }
    }

    /// Returns a list of structs describing each provider in the collection.
    /// Each struct has three fields:
    /// - `name`: The provider type name (e.g., `"DefaultInfo"`, `"FooInfo"`)
    /// - `path`: The cell path of the `.bzl` file where the provider was defined
    ///   (e.g., `"fbcode//pkg/defs.bzl"`), or `None` for built-in providers
    /// - `value`: The provider instance itself
    ///
    /// This enables BXL scripts to enumerate all providers on a target without
    /// knowing the provider types in advance.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_providers_info(ctx):
    ///     node = ctx.configured_targets("root//bin:the_binary")
    ///     result = ctx.analysis(node)
    ///     for info in result.providers_info():
    ///         ctx.output.print(info.name)
    ///         ctx.output.print(info.path)
    ///         ctx.output.print(info.value)
    /// ```
    fn providers_info<'v>(
        this: &'v StarlarkAnalysisResult,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
        let collection: FrozenValueTyped<'_, FrozenProviderCollection> = unsafe {
            this.analysis
                .lookup_inner(&this.label)?
                .value()
                .value_typed()
        };
        let heap = eval.heap();
        let mut result = Vec::new();
        for (id, value) in collection.as_ref().iter_providers() {
            let name = heap.alloc(id.name.as_str());
            let path = match &id.path {
                Some(p) => heap.alloc(p.to_string()),
                None => Value::new_none(),
            };
            let info = heap.alloc(starlark::values::structs::AllocStruct([
                ("name", name),
                ("path", path),
                ("value", value.to_value()),
            ]));
            result.push(info);
        }
        Ok(heap.alloc(result))
    }

    /// Gets the configured providers label for this analysis result.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_label(ctx):
    ///     actions = ctx.aquery().all_actions("//target")
    ///     for node in actions:
    ///         if analysis := node.analysis():
    ///             ctx.output.print(analysis.label())
    /// ```
    fn label<'v>(
        this: &StarlarkAnalysisResult,
    ) -> starlark::Result<StarlarkConfiguredProvidersLabel> {
        Ok(StarlarkConfiguredProvidersLabel::new(this.label.dupe()))
    }

    /// Converts the analysis result into a `Dependency`. Currently, you can only get a `Dependency` without any
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
