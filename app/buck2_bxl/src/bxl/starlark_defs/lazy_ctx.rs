/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::list::UnpackList;
use starlark::values::starlark_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::lazy_operation::StarlarkLazy;
use crate::bxl::starlark_defs::providers_expr::ConfiguredProvidersLabelArg;

/// Context for lazy/batch/error handling operations.
/// Available as `ctx.lazy`, has type `bxl.LazyContext`.
#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative,
    StarlarkDocs
)]
#[starlark_docs(directory = "bxl")]
#[derivative(Debug)]
#[display("bxl.LazyContext")]
pub(crate) struct StarlarkLazyCtx;

starlark_simple_value!(StarlarkLazyCtx);

#[starlark_value(type = "bxl.LazyContext")]
impl<'v> StarlarkValue<'v> for StarlarkLazyCtx {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(lazy_ctx_methods)
    }
}

#[starlark_module]
fn lazy_ctx_methods(builder: &mut MethodsBuilder) {
    /// Join a list of lazy operations into a single operation that can be evaluated.
    /// This is useful when you want to evaluate multiple operations in parallel.
    /// Using `.try_resolve()` can catch errors for the individual operations.
    ///
    /// Example:
    /// ```text
    /// def _impl(ctx):
    ///     ...
    ///     joined = ctx.lazy.join_all([ctx.lazy.analysis(t) for t in targets])
    ///     analysis_results = joined.resolve()
    ///     ctx.output.print(analysis_results)
    /// ```
    fn join_all<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyCtx,
        #[starlark(require = pos)] operations: UnpackList<&StarlarkLazy>,
    ) -> anyhow::Result<StarlarkLazy> {
        Ok(StarlarkLazy::new_batch(
            operations.into_iter().map(|o| o.dupe()),
        ))
    }

    /// Analyze a target lazily. This will return a lazy operation that can be evaluated later.
    /// The target should be a ConfiguredTargetLabel, a ConfiguredProvidersLabel, or a ConfiguredTargetNode.
    ///
    /// Example:
    /// ```text
    /// def _impl(ctx):
    ///     target = ctx.configured_targets("cell//path/to:target")
    ///     analysis_result = ctx.lazy.analysis(target).resolve()
    ///     (analysis_result, err) = ctx.lazy.analysis(target).try_resolve()
    /// ```
    fn analysis<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyCtx,
        #[starlark(require = pos)] label: ConfiguredProvidersLabelArg<'v>,
    ) -> anyhow::Result<StarlarkLazy> {
        let configured_providers_label = label.configured_providers_label();
        Ok(StarlarkLazy::new_analysis(configured_providers_label))
    }
}
