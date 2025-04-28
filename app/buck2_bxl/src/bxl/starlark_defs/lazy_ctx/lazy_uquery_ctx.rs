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
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::starlark_value;

use crate::bxl::starlark_defs::lazy_ctx::operation::StarlarkLazy;
use crate::bxl::starlark_defs::lazy_ctx::operation::uquery::LazyUqueryOperation;
use crate::bxl::starlark_defs::target_list_expr::OwnedTargetNodeArg;
use crate::bxl::starlark_defs::target_list_expr::TargetNodeOrTargetLabelOrStr;

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative,
    Debug
)]
#[display("bxl.LazyUqueryContext")]
pub(crate) struct StarlarkLazyUqueryCtx {}

impl StarlarkLazyUqueryCtx {
    pub(crate) fn new() -> Self {
        Self {}
    }
}

starlark_simple_value!(StarlarkLazyUqueryCtx);

#[starlark_value(type = "bxl.LazyUqueryContext")]
impl<'v> StarlarkValue<'v> for StarlarkLazyUqueryCtx {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(lazy_uquery_methods)
    }
}

/// The context for performing lazy `uquery` operations in bxl. The functions offered on this ctx are
/// the same behaviour as the query functions available within uquery command.
#[starlark_module]
fn lazy_uquery_methods(builder: &mut MethodsBuilder) {
    /// Querying the test targets of the given target.
    /// It returns `UnconfiguredTargetSet`
    ///
    /// Example:
    /// ```python
    /// def _impl(ctx: bxl.Context):
    ///     res = ctx.lazy.uquery().testsof("//:foo_lib").catch().resolve()
    ///     if res.is_ok():
    ///         nodes = res.get()
    ///         ctx.output.print(nodes)
    ///     else:
    ///         err = res.unwrap_err()
    ///         ctx.output.print(err)
    /// ```
    fn testsof<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyUqueryCtx,
        #[starlark(require = pos)] expr: TargetNodeOrTargetLabelOrStr<'v>,
    ) -> anyhow::Result<StarlarkLazy> {
        let expr = OwnedTargetNodeArg::from_ref(&expr);
        let op = LazyUqueryOperation::TestsOf(expr);
        Ok(StarlarkLazy::new_uquery(op))
    }
}
