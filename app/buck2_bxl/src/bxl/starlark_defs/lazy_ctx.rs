/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_common::global_cfg_options::GlobalCfgOptions;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use operation::StarlarkLazy;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::values::list::UnpackList;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueTyped;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::providers_expr::ConfiguredProvidersLabelArg;
use crate::bxl::starlark_defs::target_list_expr::ConfiguredTargetNodeArg;
use crate::bxl::starlark_defs::target_list_expr::OwnedConfiguredTargetNodeArg;
use crate::bxl::starlark_defs::target_list_expr::OwnedTargetNodeArg;
use crate::bxl::starlark_defs::target_list_expr::TargetNodeOrTargetLabelOrStr;
use crate::bxl::value_as_starlark_target_label::ValueAsStarlarkTargetLabel;

pub(crate) mod operation;

/// Context for lazy/batch/error handling operations.
/// Available as `ctx.lazy`, has type `bxl.LazyContext`.
#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative
)]
#[derivative(Debug)]
#[display("bxl.LazyContext")]
#[allocative(skip)]
pub(crate) struct StarlarkLazyCtx<'v> {
    #[derivative(Debug = "ignore")]
    ctx: ValueTyped<'v, BxlContext<'v>>,
}

impl<'v> AllocValue<'v> for StarlarkLazyCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkLazyCtx<'v> {
    pub(crate) fn new(ctx: ValueTyped<'v, BxlContext<'v>>) -> Self {
        Self { ctx }
    }
}

#[starlark_value(type = "bxl.LazyContext", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkLazyCtx<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(lazy_ctx_methods)
    }
}

#[starlark_module]
fn lazy_ctx_methods(builder: &mut MethodsBuilder) {
    /// Join two lazy operations into a single operation that can be evaluated.
    ///
    /// Example:
    /// ```text
    /// def _impl(ctx):
    ///     ...
    ///     joined = ctx.lazy.join(ctx.lazy.analysis(t1), ctx.lazy.analysis(t2))
    ///     (res1, res2) = joined.resolve()
    ///     ctx.output.print(res1)
    ///     ctx.output.print(res2)
    /// ```
    fn join<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyCtx,
        #[starlark(require = pos)] lazy0: &'v StarlarkLazy,
        #[starlark(require = pos)] lazy1: &'v StarlarkLazy,
    ) -> anyhow::Result<StarlarkLazy> {
        Ok(StarlarkLazy::new_join(lazy0.dupe(), lazy1.dupe()))
    }

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

    /// Gets the configured target node for the `expr`.
    /// If given a string target pattern, it will resolve to a target set of configured target nodes.
    /// it also accepts an optional `target_platform` and an optional modifers list which is used
    /// to resolve configurations of any unconfigured target nodes.
    /// The `target_platform` is either a string that can be parsed as a target label, or a
    /// target label.
    ///
    /// The given `expr` is either:
    ///     - a single string that is a target ot a target pattern.
    ///     - a single target node or label, configured or unconfigured
    ///
    /// Note that this function does not accept `ConfiguredProviderLabel` (which is a configured provider label), since this
    /// is the label of a subtarget. You can get the underlying configured target label on the `Label`
    /// using `configured_targets()` (ex: `my_label.configured_target()`).
    ///
    /// This returns either a target set of `ConfiguredTargetNode`s if the given `expr` is a target pattern string,
    /// else a single `ConfiguredTargetNode`.
    ///
    /// When the given a target pattern (returns the target set), for the incompatible targets, it will print the warning message of these incompatible targets.
    /// Else (returns a single `ConfiguredTargetNode`), it will raise an error if incompatible when resolve. Use `Lazy.catch()` to catch the error.
    ///
    /// Example:
    /// ```text
    /// def _impl(ctx):
    ///     # returns a single `ConfiguredTargetNode`
    ///     node = ctx.lazy.configured_target_node("cell//path/to:target").resolve()
    ///
    ///     # returns a target set of `ConfiguredTargetNode`s
    ///     target_set = ctx.lazy.configured_target_node("cell//path/to:").resolve()
    /// ```
    fn configured_target_node<'v>(
        #[starlark(this)] this: &'v StarlarkLazyCtx,
        #[starlark(require = pos)] expr: ConfiguredTargetNodeArg<'v>,
        #[starlark(require = named, default = ValueAsStarlarkTargetLabel::NONE)]
        target_platform: ValueAsStarlarkTargetLabel<'v>,
        #[starlark(require = named, default = UnpackList::default())] modifiers: UnpackList<String>,
    ) -> anyhow::Result<StarlarkLazy> {
        let bxl_ctx = this.ctx;
        let target_platform = target_platform.parse_target_platforms(
            bxl_ctx.target_alias_resolver(),
            bxl_ctx.cell_resolver(),
            bxl_ctx.cell_alias_resolver(),
            bxl_ctx.cell_name(),
            &bxl_ctx.global_cfg_options().target_platform,
        );
        let cli_modifiers = modifiers.items;
        let global_cfg_options = target_platform.map(|target_platform| GlobalCfgOptions {
            target_platform,
            cli_modifiers: Arc::new(cli_modifiers),
        });
        let owned = OwnedConfiguredTargetNodeArg::from_ref(expr);
        Ok(StarlarkLazy::new_configured_target_node(
            owned,
            global_cfg_options,
        ))
    }

    /// Gets the unconfigured target node(s) for the `expr`
    ///
    /// The given `expr` is either:
    ///     - a single string that is a target ot a target pattern.
    ///     - a single unconfigured target node or label
    ///
    /// This returns either a target set of `UnconfiguredTargetNode`s if the given `expr` is a target pattern string,
    /// else a single `UnconfiguredTargetNode`.
    fn unconfigured_target_node<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyCtx,
        #[starlark(require = pos)] expr: TargetNodeOrTargetLabelOrStr<'v>,
    ) -> anyhow::Result<StarlarkLazy> {
        let expr = OwnedTargetNodeArg::from_ref(&expr);
        Ok(StarlarkLazy::new_unconfigured_target_node(expr))
    }
}
