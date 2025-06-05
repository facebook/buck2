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
use starlark::values::none::NoneOr;
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
    /// Computes all dependency paths from `from` to `to`, with optional filter.
    ///
    /// Example:
    /// ```python
    /// res = ctx.lazy.uquery().allpaths("//:foo", "//:bar", filter = "attrfilter('name', 'some_name', target_deps()")).catch().resolve()
    /// ```
    fn allpaths<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyUqueryCtx,
        #[starlark(require = pos)] from: TargetNodeOrTargetLabelOrStr<'v>,
        #[starlark(require = pos)] to: TargetNodeOrTargetLabelOrStr<'v>,
        #[starlark(require = named, default = NoneOr::None)] filter: NoneOr<&'v str>,
    ) -> anyhow::Result<StarlarkLazy> {
        let from = OwnedTargetNodeArg::from_ref(&from);
        let to = OwnedTargetNodeArg::from_ref(&to);
        let filter = filter.into_option().map(|s| s.to_owned());
        let op = LazyUqueryOperation::AllPaths { from, to, filter };
        Ok(StarlarkLazy::new_uquery(op))
    }

    /// Computes some dependency path from `from` to `to`, with optional filter.
    ///
    /// Example:
    /// ```python
    /// res = ctx.lazy.uquery().somepath("//:foo", "//:bar", filter = "attrfilter('name', 'some_name', target_deps()")).catch().resolve()
    /// ```
    fn somepath<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyUqueryCtx,
        #[starlark(require = pos)] from: TargetNodeOrTargetLabelOrStr<'v>,
        #[starlark(require = pos)] to: TargetNodeOrTargetLabelOrStr<'v>,
        #[starlark(require = named, default = NoneOr::None)] filter: NoneOr<&'v str>,
    ) -> anyhow::Result<StarlarkLazy> {
        let from = OwnedTargetNodeArg::from_ref(&from);
        let to = OwnedTargetNodeArg::from_ref(&to);
        let filter = filter.into_option().map(|s| s.to_owned());
        let op = LazyUqueryOperation::SomePath { from, to, filter };
        Ok(StarlarkLazy::new_uquery(op))
    }

    /// Filters targets by attribute value.
    ///
    /// Example:
    /// ```python
    /// res = ctx.lazy.uquery().attrfilter("name", "some_name", "//:foo").catch().resolve()
    /// ```
    fn attrfilter<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyUqueryCtx,
        #[starlark(require = pos)] attr: &'v str,
        #[starlark(require = pos)] value: &'v str,
        #[starlark(require = pos)] targets: TargetNodeOrTargetLabelOrStr<'v>,
    ) -> anyhow::Result<StarlarkLazy> {
        let attr = attr.to_owned();
        let value = value.to_owned();
        let targets = OwnedTargetNodeArg::from_ref(&targets);
        let op = LazyUqueryOperation::AttrFilter {
            attr,
            value,
            targets,
        };
        Ok(StarlarkLazy::new_uquery(op))
    }

    /// Finds input files for the given targets.
    ///
    /// Example:
    /// ```python
    /// res = ctx.lazy.uquery().inputs("//:foo").catch().resolve()
    /// ```
    fn inputs<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyUqueryCtx,
        #[starlark(require = pos)] targets: TargetNodeOrTargetLabelOrStr<'v>,
    ) -> anyhow::Result<StarlarkLazy> {
        let targets = OwnedTargetNodeArg::from_ref(&targets);
        let op = LazyUqueryOperation::Inputs(targets);
        Ok(StarlarkLazy::new_uquery(op))
    }

    /// Filters targets by rule type using a regex pattern.
    ///
    /// Example:
    /// ```python
    /// res = ctx.lazy.uquery().kind("cpp.*", "//:foo").catch().resolve()
    /// ```
    fn kind<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyUqueryCtx,
        #[starlark(require = pos)] regex: &'v str,
        #[starlark(require = pos)] targets: TargetNodeOrTargetLabelOrStr<'v>,
    ) -> anyhow::Result<StarlarkLazy> {
        let regex = regex.to_owned();
        let targets = OwnedTargetNodeArg::from_ref(&targets);
        let op = LazyUqueryOperation::Kind { regex, targets };
        Ok(StarlarkLazy::new_uquery(op))
    }

    /// Finds the [transitive closure](https://en.wikipedia.org/wiki/Transitive_closure) of dependencies.
    ///
    /// Example:
    /// ```python
    /// res = ctx.lazy.uquery().deps("//:foo", 1, filter = "attrfilter('name', 'some_name', target_deps())").catch().resolve()
    /// ```
    fn deps<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyUqueryCtx,
        #[starlark(require = pos)] universe: TargetNodeOrTargetLabelOrStr<'v>,
        #[starlark(require = pos, default = NoneOr::None)] depth: NoneOr<i32>,
        #[starlark(require = named, default = NoneOr::None)] filter: NoneOr<&'v str>,
    ) -> anyhow::Result<StarlarkLazy> {
        let universe = OwnedTargetNodeArg::from_ref(&universe);
        let depth = depth.into_option();
        let filter = filter.into_option().map(|s| s.to_owned());
        let op = LazyUqueryOperation::Deps {
            universe,
            depth,
            filter,
        };
        Ok(StarlarkLazy::new_uquery(op))
    }

    /// Finds the [transitive closure](https://en.wikipedia.org/wiki/Transitive_closure) of reverse dependencies.
    ///
    /// Example:
    /// ```python
    /// res = ctx.lazy.uquery().rdeps("//:universe", "//:from", 1, filter = "attrfilter('name', 'some_name', target_deps())").catch().resolve()
    /// ```
    fn rdeps<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyUqueryCtx,
        #[starlark(require = pos)] universe: TargetNodeOrTargetLabelOrStr<'v>,
        #[starlark(require = pos)] from: TargetNodeOrTargetLabelOrStr<'v>,
        #[starlark(require = pos, default = NoneOr::None)] depth: NoneOr<i32>,
        #[starlark(require = named, default = NoneOr::None)] filter: NoneOr<&'v str>,
    ) -> anyhow::Result<StarlarkLazy> {
        let universe = OwnedTargetNodeArg::from_ref(&universe);
        let from = OwnedTargetNodeArg::from_ref(&from);
        let depth = depth.into_option();
        let filter = filter.into_option().map(|s| s.to_owned());
        let op = LazyUqueryOperation::Rdeps {
            universe,
            from,
            depth,
            filter,
        };
        Ok(StarlarkLazy::new_uquery(op))
    }

    /// Filters targets by name using a regex pattern.
    ///
    /// Example:
    /// ```python
    /// res = ctx.lazy.uquery().filter(".*the_binary", "//:foo").catch().resolve()
    /// ```
    fn filter<'v>(
        #[starlark(this)] _this: &'v StarlarkLazyUqueryCtx,
        #[starlark(require = pos)] regex: &'v str,
        #[starlark(require = pos)] targets: TargetNodeOrTargetLabelOrStr<'v>,
    ) -> anyhow::Result<StarlarkLazy> {
        let regex = regex.to_owned();
        let targets = OwnedTargetNodeArg::from_ref(&targets);
        let op = LazyUqueryOperation::Filter { regex, targets };
        Ok(StarlarkLazy::new_uquery(op))
    }

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
