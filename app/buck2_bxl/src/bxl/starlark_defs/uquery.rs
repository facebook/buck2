/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_build_api::query::bxl::BxlUqueryFunctions;
use buck2_build_api::query::bxl::NEW_BXL_UQUERY_FUNCTIONS;
use buck2_build_api::query::oneshot::QUERY_FRONTEND;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::syntax::simple::eval::set::TargetSetExt;
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use gazebo::prelude::OptionExt;
use gazebo::prelude::SliceExt;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::list::ListRef;
use starlark::values::none::NoneOr;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;
use starlark::StarlarkDocs;

use super::file_set::StarlarkFileSet;
use super::target_expr::TargetExpr;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::file_set::FileSetExpr;
use crate::bxl::starlark_defs::query_util::parse_query_evaluation_result;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

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
#[display(fmt = "{:?}", self)]
#[allocative(skip)]
pub struct StarlarkUQueryCtx<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    ctx: &'v BxlContext<'v>,
}

impl<'v> StarlarkValue<'v> for StarlarkUQueryCtx<'v> {
    starlark_type!("uqueryctx");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_uquery)
    }
}

pub(crate) async fn get_uquery_env<'v>(
    ctx: &'v BxlContext<'v>,
) -> anyhow::Result<Box<dyn BxlUqueryFunctions<'v> + 'v>> {
    (NEW_BXL_UQUERY_FUNCTIONS.get()?)(ctx.async_ctx.0, ctx.project_root().dupe(), ctx.cell_name)
        .await
}

impl<'v> AllocValue<'v> for StarlarkUQueryCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTypeRepr for &'v StarlarkUQueryCtx<'v> {
    fn starlark_type_repr() -> String {
        StarlarkUQueryCtx::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v StarlarkUQueryCtx<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v StarlarkUQueryCtx<'v>> {
        x.downcast_ref()
    }
}

impl<'v> StarlarkUQueryCtx<'v> {
    pub fn new(ctx: &'v BxlContext<'v>) -> anyhow::Result<Self> {
        Ok(Self { ctx })
    }
}

/// The context for performing `uquery` operations in bxl. The functions offered on this ctx are
/// the same behaviour as the query functions available within uquery command.
#[starlark_module]
fn register_uquery(builder: &mut MethodsBuilder) {
    /// The `allpaths` query for computing all dependency paths.
    fn allpaths<'v>(
        this: &StarlarkUQueryCtx<'v>,
        from: Value<'v>,
        to: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx.async_ctx.via_dice(|ctx| async {
            get_uquery_env(this.ctx)
                .await?
                .allpaths(
                    &*TargetExpr::<'v, TargetNode>::unpack(from, this.ctx, eval)
                        .await?
                        .get(ctx)
                        .await?,
                    &*TargetExpr::<'v, TargetNode>::unpack(to, this.ctx, eval)
                        .await?
                        .get(ctx)
                        .await?,
                )
                .await
                .map(StarlarkTargetSet::from)
        })
    }

    /// The somepaths query, which returns the graph of nodes on some arbitrary path from a start to destination target.
    fn somepath<'v>(
        this: &StarlarkUQueryCtx<'v>,
        from: Value<'v>,
        to: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx.async_ctx.via_dice(|ctx| async {
            get_uquery_env(this.ctx)
                .await?
                .somepath(
                    &*TargetExpr::<'v, TargetNode>::unpack(from, this.ctx, eval)
                        .await?
                        .get(ctx)
                        .await?,
                    &*TargetExpr::<'v, TargetNode>::unpack(to, this.ctx, eval)
                        .await?
                        .get(ctx)
                        .await?,
                )
                .await
                .map(StarlarkTargetSet::from)
        })
    }

    /// The attrfilter query for rule attribute filtering.
    fn attrfilter<'v>(
        this: &StarlarkUQueryCtx<'v>,
        attr: &str,
        value: &str,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx.async_ctx.via_dice(|ctx| async {
            let targets = TargetExpr::<'v, TargetNode>::unpack(targets, this.ctx, eval)
                .await?
                .get(ctx)
                .await?;
            targets
                .attrfilter(attr, &|v| Ok(v == value))
                .map(StarlarkTargetSet::from)
        })
    }

    /// The inputs query for finding input files.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_inputs(ctx):
    ///     result = ctx.uquery().inputs("root//bin:the_binary")
    ///     ctx.output.print(result)
    /// ```
    fn inputs<'v>(
        this: &StarlarkUQueryCtx<'v>,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkFileSet> {
        this.ctx
            .async_ctx
            .via_dice(|ctx| async {
                let targets = TargetExpr::<'v, TargetNode>::unpack(targets, this.ctx, eval)
                    .await?
                    .get(ctx)
                    .await?;
                targets.inputs()
            })
            .map(StarlarkFileSet::from)
    }

    /// The kind query for filtering targets by rule type.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_kind(ctx):
    ///     kind = ctx.uquery().kind(".*1", "bin/kind/...")
    ///     ctx.output.print(kind)
    /// ```
    fn kind<'v>(
        this: &StarlarkUQueryCtx<'v>,
        regex: &str,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx.async_ctx.via_dice(|ctx| async {
            let targets = TargetExpr::<'v, TargetNode>::unpack(targets, this.ctx, eval)
                .await?
                .get(ctx)
                .await?;
            targets.kind(regex).map(StarlarkTargetSet::from)
        })
    }

    /// The deps query for finding the transitive closure of dependencies.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_deps(ctx):
    ///     result = ctx.uquery().deps("root//bin:the_binary", 1)
    ///     ctx.output.print(result)
    /// ```
    fn deps<'v>(
        this: &StarlarkUQueryCtx<'v>,
        universe: Value<'v>,
        #[starlark(default = NoneOr::None)] depth: NoneOr<i32>,
        #[starlark(default = NoneOr::None)] filter: NoneOr<&'v str>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx
            .async_ctx
            .via_dice(|ctx| async {
                let filter = filter
                    .into_option()
                    .try_map(buck2_query_parser::parse_expr)?;

                get_uquery_env(this.ctx)
                    .await?
                    .deps(
                        &*TargetExpr::<'v, TargetNode>::unpack(universe, this.ctx, eval)
                            .await?
                            .get(ctx)
                            .await?,
                        depth.into_option(),
                        filter
                            .as_ref()
                            .map(|span| CapturedExpr { expr: span })
                            .as_ref(),
                    )
                    .await
            })
            .map(StarlarkTargetSet::from)
    }

    /// The rdeps query for finding the transitive closure of reverse dependencies.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_rdeps(ctx):
    ///     result = ctx.uquery().rdeps("root//bin:the_binary", "//lib:file1", 100)
    ///     ctx.output.print(result)
    /// ```
    fn rdeps<'v>(
        this: &StarlarkUQueryCtx<'v>,
        universe: Value<'v>,
        from: Value<'v>,
        depth: Option<i32>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx
            .async_ctx
            .via_dice(|ctx| async {
                get_uquery_env(this.ctx)
                    .await?
                    .rdeps(
                        &*TargetExpr::<'v, TargetNode>::unpack(universe, this.ctx, eval)
                            .await?
                            .get(ctx)
                            .await?,
                        &*TargetExpr::<'v, TargetNode>::unpack(from, this.ctx, eval)
                            .await?
                            .get(ctx)
                            .await?,
                        depth,
                    )
                    .await
            })
            .map(StarlarkTargetSet::from)
    }

    /// The filter query for filtering targets by name.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_filter(ctx):
    ///     result = ctx.uquery().filter(".*the_binary", "root//...")
    ///     ctx.output.print(result)
    /// ```
    fn filter<'v>(
        this: &StarlarkUQueryCtx<'v>,
        regex: &str,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx
            .async_ctx
            .via_dice(|ctx| async {
                let targets = TargetExpr::<'v, TargetNode>::unpack(targets, this.ctx, eval)
                    .await?
                    .get(ctx)
                    .await?;
                targets.filter_name(regex)
            })
            .map(StarlarkTargetSet::from)
    }

    /// The testsof query for listing the tests of the specified targets.
    ///
    /// Sample usage:
    /// ```text
    /// def _testsof_impl(ctx):
    ///     result = ctx.uquery().testsof("//:foo_lib")
    ///     ctx.output.print(result)
    /// ```
    fn testsof<'v>(
        this: &StarlarkUQueryCtx<'v>,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx
            .async_ctx
            .via_dice(|ctx| async {
                get_uquery_env(this.ctx)
                    .await?
                    .testsof(
                        &*TargetExpr::<'v, TargetNode>::unpack(targets, this.ctx, eval)
                            .await?
                            .get(ctx)
                            .await?,
                    )
                    .await
            })
            .map(StarlarkTargetSet::from)
    }

    /// Find the build file(s) that defines a target or a target set.
    ///
    /// Sample usage:
    /// ```text
    /// def _buildfile_impl(ctx):
    ///     owner = ctx.uquery().owner(["bin/TARGET", "bin/kind"])
    ///     result = ctx.uquery().buildfile(owner)
    ///     ctx.output.print(result)
    /// ```
    fn buildfile<'v>(
        this: &StarlarkUQueryCtx<'v>,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkFileSet> {
        this.ctx
            .async_ctx
            .via_dice(|ctx| async {
                let targets = &*TargetExpr::<'v, TargetNode>::unpack(targets, this.ctx, eval)
                    .await?
                    .get(ctx)
                    .await?;

                Ok(targets.buildfile())
            })
            .map(StarlarkFileSet::from)
    }

    /// The owner query for finding targets that own specified files.
    ///
    /// Sample usage:
    /// ```text
    /// def _owner_impl(ctx):
    ///     owner = ctx.uquery().owner("bin/TARGETS.fixture")
    ///     ctx.output.print(owner)
    /// ```
    fn owner<'v>(
        this: &StarlarkUQueryCtx,
        files: FileSetExpr,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                get_uquery_env(this.ctx)
                    .await?
                    .owner((files.get(this.ctx).await?).as_ref())
                    .await
            })
            .map(StarlarkTargetSet::from)
    }

    /// The attrregexfilter query for rule attribute filtering with regex.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_attrregexfilter(ctx):
    ///     filtered = ctx.uquery().attrregexfilter("foo", "he.lo", "bin/kind/...")
    ///     ctx.output.print(filtered)
    /// ```
    fn attrregexfilter<'v>(
        this: &StarlarkUQueryCtx<'v>,
        attribute: &str,
        value: &str,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx.async_ctx.via_dice(|ctx| async {
            let targets = TargetExpr::<'v, TargetNode>::unpack(targets, this.ctx, eval)
                .await?
                .get(ctx)
                .await?;
            targets
                .attrregexfilter(attribute, value)
                .map(StarlarkTargetSet::from)
        })
    }

    /// Evaluates some general query string, `query_args` can be a target_set of unconfigured nodes, or
    /// a list of strings.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_eval(ctx):
    ///     result1 = ctx.uquery().eval("inputs(cell//path/to/file:target)")
    ///     ctx.output.print(result1)
    ///
    ///     result2 = ctx.uquery().eval("inputs(%s)", query_args = ["cell//path/to/file:target"])
    ///     ctx.output.print(result2)
    /// ```
    fn eval<'v>(
        this: &StarlarkUQueryCtx<'v>,
        query: &'v str,
        #[starlark(default = NoneOr::None)] query_args: NoneOr<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let query_args = if query_args.is_none() {
            Vec::new()
        } else {
            let unwrapped_query_args = query_args.into_option().unwrap();
            if let Some(query_args) = unpack_unconfigured_query_args(unwrapped_query_args)? {
                query_args
            } else {
                return Err(ValueError::IncorrectParameterTypeWithExpected(
                    "list of strings, or a target_set with unconfigured nodes".to_owned(),
                    unwrapped_query_args.get_type().to_owned(),
                )
                .into());
            }
        };

        this.ctx.async_ctx.via_dice(|ctx| async {
            parse_query_evaluation_result(
                QUERY_FRONTEND
                    .get()?
                    .eval_uquery(ctx, &this.ctx.working_dir()?, query, &query_args, None)
                    .await?,
                eval,
            )
        })
    }
}

pub(crate) fn unpack_unconfigured_query_args<'v>(
    query_args: Value<'v>,
) -> anyhow::Result<Option<Vec<String>>> {
    if let Some(list) = <&ListRef>::unpack_value(query_args) {
        Ok(Some(list.content().try_map(|e| match e.unpack_str() {
            Some(arg) => Ok(arg.to_owned()),
            None => Err(ValueError::IncorrectParameterTypeWithExpected(
                "list of strings, or a target_set of unconfigured nodes".to_owned(),
                query_args.get_type().to_owned(),
            )),
        })?))
    } else if let Some(set) = <&StarlarkTargetSet<TargetNode>>::unpack_value(query_args) {
        // TODO - we really should change eval_query() to handle this, but escaping the unconfigured target label for now
        // as a quick solution.
        Ok(Some(
            set.0.iter_names().map(|e| format!("\"{}\"", e)).collect(),
        ))
    } else {
        Ok(None)
    }
}
