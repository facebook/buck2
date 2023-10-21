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
use futures::FutureExt;
use gazebo::prelude::OptionExt;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::list::UnpackList;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::StarlarkDocs;

use super::file_set::StarlarkFileSet;
use super::target_list_expr::TargetListExpr;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::context::BxlContextNoDice;
use crate::bxl::starlark_defs::file_set::FileSetExpr;
use crate::bxl::starlark_defs::query_util::parse_query_evaluation_result;
use crate::bxl::starlark_defs::target_list_expr::TargetListExprArg;
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
pub(crate) struct StarlarkUQueryCtx<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    ctx: &'v BxlContext<'v>,
}

#[starlark_value(type = "uqueryctx", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkUQueryCtx<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(uquery_methods)
    }
}

pub(crate) async fn get_uquery_env<'v>(
    ctx: &BxlContextNoDice<'v>,
) -> anyhow::Result<Box<dyn BxlUqueryFunctions>> {
    (NEW_BXL_UQUERY_FUNCTIONS.get()?)(
        ctx.project_root().dupe(),
        ctx.cell_name,
        ctx.cell_resolver.dupe(),
    )
    .await
}

impl<'v> AllocValue<'v> for StarlarkUQueryCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkUQueryCtx<'v> {
    pub(crate) fn new(ctx: &'v BxlContext<'v>) -> anyhow::Result<Self> {
        Ok(Self { ctx })
    }
}

/// The context for performing `uquery` operations in bxl. The functions offered on this ctx are
/// the same behaviour as the query functions available within uquery command.
#[starlark_module]
fn uquery_methods(builder: &mut MethodsBuilder) {
    /// The `allpaths` query for computing all dependency paths.
    fn allpaths<'v>(
        this: &StarlarkUQueryCtx<'v>,
        from: TargetListExprArg<'v>,
        to: TargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx.via_dice(|mut dice, ctx| {
            dice.via(|dice| {
                async {
                    let from = TargetListExpr::<'v, TargetNode>::unpack(from, ctx, dice)
                        .await?
                        .get(dice)
                        .await?;
                    let to = TargetListExpr::<'v, TargetNode>::unpack(to, ctx, dice)
                        .await?
                        .get(dice)
                        .await?;
                    get_uquery_env(ctx)
                        .await?
                        .allpaths(dice, &from, &to)
                        .await
                        .map(StarlarkTargetSet::from)
                }
                .boxed_local()
            })
        })
    }

    /// The somepaths query, which returns the graph of nodes on some arbitrary path from a start to destination target.
    fn somepath<'v>(
        this: &StarlarkUQueryCtx<'v>,
        from: TargetListExprArg<'v>,
        to: TargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx.via_dice(|mut dice, ctx| {
            dice.via(|dice| {
                async {
                    let from = TargetListExpr::<'v, TargetNode>::unpack(from, ctx, dice)
                        .await?
                        .get(dice)
                        .await?;
                    let to = TargetListExpr::<'v, TargetNode>::unpack(to, ctx, dice)
                        .await?
                        .get(dice)
                        .await?;
                    get_uquery_env(ctx)
                        .await?
                        .somepath(dice, &from, &to)
                        .await
                        .map(StarlarkTargetSet::from)
                }
                .boxed_local()
            })
        })
    }

    /// The attrfilter query for rule attribute filtering.
    fn attrfilter<'v>(
        this: &StarlarkUQueryCtx<'v>,
        attr: &str,
        value: &str,
        targets: TargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx.via_dice(|mut dice, ctx| {
            dice.via(|dice| {
                async {
                    let targets = TargetListExpr::<'v, TargetNode>::unpack(targets, ctx, dice)
                        .await?
                        .get(dice)
                        .await?;
                    targets
                        .attrfilter(attr, &|v| Ok(v == value))
                        .map(StarlarkTargetSet::from)
                }
                .boxed_local()
            })
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
        targets: TargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkFileSet> {
        this.ctx
            .via_dice(|mut dice, ctx| {
                dice.via(|dice| {
                    async {
                        let targets = TargetListExpr::<'v, TargetNode>::unpack(targets, ctx, dice)
                            .await?
                            .get(dice)
                            .await?;
                        targets.inputs()
                    }
                    .boxed_local()
                })
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
        targets: TargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx.via_dice(|mut dice, ctx| {
            dice.via(|dice| {
                async {
                    let targets = TargetListExpr::<'v, TargetNode>::unpack(targets, ctx, dice)
                        .await?
                        .get(dice)
                        .await?;
                    targets.kind(regex).map(StarlarkTargetSet::from)
                }
                .boxed_local()
            })
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
        universe: TargetListExprArg<'v>,
        #[starlark(default = NoneOr::None)] depth: NoneOr<i32>,
        #[starlark(default = NoneOr::None)] filter: NoneOr<&'v str>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx
            .via_dice(|mut dice, ctx| {
                dice.via(|dice| {
                    async {
                        let filter = filter
                            .into_option()
                            .try_map(buck2_query_parser::parse_expr)?;

                        let targets = TargetListExpr::<'v, TargetNode>::unpack(universe, ctx, dice)
                            .await?
                            .get(dice)
                            .await?;

                        get_uquery_env(ctx)
                            .await?
                            .deps(
                                dice,
                                &targets,
                                depth.into_option(),
                                filter
                                    .as_ref()
                                    .map(|span| CapturedExpr { expr: span })
                                    .as_ref(),
                            )
                            .await
                    }
                    .boxed_local()
                })
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
        universe: TargetListExprArg<'v>,
        from: TargetListExprArg<'v>,
        depth: Option<i32>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx
            .via_dice(|mut dice, ctx| {
                dice.via(|dice| {
                    async {
                        let universe =
                            TargetListExpr::<'v, TargetNode>::unpack(universe, ctx, dice)
                                .await?
                                .get(dice)
                                .await?;

                        let targets = TargetListExpr::<'v, TargetNode>::unpack(from, ctx, dice)
                            .await?
                            .get(dice)
                            .await?;

                        get_uquery_env(ctx)
                            .await?
                            .rdeps(dice, &universe, &targets, depth)
                            .await
                    }
                    .boxed_local()
                })
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
        targets: TargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx
            .via_dice(|mut dice, ctx| {
                dice.via(|dice| {
                    async {
                        let targets = TargetListExpr::<'v, TargetNode>::unpack(targets, ctx, dice)
                            .await?
                            .get(dice)
                            .await?;
                        targets.filter_name(regex)
                    }
                    .boxed_local()
                })
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
        targets: TargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx
            .via_dice(|mut dice, ctx| {
                dice.via(|dice| {
                    async {
                        let targets = TargetListExpr::<'v, TargetNode>::unpack(targets, ctx, dice)
                            .await?
                            .get(dice)
                            .await?;
                        get_uquery_env(ctx).await?.testsof(dice, &targets).await
                    }
                    .boxed_local()
                })
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
        targets: TargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkFileSet> {
        this.ctx
            .via_dice(|mut dice, ctx| {
                dice.via(|dice| {
                    async {
                        let targets =
                            &*TargetListExpr::<'v, TargetNode>::unpack(targets, ctx, dice)
                                .await?
                                .get(dice)
                                .await?;

                        Ok(targets.buildfile())
                    }
                    .boxed_local()
                })
            })
            .map(StarlarkFileSet::from)
    }

    /// The owner query for finding targets that own specified files. Note that if you do not pass in a cell
    /// path (where the format is `<cell>//path/to/file`), the path is resolved against the cell that the BXL
    /// script lives in. If you need to evaluate a file path that lives in a different cell, you must pass in
    /// the fully qualified cell path.
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
            .via_dice(|mut dice, ctx| {
                dice.via(|dice| {
                    async {
                        get_uquery_env(ctx)
                            .await?
                            .owner(dice, (files.get(ctx).await?).as_ref())
                            .await
                    }
                    .boxed_local()
                })
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
        targets: TargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<TargetNode>> {
        this.ctx.via_dice(|mut dice, ctx| {
            dice.via(|dice| {
                async {
                    let targets = TargetListExpr::<'v, TargetNode>::unpack(targets, ctx, dice)
                        .await?
                        .get(dice)
                        .await?;
                    targets
                        .attrregexfilter(attribute, value)
                        .map(StarlarkTargetSet::from)
                }
                .boxed_local()
            })
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
            if let Some(query_args) = unpack_unconfigured_query_args(unwrapped_query_args) {
                query_args
            } else {
                return Err(ValueError::IncorrectParameterTypeWithExpected(
                    "list of strings, or a target_set with unconfigured nodes".to_owned(),
                    unwrapped_query_args.get_type().to_owned(),
                )
                .into());
            }
        };

        this.ctx.via_dice(|mut dice, _| {
            dice.via(|dice| {
                async {
                    parse_query_evaluation_result(
                        QUERY_FRONTEND
                            .get()?
                            .eval_uquery(dice, &this.ctx.working_dir()?, query, &query_args, None)
                            .await?,
                        eval,
                    )
                }
                .boxed_local()
            })
        })
    }
}

#[allow(clippy::manual_map)]
pub(crate) fn unpack_unconfigured_query_args(query_args: Value) -> Option<Vec<String>> {
    if let Some(list) = UnpackList::unpack_value(query_args) {
        Some(list.items)
    } else if let Some(set) = <&StarlarkTargetSet<TargetNode>>::unpack_value(query_args) {
        // TODO - we really should change eval_query() to handle this, but escaping the unconfigured target label for now
        // as a quick solution.
        Some(set.0.iter_names().map(|e| format!("\"{}\"", e)).collect())
    } else {
        None
    }
}
