/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_build_api::query::bxl::BxlCqueryFunctions;
use buck2_build_api::query::bxl::NEW_BXL_CQUERY_FUNCTIONS;
use buck2_build_api::query::oneshot::CqueryOwnerBehavior;
use buck2_build_api::query::oneshot::QUERY_FRONTEND;
use buck2_common::global_cfg_options::GlobalCfgOptions;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use futures::FutureExt;
use gazebo::prelude::*;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::context::BxlContextNoDice;
use crate::bxl::starlark_defs::file_set::FileSetExpr;
use crate::bxl::starlark_defs::file_set::StarlarkFileSet;
use crate::bxl::starlark_defs::query_util::parse_query_evaluation_result;
use crate::bxl::starlark_defs::target_list_expr::filter_incompatible;
use crate::bxl::starlark_defs::target_list_expr::ConfiguredTargetListExprArg;
use crate::bxl::starlark_defs::target_list_expr::TargetListExpr;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::starlark_defs::uquery::UnpackUnconfiguredQueryArgs;
use crate::bxl::value_as_starlark_target_label::ValueAsStarlarkTargetLabel;

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(directory = "bxl")]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
#[allocative(skip)]
pub(crate) struct StarlarkCQueryCtx<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    ctx: &'v BxlContext<'v>,
    #[derivative(Debug = "ignore")]
    // Overrides the GlobalCfgOptions in the BxlContext
    global_cfg_options_override: GlobalCfgOptions,
}

#[starlark_value(type = "cqueryctx", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkCQueryCtx<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(cquery_methods)
    }
}

impl<'v> AllocValue<'v> for StarlarkCQueryCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

pub(crate) async fn get_cquery_env(
    ctx: &BxlContextNoDice<'_>,
    global_cfg_options_override: &GlobalCfgOptions,
) -> anyhow::Result<Box<dyn BxlCqueryFunctions>> {
    (NEW_BXL_CQUERY_FUNCTIONS.get()?)(
        global_cfg_options_override.clone(),
        ctx.project_root().dupe(),
        ctx.cell_name(),
        ctx.cell_resolver().dupe(),
    )
    .await
}

async fn unpack_targets<'v>(
    this: &StarlarkCQueryCtx<'v>,
    dice: &mut DiceComputations<'_>,
    targets: ConfiguredTargetListExprArg<'v>,
) -> anyhow::Result<TargetSet<ConfiguredTargetNode>> {
    filter_incompatible(
        TargetListExpr::<'v, ConfiguredTargetNode>::unpack(
            targets,
            &this.global_cfg_options_override,
            &this.ctx.data,
            dice,
        )
        .await?
        .get(dice)
        .await?,
        &this.ctx.data,
    )
}

impl<'v> StarlarkCQueryCtx<'v> {
    pub(crate) fn new(
        ctx: &'v BxlContext<'v>,
        global_target_platform: ValueAsStarlarkTargetLabel<'v>,
        global_cfg_options: &GlobalCfgOptions,
    ) -> anyhow::Result<StarlarkCQueryCtx<'v>> {
        let target_platform = global_target_platform.parse_target_platforms(
            ctx.target_alias_resolver(),
            ctx.cell_resolver(),
            ctx.cell_alias_resolver(),
            ctx.cell_name(),
            &global_cfg_options.target_platform,
        )?;

        Ok(Self {
            ctx,
            global_cfg_options_override: GlobalCfgOptions {
                target_platform,
                cli_modifiers: vec![].into(),
            },
        })
    }
}

/// The context for performing `cquery` operations in bxl. The functions offered on this ctx are
/// the same behaviour as the query functions available within cquery command.
///
/// Query results are `target_set`s of `target_node`s, which supports iteration,
/// indexing, `len()`, set addition/subtraction, and `equals()`.
#[starlark_module]
fn cquery_methods(builder: &mut MethodsBuilder) {
    /// The `allpaths` query for computing all dependency paths.
    fn allpaths<'v>(
        this: &StarlarkCQueryCtx<'v>,
        from: ConfiguredTargetListExprArg<'v>,
        to: ConfiguredTargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.via_dice(move |dice, ctx| {
            dice.via(|dice| {
                async move {
                    let from = unpack_targets(this, dice, from).await?;
                    let to = unpack_targets(this, dice, to).await?;
                    get_cquery_env(ctx, &this.global_cfg_options_override)
                        .await?
                        .allpaths(dice, &from, &to)
                        .await
                        .map(StarlarkTargetSet::from)
                }
                .boxed_local()
            })
        })
    }

    // The somepath query.
    fn somepath<'v>(
        this: &StarlarkCQueryCtx<'v>,
        from: ConfiguredTargetListExprArg<'v>,
        to: ConfiguredTargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.via_dice(|dice, ctx| {
            dice.via(|dice| {
                async {
                    let from = unpack_targets(this, dice, from).await?;
                    let to = unpack_targets(this, dice, to).await?;
                    get_cquery_env(ctx, &this.global_cfg_options_override)
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
        this: &StarlarkCQueryCtx<'v>,
        attr: &str,
        value: &str,
        targets: ConfiguredTargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.via_dice(|dice, _| {
            dice.via(|dice| {
                async {
                    unpack_targets(this, dice, targets)
                        .await?
                        .attrfilter(attr, &|v| Ok(v == value))
                        .map(StarlarkTargetSet::from)
                }
                .boxed_local()
            })
        })
    }

    /// The kind query for filtering targets by rule type.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_kind(ctx):
    ///     kind = ctx.cquery().kind(".*1", "bin/kind/...")
    ///     ctx.output.print(kind)
    /// ```
    fn kind<'v>(
        this: &StarlarkCQueryCtx<'v>,
        regex: &str,
        targets: ConfiguredTargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.via_dice(|dice, _| {
            dice.via(|dice| {
                async {
                    unpack_targets(this, dice, targets)
                        .await?
                        .kind(regex)
                        .map(StarlarkTargetSet::from)
                }
                .boxed_local()
            })
        })
    }

    /// The attrregexfilter query for rule attribute filtering with regex.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_attrregexfilter(ctx):
    ///     filtered = ctx.cquery().attrregexfilter("foo", "he.lo", "bin/kind/...")
    ///     ctx.output.print(filtered)
    /// ```
    fn attrregexfilter<'v>(
        this: &StarlarkCQueryCtx<'v>,
        attribute: &str,
        value: &str,
        targets: ConfiguredTargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.via_dice(|dice, _| {
            dice.via(|dice| {
                async {
                    unpack_targets(this, dice, targets)
                        .await?
                        .attrregexfilter(attribute, value)
                        .map(StarlarkTargetSet::from)
                }
                .boxed_local()
            })
        })
    }

    /// The owner query for finding targets that own specified files. Note that if you do not pass in a cell
    /// path (where the format is `<cell>//path/to/file`), the path is resolved against the cell that the BXL
    /// script lives in. If you need to evaluate a file path that lives in a different cell, you must pass in
    /// the fully qualified cell path.
    ///
    /// Sample usage:
    /// ```text
    /// def _owner_impl(ctx):
    ///     owner = ctx.cquery().owner("bin/TARGETS.fixture", "foo//target/universe/...")
    ///     ctx.output.print(owner)
    /// ```
    fn owner<'v>(
        this: &StarlarkCQueryCtx<'v>,
        files: FileSetExpr,
        #[starlark(default = NoneOr::None)] universe: NoneOr<ConfiguredTargetListExprArg<'v>>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .via_dice(|dice, ctx| {
                dice.via(|dice| {
                    async {
                        let universe = match universe.into_option() {
                            Some(universe) => Some(unpack_targets(this, dice, universe).await?),
                            None => None,
                        };

                        get_cquery_env(ctx, &this.global_cfg_options_override)
                            .await?
                            .owner(dice, files.get(ctx).await?.as_ref(), universe.as_ref())
                            .await
                    }
                    .boxed_local()
                })
            })
            .map(StarlarkTargetSet::from)
    }

    /// The deps query for finding the transitive closure of dependencies.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_deps(ctx):
    ///     result = ctx.cquery().deps("root//bin:the_binary", 1)
    ///     ctx.output.print(result)
    /// ```
    fn deps<'v>(
        this: &StarlarkCQueryCtx<'v>,
        universe: ConfiguredTargetListExprArg<'v>,
        #[starlark(default = NoneOr::None)] depth: NoneOr<i32>,
        #[starlark(default = NoneOr::None)] filter: NoneOr<&'v str>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .via_dice(|dice, ctx| {
                dice.via(|dice| {
                    async {
                        let filter = filter
                            .into_option()
                            .try_map(buck2_query_parser::parse_expr)?;

                        let targets = unpack_targets(this, dice, universe).await?;

                        get_cquery_env(ctx, &this.global_cfg_options_override)
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

    /// The filter query for filtering targets by name.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_filter(ctx):
    ///     result = ctx.cquery().filter(".*the_binary", "root//...")
    ///     ctx.output.print(result)
    /// ```
    fn filter<'v>(
        this: &StarlarkCQueryCtx<'v>,
        regex: &str,
        targets: ConfiguredTargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .via_dice(|dice, _| {
                dice.via(|dice| {
                    async {
                        unpack_targets(this, dice, targets)
                            .await?
                            .filter_name(regex)
                    }
                    .boxed_local()
                })
            })
            .map(StarlarkTargetSet::from)
    }

    /// The inputs query for finding input files.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_inputs(ctx):
    ///     result = ctx.cquery().inputs("root//bin:the_binary")
    ///     ctx.output.print(result)
    /// ```
    fn inputs<'v>(
        this: &StarlarkCQueryCtx<'v>,
        targets: ConfiguredTargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkFileSet> {
        this.ctx
            .via_dice(|dice, _| {
                dice.via(|dice| {
                    async { unpack_targets(this, dice, targets).await?.inputs() }.boxed_local()
                })
            })
            .map(StarlarkFileSet::from)
    }

    /// The testsof query for listing the tests of the specified targets.
    fn testsof<'v>(
        this: &StarlarkCQueryCtx<'v>,
        targets: ConfiguredTargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .via_dice(|dice, ctx| {
                dice.via(|dice| {
                    async {
                        let targets = unpack_targets(this, dice, targets).await?;
                        get_cquery_env(ctx, &this.global_cfg_options_override)
                            .await?
                            .testsof(dice, &targets)
                            .await
                    }
                    .boxed_local()
                })
            })
            .map(StarlarkTargetSet::from)
    }

    /// The testsof query for listing the tests of the specified targets. Performs default target platform
    /// resolution under the hood for the tests found.
    fn testsof_with_default_target_platform<'v>(
        this: &StarlarkCQueryCtx<'v>,
        targets: ConfiguredTargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .via_dice(|dice, ctx| {
                dice.via(|dice| {
                    async {
                        let targets = unpack_targets(this, dice, targets).await?;
                        let maybe_compatibles =
                            get_cquery_env(ctx, &this.global_cfg_options_override)
                                .await?
                                .testsof_with_default_target_platform(dice, &targets)
                                .await?;

                        filter_incompatible(maybe_compatibles, ctx)
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
    ///     result = ctx.cquery().rdeps("root//bin:the_binary", "//lib:file1", 100)
    ///     ctx.output.print(result)
    /// ```
    fn rdeps<'v>(
        this: &StarlarkCQueryCtx<'v>,
        universe: ConfiguredTargetListExprArg<'v>,
        from: ConfiguredTargetListExprArg<'v>,
        depth: Option<i32>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .via_dice(|dice, ctx| {
                dice.via(|dice| {
                    async {
                        let universe = unpack_targets(this, dice, universe).await?;
                        let targets = unpack_targets(this, dice, from).await?;
                        get_cquery_env(ctx, &this.global_cfg_options_override)
                            .await?
                            .rdeps(dice, &universe, &targets, depth)
                            .await
                    }
                    .boxed_local()
                })
            })
            .map(StarlarkTargetSet::from)
    }

    /// Evaluates some general query string. `query_args` can be a target_set of unconfigured nodes, or
    /// a list of strings. Returns a `dict` of target labels mapped to their `target_set` results if `query_args`
    /// was passed in, otherwise returns a single `target_set`.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_eval(ctx):
    ///     result1 = ctx.cquery().eval("inputs(root//bin:the_binary)")
    ///     ctx.output.print(result1)
    ///
    ///     result2 = ctx.cquery().eval("inputs(%s)", query_args = ["cell//path/to/file:target"])
    ///     ctx.output.print(result2)
    /// ```
    fn eval<'v>(
        this: &StarlarkCQueryCtx<'v>,
        query: &'v str,
        #[starlark(default = NoneOr::None)] query_args: NoneOr<UnpackUnconfiguredQueryArgs<'v>>,
        #[starlark(default = NoneOr::None)] target_universe: NoneOr<UnpackListOrTuple<String>>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let query_args = match query_args {
            NoneOr::None => Vec::new(),
            NoneOr::Other(query_args) => query_args.into_strings(),
        };

        this.ctx.via_dice(|dice, ctx| {
            dice.via(|dice| {
                async {
                    parse_query_evaluation_result(
                        QUERY_FRONTEND
                            .get()?
                            .eval_cquery(
                                dice,
                                &ctx.working_dir()?,
                                CqueryOwnerBehavior::Correct,
                                query,
                                &query_args,
                                this.global_cfg_options_override.clone(),
                                target_universe.into_option().as_ref().map(|v| &v.items[..]),
                            )
                            .await?,
                        eval,
                    )
                }
                .boxed_local()
            })
        })
    }

    /// Find the build file(s) that defines a target or a target set.
    ///
    /// Sample usage:
    /// ```text
    /// def _buildfile_impl(ctx):
    ///     owner = ctx.cquery().owner(["bin/TARGET", "bin/kind"])
    ///     result = ctx.cquery().buildfile(owner)
    ///     ctx.output.print(result)
    /// ```
    fn buildfile<'v>(
        this: &StarlarkCQueryCtx<'v>,
        targets: ConfiguredTargetListExprArg<'v>,
    ) -> anyhow::Result<StarlarkFileSet> {
        this.ctx
            .via_dice(|dice, _| {
                dice.via(|dice| {
                    async {
                        let targets = unpack_targets(this, dice, targets).await?;
                        Ok(targets.buildfile())
                    }
                    .boxed_local()
                })
            })
            .map(StarlarkFileSet::from)
    }
}
