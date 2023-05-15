/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use anyhow::Context;
use buck2_build_api::query::bxl::BxlCqueryFunctions;
use buck2_build_api::query::bxl::NEW_BXL_CQUERY_FUNCTIONS;
use buck2_build_api::query::oneshot::CqueryOwnerBehavior;
use buck2_build_api::query::oneshot::QUERY_FRONTEND;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::set::TargetSetExt;
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use gazebo::prelude::*;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_type;
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

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::file_set::FileSetExpr;
use crate::bxl::starlark_defs::file_set::StarlarkFileSet;
use crate::bxl::starlark_defs::query_util::parse_query_evaluation_result;
use crate::bxl::starlark_defs::target_expr::filter_incompatible;
use crate::bxl::starlark_defs::target_expr::TargetExpr;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::starlark_defs::uquery::unpack_unconfigured_query_args;
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
pub struct StarlarkCQueryCtx<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    ctx: &'v BxlContext<'v>,
    #[derivative(Debug = "ignore")]
    target_platform: Option<TargetLabel>,
}

impl<'v> StarlarkValue<'v> for StarlarkCQueryCtx<'v> {
    starlark_type!("cqueryctx");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_cquery)
    }
}

impl<'v> AllocValue<'v> for StarlarkCQueryCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTypeRepr for &'v StarlarkCQueryCtx<'v> {
    fn starlark_type_repr() -> String {
        StarlarkCQueryCtx::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v StarlarkCQueryCtx<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v StarlarkCQueryCtx<'v>> {
        x.downcast_ref()
    }
}

pub(crate) async fn get_cquery_env<'v>(
    ctx: &'v BxlContext<'v>,
    target_platform: Option<TargetLabel>,
) -> anyhow::Result<Box<dyn BxlCqueryFunctions<'v> + 'v>> {
    (NEW_BXL_CQUERY_FUNCTIONS.get()?)(
        ctx.async_ctx.0,
        target_platform,
        ctx.project_root().dupe(),
        ctx.cell_name,
    )
    .await
}

impl<'v> StarlarkCQueryCtx<'v> {
    pub async fn new(
        ctx: &'v BxlContext<'v>,
        global_target_platform: Value<'v>,
        default_target_platform: &Option<TargetLabel>,
    ) -> anyhow::Result<StarlarkCQueryCtx<'v>> {
        let target_platform = global_target_platform.parse_target_platforms(
            &ctx.target_alias_resolver,
            &ctx.cell_resolver,
            ctx.cell_name,
            default_target_platform,
        )?;

        Ok(Self {
            ctx,
            target_platform,
        })
    }
}

/// The context for performing `cquery` operations in bxl. The functions offered on this ctx are
/// the same behaviour as the query functions available within cquery command.
///
/// Query results are `[StarlarkTargetSet]`s of `[ConfiguredTargetNod]`s, which supports iteration,
/// indexing, `len()`, set addition/subtraction, and `equals()`.
#[starlark_module]
fn register_cquery(builder: &mut MethodsBuilder) {
    /// The `allpaths` query for computing all dependency paths.
    fn allpaths<'v>(
        this: &StarlarkCQueryCtx<'v>,
        from: Value<'v>,
        to: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.async_ctx.via(|| async {
            get_cquery_env(this.ctx, this.target_platform.dupe())
                .await?
                .allpaths(
                    &filter_incompatible(
                        TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                            from,
                            &this.target_platform,
                            this.ctx,
                            eval,
                        )
                        .await?
                        .get(this.ctx.async_ctx.0)
                        .await?
                        .into_iter(),
                        this.ctx,
                    )?,
                    &filter_incompatible(
                        TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                            to,
                            &this.target_platform,
                            this.ctx,
                            eval,
                        )
                        .await?
                        .get(this.ctx.async_ctx.0)
                        .await?
                        .into_iter(),
                        this.ctx,
                    )?,
                )
                .await
                .map(StarlarkTargetSet::from)
        })
    }

    // The somepath query.
    fn somepath<'v>(
        this: &StarlarkCQueryCtx<'v>,
        from: Value<'v>,
        to: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.async_ctx.via(|| async {
            get_cquery_env(this.ctx, this.target_platform.dupe())
                .await?
                .somepath(
                    &filter_incompatible(
                        TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                            from,
                            &this.target_platform,
                            this.ctx,
                            eval,
                        )
                        .await?
                        .get(this.ctx.async_ctx.0)
                        .await?
                        .into_iter(),
                        this.ctx,
                    )?,
                    &filter_incompatible(
                        TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                            to,
                            &this.target_platform,
                            this.ctx,
                            eval,
                        )
                        .await?
                        .get(this.ctx.async_ctx.0)
                        .await?
                        .into_iter(),
                        this.ctx,
                    )?,
                )
                .await
                .map(StarlarkTargetSet::from)
        })
    }

    /// The attrfilter query for rule attribute filtering.
    fn attrfilter<'v>(
        this: &StarlarkCQueryCtx<'v>,
        attr: &str,
        value: &str,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.async_ctx.via(|| async {
            filter_incompatible(
                TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                    targets,
                    &this.target_platform,
                    this.ctx,
                    eval,
                )
                .await?
                .get(this.ctx.async_ctx.0)
                .await?
                .into_iter(),
                this.ctx,
            )?
            .attrfilter(attr, &|v| Ok(v == value))
            .map(StarlarkTargetSet::from)
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
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.async_ctx.via(|| async {
            filter_incompatible(
                TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                    targets,
                    &this.target_platform,
                    this.ctx,
                    eval,
                )
                .await?
                .get(this.ctx.async_ctx.0)
                .await?
                .into_iter(),
                this.ctx,
            )?
            .kind(regex)
            .map(StarlarkTargetSet::from)
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
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.async_ctx.via(|| async {
            filter_incompatible(
                TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                    targets,
                    &this.target_platform,
                    this.ctx,
                    eval,
                )
                .await?
                .get(this.ctx.async_ctx.0)
                .await?
                .into_iter(),
                this.ctx,
            )?
            .attrregexfilter(attribute, value)
            .map(StarlarkTargetSet::from)
        })
    }

    /// The owner query for finding targets that own specified files.
    ///
    /// Sample usage:
    /// ```text
    /// def _owner_impl(ctx):
    ///     owner = ctx.cquery().owner("bin/TARGETS.fixture")
    ///     ctx.output.print(owner)
    /// ```
    fn owner<'v>(
        this: &StarlarkCQueryCtx,
        files: FileSetExpr,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                get_cquery_env(this.ctx, this.target_platform.dupe())
                    .await?
                    .owner(files.get(this.ctx).await?.as_ref())
                    .await
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
        universe: Value<'v>,
        #[starlark(default = NoneOr::None)] depth: NoneOr<i32>,
        #[starlark(default = NoneOr::None)] filter: NoneOr<&'v str>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                let filter = filter
                    .into_option()
                    .try_map(buck2_query_parser::parse_expr)?;

                get_cquery_env(this.ctx, this.target_platform.dupe())
                    .await?
                    .deps(
                        &filter_incompatible(
                            TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                                universe,
                                &this.target_platform,
                                this.ctx,
                                eval,
                            )
                            .await?
                            .get(this.ctx.async_ctx.0)
                            .await?
                            .into_iter(),
                            this.ctx,
                        )?,
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
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                filter_incompatible(
                    TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        targets,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(this.ctx.async_ctx.0)
                    .await?
                    .into_iter(),
                    this.ctx,
                )?
                .filter_name(regex)
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
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkFileSet> {
        this.ctx
            .async_ctx
            .via(|| async {
                filter_incompatible(
                    TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        targets,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(this.ctx.async_ctx.0)
                    .await?
                    .into_iter(),
                    this.ctx,
                )?
                .inputs()
            })
            .map(StarlarkFileSet::from)
    }

    /// The testsof query for listing the tests of the specified targets.
    fn testsof<'v>(
        this: &StarlarkCQueryCtx<'v>,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                get_cquery_env(this.ctx, this.target_platform.dupe())
                    .await?
                    .testsof(&filter_incompatible(
                        TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                            targets,
                            &this.target_platform,
                            this.ctx,
                            eval,
                        )
                        .await?
                        .get(this.ctx.async_ctx.0)
                        .await?
                        .into_iter(),
                        this.ctx,
                    )?)
                    .await
            })
            .map(StarlarkTargetSet::from)
    }

    /// The testsof query for listing the tests of the specified targets. Performs default target platform
    /// resolution under the hood for the tests found.
    fn testsof_with_default_target_platform<'v>(
        this: &StarlarkCQueryCtx<'v>,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                let maybe_compatibles = get_cquery_env(this.ctx, this.target_platform.dupe())
                    .await?
                    .testsof_with_default_target_platform(&filter_incompatible(
                        TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                            targets,
                            &this.target_platform,
                            this.ctx,
                            eval,
                        )
                        .await?
                        .get(this.ctx.async_ctx.0)
                        .await?
                        .into_iter(),
                        this.ctx,
                    )?)
                    .await?;

                filter_incompatible(maybe_compatibles.into_iter(), this.ctx)
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
        universe: Value<'v>,
        from: Value<'v>,
        depth: Option<i32>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                get_cquery_env(this.ctx, this.target_platform.dupe())
                    .await?
                    .rdeps(
                        &filter_incompatible(
                            TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                                universe,
                                &this.target_platform,
                                this.ctx,
                                eval,
                            )
                            .await?
                            .get(this.ctx.async_ctx.0)
                            .await?
                            .into_iter(),
                            this.ctx,
                        )?,
                        &filter_incompatible(
                            TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                                from,
                                &this.target_platform,
                                this.ctx,
                                eval,
                            )
                            .await?
                            .get(this.ctx.async_ctx.0)
                            .await?
                            .into_iter(),
                            this.ctx,
                        )?,
                        depth,
                    )
                    .await
            })
            .map(StarlarkTargetSet::from)
    }

    /// Evaluates some general query string. `query_args` can be a target_set of unconfigured nodes, or
    /// a list of strings.
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
        #[starlark(default = NoneOr::None)] query_args: NoneOr<Value<'v>>,
        #[starlark(default = NoneOr::None)] target_universe: NoneOr<Vec<String>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let query_args = if query_args.is_none() {
            Vec::new()
        } else {
            let unwrapped_query_args = query_args.into_option().unwrap();
            if let Some(query_args) = unpack_unconfigured_query_args(unwrapped_query_args)? {
                query_args
            } else {
                let err = Err(ValueError::IncorrectParameterTypeWithExpected(
                    "list of strings, or a target_set of unconfigured nodes".to_owned(),
                    query_args.into_option().unwrap().get_type().to_owned(),
                )
                .into());

                if <&StarlarkTargetSet<ConfiguredTargetNode>>::unpack_value(unwrapped_query_args)
                    .is_some()
                {
                    return err
                        .context("target_set with configured nodes are currently not supported");
                }

                return err;
            }
        };

        this.ctx.async_ctx.via_dice(|ctx| async {
            parse_query_evaluation_result(
                QUERY_FRONTEND
                    .get()?
                    .eval_cquery(
                        ctx,
                        &this.ctx.working_dir()?,
                        CqueryOwnerBehavior::Deprecated,
                        query,
                        &query_args,
                        this.target_platform.dupe(),
                        target_universe.into_option().as_ref().map(|v| &v[..]),
                    )
                    .await?,
                eval,
            )
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
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkFileSet> {
        this.ctx
            .async_ctx
            .via(|| async {
                let targets = &filter_incompatible(
                    TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        targets,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(this.ctx.async_ctx.0)
                    .await?
                    .into_iter(),
                    this.ctx,
                )?;

                Ok(targets.buildfile())
            })
            .map(StarlarkFileSet::from)
    }
}
