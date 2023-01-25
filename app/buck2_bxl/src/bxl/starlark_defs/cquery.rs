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
use buck2_build_api::query::cquery::environment::CqueryEnvironment;
use buck2_build_api::query::cquery::environment::CqueryOwnerBehavior;
use buck2_build_api::query::cquery::evaluator::get_cquery_evaluator;
use buck2_common::dice::cells::HasCellResolver;
use buck2_core::target::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctions;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
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
use starlark::values::ValueLike;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::file_set::FileSetExpr;
use crate::bxl::starlark_defs::file_set::StarlarkFileSet;
use crate::bxl::starlark_defs::query_util::parse_query_evaluation_result;
use crate::bxl::starlark_defs::target_expr::TargetExpr;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
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
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    functions: DefaultQueryFunctions<CqueryEnvironment<'v>>,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    env: CqueryEnvironment<'v>,
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
    ctx: &'v DiceComputations,
    target_platform: Option<TargetLabel>,
) -> anyhow::Result<CqueryEnvironment<'v>> {
    let dice_query_delegate = BxlContext::dice_query_delegate(ctx, target_platform).await?;
    let cquery_delegate = Arc::new(dice_query_delegate);
    Ok(CqueryEnvironment::new(
        cquery_delegate.dupe(),
        cquery_delegate,
        // TODO(nga): add universe.
        None,
        CqueryOwnerBehavior::Deprecated,
    ))
}

impl<'v> StarlarkCQueryCtx<'v> {
    pub async fn new(
        ctx: &'v BxlContext<'v>,
        global_target_platform: Value<'v>,
    ) -> anyhow::Result<StarlarkCQueryCtx<'v>> {
        let target_platform =
            global_target_platform.parse_target_platforms(&ctx.target_alias_resolver, &ctx.cell)?;

        let env = get_cquery_env(ctx.async_ctx.0, target_platform.dupe()).await?;
        Ok(Self {
            ctx,
            functions: DefaultQueryFunctions::new(),
            env,
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
        Ok(this.ctx.async_ctx.via(|| async {
            this.functions
                .allpaths(
                    &this.env,
                    &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        from,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(&this.env)
                    .await?,
                    &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        to,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(&this.env)
                    .await?,
                )
                .await
                .map(StarlarkTargetSet::from)
        })?)
    }

    // The somepaths query.
    fn somepaths<'v>(
        this: &StarlarkCQueryCtx<'v>,
        from: Value<'v>,
        to: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        Ok(this.ctx.async_ctx.via(|| async {
            this.functions
                .somepath(
                    &this.env,
                    &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        from,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(&this.env)
                    .await?,
                    &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        to,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(&this.env)
                    .await?,
                )
                .await
                .map(StarlarkTargetSet::from)
        })?)
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
            this.functions
                .attrfilter(
                    attr,
                    value,
                    &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        targets,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(&this.env)
                    .await?,
                )
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
            this.functions
                .kind(
                    regex,
                    &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        targets,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(&this.env)
                    .await?,
                )
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
            this.functions
                .attrregexfilter(
                    attribute,
                    value,
                    &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        targets,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(&this.env)
                    .await?,
                )
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
                this.functions
                    .owner(&this.env, (files.get(&this.env).await?).as_ref())
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

                this.functions
                    .deps(
                        &this.env,
                        &DefaultQueryFunctionsModule::new(),
                        &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                            universe,
                            &this.target_platform,
                            this.ctx,
                            eval,
                        )
                        .await?
                        .get(&this.env)
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
                this.functions.filter_target_set(
                    regex,
                    &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        targets,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(&this.env)
                    .await?,
                )
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
                this.functions.inputs(
                    &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                        targets,
                        &this.target_platform,
                        this.ctx,
                        eval,
                    )
                    .await?
                    .get(&this.env)
                    .await?,
                )
            })
            .map(StarlarkFileSet::from)
    }

    /// The testsof query for lising the tests of the specified targets.
    fn testsof<'v>(
        this: &StarlarkCQueryCtx<'v>,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                this.functions
                    .testsof(
                        &this.env,
                        &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                            targets,
                            &this.target_platform,
                            this.ctx,
                            eval,
                        )
                        .await?
                        .get(&this.env)
                        .await?,
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
                this.functions
                    .rdeps(
                        &this.env,
                        &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                            universe,
                            &this.target_platform,
                            this.ctx,
                            eval,
                        )
                        .await?
                        .get(&this.env)
                        .await?,
                        &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                            from,
                            &this.target_platform,
                            this.ctx,
                            eval,
                        )
                        .await?
                        .get(&this.env)
                        .await?,
                        depth,
                    )
                    .await
            })
            .map(StarlarkTargetSet::from)
    }

    /// Evaluates some general query string.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_eval(ctx):
    ///     result = ctx.cquery().eval("inputs(root//bin:the_binary)")
    ///     ctx.output.print(result)
    /// ```
    fn eval<'v>(
        this: &StarlarkCQueryCtx<'v>,
        query: &'v str,
        #[starlark(default = Vec::new())] query_args: Vec<&'v str>,
        #[starlark(default = NoneOr::None)] target_universe: NoneOr<Vec<&'v str>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        this.ctx.async_ctx.via_dice(|ctx| async {
            match get_cquery_evaluator(
                ctx,
                ctx.get_cell_resolver()
                    .await?
                    .get(this.ctx.current_bxl.label().bxl_path.cell())?
                    .path(),
                this.target_platform.dupe(),
                CqueryOwnerBehavior::Deprecated,
            )
            .await
            {
                Ok(evaluator) => parse_query_evaluation_result::<CqueryEnvironment>(
                    evaluator
                        .eval_query(
                            query,
                            &query_args,
                            target_universe.into_option().as_ref().map(|v| &v[..]),
                        )
                        .await?,
                    eval,
                ),
                Err(e) => Err(e),
            }
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
                let targets = &*TargetExpr::<'v, ConfiguredTargetNode>::unpack(
                    targets,
                    &this.target_platform,
                    this.ctx,
                    eval,
                )
                .await?
                .get(&this.env)
                .await?;

                Ok(this.functions.buildfile(targets))
            })
            .map(StarlarkFileSet::from)
    }
}
