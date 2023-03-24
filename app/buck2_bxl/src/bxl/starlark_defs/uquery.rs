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
use buck2_build_api::query::dice::DiceQueryDelegate;
use buck2_build_api::query::uquery::environment::UqueryEnvironment;
use buck2_build_api::query::uquery::evaluator::get_uquery_evaluator;
use buck2_common::dice::cells::HasCellResolver;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctions;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_type;
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
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    functions: DefaultQueryFunctions<UqueryEnvironment<'v>>,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    env: UqueryEnvironment<'v>,
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
) -> anyhow::Result<UqueryEnvironment<'v>> {
    let dice_query_delegate = ctx.dice_query_delegate(None).await?;
    let uquery_delegate = Arc::new(dice_query_delegate);
    Ok(UqueryEnvironment::new(
        uquery_delegate.dupe(),
        uquery_delegate,
    ))
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
    pub fn new(
        ctx: &'v BxlContext<'v>,
        cquery_delegate: Arc<DiceQueryDelegate<'v>>,
    ) -> anyhow::Result<Self> {
        let env = UqueryEnvironment::new(cquery_delegate.dupe(), cquery_delegate);
        Ok(Self {
            ctx,
            functions: DefaultQueryFunctions::new(),
            env,
        })
    }
}

/// The context for performing `uquery` operations in bxl. The functions offered on this ctx are
/// the same behaviour as the query functions available within uquery command.
#[starlark_module]
fn register_uquery(builder: &mut MethodsBuilder) {
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
            .via(|| async {
                this.functions.filter_target_set(
                    regex,
                    &*TargetExpr::<'v, TargetNode>::unpack(targets, this.ctx, eval)
                        .await?
                        .get(&this.env)
                        .await?,
                )
            })
            .map(StarlarkTargetSet::from)
    }

    /// The testsof query for lising the tests of the specified targets.
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
            .via(|| async {
                this.functions
                    .testsof(
                        &this.env,
                        &*TargetExpr::<'v, TargetNode>::unpack(targets, this.ctx, eval)
                            .await?
                            .get(&this.env)
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
            .via(|| async {
                let targets = &*TargetExpr::<'v, TargetNode>::unpack(targets, this.ctx, eval)
                    .await?
                    .get(&this.env)
                    .await?;

                Ok(this.functions.buildfile(targets))
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
                this.functions
                    .owner(&this.env, (files.get(&this.env).await?).as_ref())
                    .await
            })
            .map(StarlarkTargetSet::from)
    }

    /// Evaluates some general query string
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_eval(ctx):
    ///     result = ctx.uquery().eval("inputs(cell//path/to/file:target)")
    ///     ctx.output.print(result)
    /// ```
    fn eval<'v>(
        this: &StarlarkUQueryCtx<'v>,
        query: &'v str,
        #[starlark(default = Vec::new())] query_args: Vec<String>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        this.ctx.async_ctx.via_dice(|ctx| async {
            match get_uquery_evaluator(
                ctx,
                ctx.get_cell_resolver()
                    .await?
                    .get(this.ctx.current_bxl.label().bxl_path.cell())?
                    .path(),
                None,
            )
            .await
            {
                Ok(evaluator) => parse_query_evaluation_result::<UqueryEnvironment>(
                    evaluator.eval_query(query, &query_args).await?,
                    eval,
                ),
                Err(e) => Err(e),
            }
        })
    }
}
