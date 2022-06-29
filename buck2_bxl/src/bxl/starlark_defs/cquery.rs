use std::sync::Arc;

use buck2_build_api::query::cquery::environment::CqueryEnvironment;
use buck2_build_api::query::cquery::evaluator::get_cquery_evaluator;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::target::TargetLabel;
use buck2_docs_gen::Buck2Docs;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationResult;
use buck2_query::query::syntax::simple::eval::values::QueryEvaluationValue;
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctions;
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctionsModule;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::dict::Dict;
use starlark::values::none::NoneOr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::file_set::FileSetExpr;
use crate::bxl::starlark_defs::file_set::StarlarkFileSet;
use crate::bxl::starlark_defs::target_expr::TargetExpr;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::value_as_starlak_target_label::ValueAsStarlarkTargetLabel;

/// The context for performing `cquery` operations in bxl. The functions offered on this ctx are
/// the same behaviour as the query functions available within cquery command.
#[derive(ProvidesStaticType, Derivative, Display, Trace, NoSerialize, Buck2Docs)]
#[buck2_docs(register_cquery, name = "cqueryctx")]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
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
    #[trace(unsafe_ignore)]
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

impl<'v> UnpackValue<'v> for &'v StarlarkCQueryCtx<'v> {
    fn expected() -> String {
        StarlarkCQueryCtx::get_type_value_static()
            .as_str()
            .to_owned()
    }

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

#[starlark_module]
fn register_cquery(builder: &mut MethodsBuilder) {
    /// the `allpaths` query.
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
                    &*TargetExpr::unpack(from, &this.target_platform, this.ctx, &this.env, eval)
                        .await?
                        .get(&this.env)
                        .await?,
                    &*TargetExpr::unpack(to, &this.target_platform, this.ctx, &this.env, eval)
                        .await?
                        .get(&this.env)
                        .await?,
                )
                .await
                .map(StarlarkTargetSet::from)
        })?)
    }

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
                    &*TargetExpr::unpack(targets, &this.target_platform, this.ctx, &this.env, eval)
                        .await?
                        .get(&this.env)
                        .await?,
                )
                .map(StarlarkTargetSet::from)
        })
    }

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
                    &*TargetExpr::unpack(targets, &this.target_platform, this.ctx, &this.env, eval)
                        .await?
                        .get(&this.env)
                        .await?,
                )
                .map(StarlarkTargetSet::from)
        })
    }

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
                    &*TargetExpr::unpack(targets, &this.target_platform, this.ctx, &this.env, eval)
                        .await?
                        .get(&this.env)
                        .await?,
                )
                .map(StarlarkTargetSet::from)
        })
    }

    fn owner<'v>(
        this: &StarlarkCQueryCtx,
        files: FileSetExpr,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                this.functions
                    .owner(&this.env, &*(files.get(&this.env).await?))
                    .await
            })
            .map(StarlarkTargetSet::from)
    }

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
                    .try_map(|v| buck2_query_parser::parse_expr(*v))?;

                this.functions
                    .deps(
                        &this.env,
                        &DefaultQueryFunctionsModule::new(),
                        &*TargetExpr::unpack(
                            universe,
                            &this.target_platform,
                            this.ctx,
                            &this.env,
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

    pub fn filter<'v>(
        this: &StarlarkCQueryCtx<'v>,
        regex: &str,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                this.functions.filter(
                    regex,
                    &*TargetExpr::unpack(targets, &this.target_platform, this.ctx, &this.env, eval)
                        .await?
                        .get(&this.env)
                        .await?,
                )
            })
            .map(StarlarkTargetSet::from)
    }

    pub fn inputs<'v>(
        this: &StarlarkCQueryCtx<'v>,
        targets: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkFileSet> {
        this.ctx
            .async_ctx
            .via(|| async {
                this.functions.inputs(
                    &*TargetExpr::unpack(targets, &this.target_platform, this.ctx, &this.env, eval)
                        .await?
                        .get(&this.env)
                        .await?,
                )
            })
            .map(StarlarkFileSet::from)
    }

    pub fn testsof<'v>(
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
                        &*TargetExpr::unpack(
                            targets,
                            &this.target_platform,
                            this.ctx,
                            &this.env,
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
                        &*TargetExpr::unpack(
                            universe,
                            &this.target_platform,
                            this.ctx,
                            &this.env,
                            eval,
                        )
                        .await?
                        .get(&this.env)
                        .await?,
                        &*TargetExpr::unpack(
                            from,
                            &this.target_platform,
                            this.ctx,
                            &this.env,
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

    /// evaluates some general query string
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
                ctx.global_data().get_io_provider().fs().root.clone(),
                this.target_platform.dupe(),
            )
            .await
            {
                Ok(evaluator) => Ok(
                    match evaluator
                        .eval_query(
                            query,
                            &query_args,
                            target_universe.into_option().as_ref().map(|v| &v[..]),
                        )
                        .await?
                    {
                        QueryEvaluationResult::Single(result) => match result {
                            QueryEvaluationValue::TargetSet(targets) => {
                                eval.heap().alloc(StarlarkTargetSet::from(targets))
                            }
                            QueryEvaluationValue::FileSet(files) => {
                                eval.heap().alloc(StarlarkFileSet::from(files))
                            }
                        },
                        QueryEvaluationResult::Multiple(multi) => eval.heap().alloc(Dict::new(
                            multi
                                .0
                                .into_iter()
                                .map(|(q, res)| {
                                    Ok((
                                        eval.heap().alloc(q).get_hashed()?,
                                        match res? {
                                            QueryEvaluationValue::TargetSet(targets) => {
                                                eval.heap().alloc(StarlarkTargetSet::from(targets))
                                            }
                                            QueryEvaluationValue::FileSet(files) => {
                                                eval.heap().alloc(StarlarkFileSet::from(files))
                                            }
                                        },
                                    ))
                                })
                                .collect::<anyhow::Result<_>>()?,
                        )),
                    },
                ),
                Err(e) => Err(e),
            }
        })
    }
}
