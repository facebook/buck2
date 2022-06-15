use std::sync::Arc;

use buck2_build_api::{
    nodes::configured::ConfiguredTargetNode, query::cquery::environment::CqueryEnvironment,
};
use buck2_core::target::TargetLabel;
use buck2_query::query::syntax::simple::functions::{
    helpers::CapturedExpr, DefaultQueryFunctions, DefaultQueryFunctionsModule,
};
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use gazebo::{any::ProvidesStaticType, prelude::*};
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    eval::Evaluator,
    starlark_module, starlark_type,
    values::{
        none::NoneOr, AllocValue, Heap, NoSerialize, StarlarkValue, Trace, UnpackValue, Value,
        ValueLike,
    },
};

use crate::bxl::{
    starlark_defs::{
        context::BxlContext,
        file_set::{FileSetExpr, StarlarkFileSet},
        target_expr::TargetExpr,
        targetset::StarlarkTargetSet,
    },
    value_as_starlak_target_label::ValueAsStarlarkTargetLabel,
};

/// The context for performing `cquery` operations in bxl. The functions offered on this ctx are
/// the same behaviour as the query functions available within cquery command.
#[derive(ProvidesStaticType, Derivative, Display, Trace, NoSerialize)]
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
}
