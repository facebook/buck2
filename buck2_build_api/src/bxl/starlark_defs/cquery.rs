use std::sync::Arc;

use buck2_core::target::TargetLabel;
use buck2_query::query::{environment::QueryEnvironment, syntax::simple::eval::set::TargetSetExt};
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use gazebo::{any::AnyLifetime, prelude::*};
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{
        AllocValue, Freeze, Freezer, Heap, NoSerialize, NoSimpleValue, StarlarkValue, Trace,
        UnpackValue, Value, ValueLike,
    },
};

use crate::{
    bxl::{
        common::ValueAsStarlarkTargetLabel,
        starlark_defs::{
            context::BxlContext,
            file_set::FileSetExpr,
            target_expr::{targets, TargetExpr},
            BxlError,
        },
        StarlarkTargetSet,
    },
    nodes::configured::ConfiguredTargetNode,
    query::cquery::environment::CqueryEnvironment,
};

#[derive(AnyLifetime, Derivative, Display, Trace, NoSerialize)]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkCQueryCtx<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    ctx: &'v BxlContext<'v>,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    env: CqueryEnvironment<'v>,
}

impl<'v> StarlarkValue<'v> for StarlarkCQueryCtx<'v> {
    starlark_type!("cqueryctx");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_cquery)
    }
}

impl<'v> AllocValue<'v> for StarlarkCQueryCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> Freeze for StarlarkCQueryCtx<'v> {
    type Frozen = NoSimpleValue;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Err(BxlError::NoFreeze("StarlarkCQueryCtx").into())
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
    global_target_platform: Option<TargetLabel>,
) -> anyhow::Result<CqueryEnvironment<'v>> {
    let dice_query_delegate = BxlContext::dice_query_delegate(ctx, global_target_platform).await?;
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
        let global_target_platform =
            global_target_platform.parse_target_platforms(&ctx.target_alias_resolver, &ctx.cell)?;

        let env = get_cquery_env(ctx.async_ctx.0, global_target_platform).await?;
        Ok(Self { ctx, env })
    }
}

#[starlark_module]
fn register_cquery(builder: &mut MethodsBuilder) {
    fn kind(
        this: &StarlarkCQueryCtx,
        regex: &str,
        targets: TargetExpr<ConfiguredTargetNode>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.async_ctx.via(|| async {
            targets!(&this.env, targets)
                .kind(regex)
                .map(StarlarkTargetSet::from)
        })
    }

    fn attrregexfilter(
        this: &StarlarkCQueryCtx,
        attribute: &str,
        value: &str,
        targets: TargetExpr<ConfiguredTargetNode>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx.async_ctx.via(|| async {
            targets!(&this.env, targets)
                .attrregexfilter(attribute, value)
                .map(StarlarkTargetSet::from)
        })
    }

    fn owner(
        this: &StarlarkCQueryCtx,
        files: FileSetExpr,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async { this.env.owner(&*(files.get(&this.env).await?)).await })
            .map(StarlarkTargetSet::from)
    }

    fn rdeps(
        this: &StarlarkCQueryCtx,
        universe: TargetExpr<ConfiguredTargetNode>,
        from: TargetExpr<ConfiguredTargetNode>,
        depth: Option<i32>,
    ) -> anyhow::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        this.ctx
            .async_ctx
            .via(|| async {
                this.env
                    .rdeps(
                        targets!(&this.env, universe),
                        targets!(&this.env, from),
                        depth,
                    )
                    .await
            })
            .map(StarlarkTargetSet::from)
    }
}
