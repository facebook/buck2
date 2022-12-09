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
use buck2_query::query::syntax::simple::functions::DefaultQueryFunctions;
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
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::query_util::parse_query_evaluation_result;

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative
)]
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
    ctx: &'v DiceComputations,
) -> anyhow::Result<UqueryEnvironment<'v>> {
    let dice_query_delegate = BxlContext::dice_query_delegate(ctx, None).await?;
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

#[starlark_module]
fn register_uquery(builder: &mut MethodsBuilder) {
    /// Evaluates some general query string
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
