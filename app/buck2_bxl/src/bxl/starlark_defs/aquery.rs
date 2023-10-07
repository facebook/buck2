/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::query::bxl::BxlAqueryFunctions;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use starlark::any::ProvidesStaticType;
use starlark::eval::Evaluator;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::StarlarkDocs;
use thiserror::Error;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::context::BxlContextNoDice;
use crate::bxl::starlark_defs::providers_expr::ProvidersExpr;
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
pub(crate) struct StarlarkAQueryCtx<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    ctx: &'v BxlContext<'v>,
    #[derivative(Debug = "ignore")]
    target_platform: Option<TargetLabel>,
}

#[starlark_value(type = "aqueryctx", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkAQueryCtx<'v> {}

impl<'v> AllocValue<'v> for StarlarkAQueryCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkAQueryCtx<'v> {
    pub(crate) fn new(
        ctx: &'v BxlContext<'v>,
        global_target_platform: Value<'v>,
        default_target_platform: &Option<TargetLabel>,
    ) -> anyhow::Result<StarlarkAQueryCtx<'v>> {
        let target_platform = global_target_platform.parse_target_platforms(
            &ctx.data.target_alias_resolver,
            &ctx.data.cell_resolver,
            ctx.data.cell_name,
            default_target_platform,
        )?;

        Ok(Self {
            ctx,
            target_platform,
        })
    }
}

#[derive(Debug, Error)]
pub(crate) enum BxlAqueryError {
    #[error(
        "Expected a list of target-like or provider-like items, or a target set of action query nodes, but was `{0}`"
    )]
    InvalidInputs(String),
}

// Aquery operates on `ActionQueryNode`s. Under the hood, the target set of action query nodes is obtained
// by running analysis on a configured providers label. We can accept either a `TargetExpr`, `ProvidersExpr`,
// or a `TargetSet<ActionQueryNode>` (which would have been produced from a previous aquery). For `TargetExpr`
// and `ProvidersExpr`, we need to pass the aquery delegate a list of configured providers labels, and it will
// run analysis on them to construct the `ActionQueryNode`s.
async fn unpack_action_nodes<'v>(
    expr: Value<'v>,
    target_platform: &Option<TargetLabel>,
    ctx: &BxlContextNoDice<'v>,
    dice: &mut DiceComputations,
    aquery_env: &dyn BxlAqueryFunctions,
    eval: &mut Evaluator<'v, '_>,
) -> anyhow::Result<TargetSet<ActionQueryNode>> {
    if let Some(action_nodes) = expr.downcast_ref::<StarlarkTargetSet<ActionQueryNode>>() {
        return Ok(action_nodes.0.clone());
    }

    let providers = if let Some(providers) = ProvidersExpr::<ConfiguredProvidersLabel>::unpack_opt(
        expr,
        target_platform.clone(),
        ctx,
        dice,
        eval,
    )
    .await?
    {
        providers.labels().cloned().collect()
    } else if let Some(targets) =
        TargetExpr::<ConfiguredTargetNode>::unpack_opt(expr, target_platform, ctx, dice, eval, true)
            .await?
    {
        targets.as_provider_labels()
    } else {
        return Err(anyhow::anyhow!(BxlAqueryError::InvalidInputs(
            expr.to_repr()
        )));
    };

    let result = aquery_env.get_target_set(dice, providers).await?;
    Ok(result)
}
