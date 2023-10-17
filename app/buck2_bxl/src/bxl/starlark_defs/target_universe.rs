/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_node::configured_universe::CqueryUniverse;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use futures::FutureExt;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueTyped;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::target_list_expr::TargetListExpr;
use crate::bxl::starlark_defs::target_list_expr::TargetListExprArg;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

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
#[display(fmt = "<universe of {} nodes>", "self.target_universe.len()")]
pub(crate) struct StarlarkTargetUniverse<'v> {
    // Cquery universe for performing
    target_universe: CqueryUniverse,
    // The target universe as the target set
    target_set: TargetSet<ConfiguredTargetNode>,
    // Trace/Allocative are implemented for BxlContext, but we take a reference here.
    // This is used in unpacking target expressions for lookups.
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    ctx: &'v BxlContext<'v>,
}

#[starlark_value(type = "target_universe", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkTargetUniverse<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(target_universe_methods)
    }
}

impl<'v> AllocValue<'v> for StarlarkTargetUniverse<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTargetUniverse<'v> {
    pub(crate) async fn new(
        ctx: &'v BxlContext<'v>,
        target_set: TargetSet<ConfiguredTargetNode>,
    ) -> anyhow::Result<StarlarkTargetUniverse<'v>> {
        let target_universe = CqueryUniverse::build(&target_set).await?;
        let target_set = target_universe
            .get_from_targets(target_set.iter().map(|i| i.label().unconfigured().dupe()));
        Ok(StarlarkTargetUniverse {
            ctx,
            target_universe,
            target_set,
        })
    }
}

/// Target universe in BXL. Used for looking up valid configured targets to use in cquery. This is not needed for uquery.
#[starlark_module]
fn target_universe_methods(builder: &mut MethodsBuilder) {
    // The target set of the target universe.
    fn target_set<'v>(
        this: &'v StarlarkTargetUniverse<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkTargetSet<ConfiguredTargetNode>>> {
        Ok(heap.alloc_typed(StarlarkTargetSet::from(this.target_set.clone())))
    }

    // Looks up valid configured target nodes within the universe. The targets passed in are either string literals,
    // unconfigured target nodes, or unconfigured target labels.
    fn lookup<'v>(
        this: &'v StarlarkTargetUniverse<'v>,
        targets: TargetListExprArg<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ValueTyped<'v, StarlarkTargetSet<ConfiguredTargetNode>>> {
        this.ctx.via_dice(|mut dice, ctx| {
            dice.via(|dice| {
                async move {
                    let inputs = &*TargetListExpr::<'v, TargetNode>::unpack(targets, ctx, dice)
                        .await?
                        .get(dice)
                        .await?;

                    let result = this
                        .target_universe
                        .get_from_targets(inputs.iter().map(|i| i.label().dupe()));

                    Ok(eval.heap().alloc_typed(StarlarkTargetSet::from(result)))
                }
                .boxed_local()
            })
        })
    }
}
