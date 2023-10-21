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
use buck2_build_api::query::bxl::NEW_BXL_AQUERY_FUNCTIONS;
use buck2_build_api::query::oneshot::QUERY_FRONTEND;
use buck2_core::configuration::compatibility::IncompatiblePlatformReason;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_query::query::syntax::simple::eval::set::TargetSetExt;
use buck2_query::query::syntax::simple::functions::helpers::CapturedExpr;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use either::Either;
use futures::FutureExt;
use gazebo::prelude::OptionExt;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::context::BxlContextNoDice;
use crate::bxl::starlark_defs::providers_expr::ConfiguredProvidersExprArg;
use crate::bxl::starlark_defs::providers_expr::ProvidersExpr;
use crate::bxl::starlark_defs::query_util::parse_query_evaluation_result;
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
pub(crate) struct StarlarkAQueryCtx<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    ctx: &'v BxlContext<'v>,
    #[derivative(Debug = "ignore")]
    target_platform: Option<TargetLabel>,
}

#[starlark_value(type = "aqueryctx", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkAQueryCtx<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(aquery_methods)
    }
}

impl<'v> AllocValue<'v> for StarlarkAQueryCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkAQueryCtx<'v> {
    pub(crate) fn new(
        ctx: &'v BxlContext<'v>,
        global_target_platform: ValueAsStarlarkTargetLabel<'v>,
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

pub(crate) async fn get_aquery_env(
    ctx: &BxlContextNoDice<'_>,
    target_platform: Option<TargetLabel>,
) -> anyhow::Result<Box<dyn BxlAqueryFunctions>> {
    (NEW_BXL_AQUERY_FUNCTIONS.get()?)(
        target_platform,
        ctx.project_root().dupe(),
        ctx.cell_name,
        ctx.cell_resolver.dupe(),
    )
    .await
}

#[derive(StarlarkTypeRepr, UnpackValue)]
enum UnpackActionNodes<'v> {
    ActionQueryNodes(&'v StarlarkTargetSet<ActionQueryNode>),
    ConfiguredProviders(ConfiguredProvidersExprArg<'v>),
    ConfiguredTargets(ConfiguredTargetListExprArg<'v>),
}

// Aquery operates on `ActionQueryNode`s. Under the hood, the target set of action query nodes is obtained
// by running analysis on a configured providers label. We can accept either a `TargetExpr`, `ProvidersExpr`,
// or a `TargetSet<ActionQueryNode>` (which would have been produced from a previous aquery). For `TargetExpr`
// and `ProvidersExpr`, we need to pass the aquery delegate a list of configured providers labels, and it will
// run analysis on them to construct the `ActionQueryNode`s.
async fn unpack_action_nodes<'v>(
    expr: UnpackActionNodes<'v>,
    target_platform: &Option<TargetLabel>,
    ctx: &BxlContextNoDice<'v>,
    dice: &mut DiceComputations,
    aquery_env: &dyn BxlAqueryFunctions,
) -> anyhow::Result<TargetSet<ActionQueryNode>> {
    let providers = match expr {
        UnpackActionNodes::ActionQueryNodes(action_nodes) => return Ok(action_nodes.0.clone()),
        UnpackActionNodes::ConfiguredProviders(arg) => {
            ProvidersExpr::<ConfiguredProvidersLabel>::unpack(
                arg,
                target_platform.clone(),
                ctx,
                dice,
            )
            .await?
            .labels()
            .cloned()
            .collect()
        }
        UnpackActionNodes::ConfiguredTargets(arg) => {
            TargetListExpr::<ConfiguredTargetNode>::unpack_opt(
                arg,
                target_platform,
                ctx,
                dice,
                true,
            )
            .await?
            .as_provider_labels()
        }
    };

    let (incompatible_targets, result) = aquery_env.get_target_set(dice, providers).await?;

    if !incompatible_targets.is_empty() {
        ctx.print_to_error_stream(IncompatiblePlatformReason::skipping_message_for_multiple(
            incompatible_targets.iter(),
        ))?;
    }

    Ok(result)
}

/// The context for performing `aquery` operations in bxl. The functions offered on this ctx are
/// the same behaviour as the query functions available within aquery command.
///
/// Query results are `[StarlarkTargetSet]`s of `[ActionQueryNode]`s, which supports iteration,
/// indexing, `len()`, set addition/subtraction, and `equals()`.
#[starlark_module]
fn aquery_methods(builder: &mut MethodsBuilder) {
    /// The deps query for finding the transitive closure of dependencies.
    fn deps<'v>(
        this: &StarlarkAQueryCtx<'v>,
        // TODO(nga): parameters should be either positional or named, not both.
        universe: UnpackActionNodes<'v>,
        #[starlark(default = NoneOr::None)] depth: NoneOr<i32>,
        #[starlark(default = NoneOr::None)] filter: NoneOr<&'v str>,
    ) -> anyhow::Result<StarlarkTargetSet<ActionQueryNode>> {
        this.ctx
            .via_dice(|mut dice, ctx| {
                dice.via(|dice| {
                    async {
                        let aquery_env = get_aquery_env(ctx, this.target_platform.dupe()).await?;

                        let filter = filter
                            .into_option()
                            .try_map(buck2_query_parser::parse_expr)?;

                        let universe = unpack_action_nodes(
                            universe,
                            &this.target_platform,
                            ctx,
                            dice,
                            aquery_env.as_ref(),
                        )
                        .await?;

                        aquery_env
                            .deps(
                                dice,
                                &universe,
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

    /// Obtain all the actions declared within the analysis of a given target.
    ///
    /// This operation only makes sense on a target literal (it is a simple passthrough when passed
    /// an action).
    fn all_actions<'v>(
        this: &StarlarkAQueryCtx<'v>,
        // TODO(nga): parameters should be either positional or named, not both.
        targets: UnpackActionNodes<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ActionQueryNode>> {
        this.ctx
            .via_dice(|mut dice, ctx| {
                dice.via(|dice| {
                    async {
                        let aquery_env = get_aquery_env(ctx, this.target_platform.dupe()).await?;

                        let targets = unpack_action_nodes(
                            targets,
                            &this.target_platform,
                            ctx,
                            dice,
                            aquery_env.as_ref(),
                        )
                        .await?;

                        get_aquery_env(ctx, this.target_platform.dupe())
                            .await?
                            .all_actions(dice, &targets)
                            .await
                    }
                    .boxed_local()
                })
            })
            .map(StarlarkTargetSet::from)
    }

    /// Obtain the actions for all the outputs provided by the `DefaultInfo` for the targets passed
    /// as input. This includes both the `default_outputs` and `other_outputs`.
    ///
    /// This operation only makes sense on a target literal (it does nothing if passed something
    /// else).
    fn all_outputs<'v>(
        this: &StarlarkAQueryCtx<'v>,
        // TODO(nga): parameters should be either positional or named, not both.
        targets: UnpackActionNodes<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ActionQueryNode>> {
        this.ctx
            .via_dice(|mut dice, ctx| {
                dice.via(|dice| {
                    async {
                        let aquery_env = get_aquery_env(ctx, this.target_platform.dupe()).await?;

                        let targets = unpack_action_nodes(
                            targets,
                            &this.target_platform,
                            ctx,
                            dice,
                            aquery_env.as_ref(),
                        )
                        .await?;

                        get_aquery_env(ctx, this.target_platform.dupe())
                            .await?
                            .all_outputs(dice, &targets)
                            .await
                    }
                    .boxed_local()
                })
            })
            .map(StarlarkTargetSet::from)
    }

    /// The attrfilter query for rule attribute filtering.
    fn attrfilter<'v>(
        this: &StarlarkAQueryCtx<'v>,
        // TODO(nga): parameters should be either positional or named, not both.
        attr: &str,
        value: &str,
        targets: UnpackActionNodes<'v>,
    ) -> anyhow::Result<StarlarkTargetSet<ActionQueryNode>> {
        this.ctx.via_dice(|mut dice, ctx| {
            dice.via(|dice| {
                async {
                    let aquery_env = get_aquery_env(ctx, this.target_platform.dupe()).await?;

                    let targets = unpack_action_nodes(
                        targets,
                        &this.target_platform,
                        ctx,
                        dice,
                        aquery_env.as_ref(),
                    )
                    .await?;

                    targets
                        .attrfilter(attr, &|v| Ok(v == value))
                        .map(StarlarkTargetSet::from)
                }
                .boxed_local()
            })
        })
    }

    /// Evaluates some general query string. `query_args` can be a target_set of unconfigured nodes, or
    /// a list of strings.
    fn eval<'v>(
        this: &StarlarkAQueryCtx<'v>,
        query: &'v str,
        #[starlark(default = NoneOr::None)] query_args: NoneOr<
            Either<UnpackUnconfiguredQueryArgs<'v>, &'v StarlarkTargetSet<ConfiguredTargetNode>>,
        >,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let query_args = match query_args {
            NoneOr::None => Vec::new(),
            NoneOr::Other(Either::Left(query_args)) => query_args.into_strings(),
            NoneOr::Other(Either::Right(_)) => {
                return Err(anyhow::anyhow!(
                    "target_set with configured nodes are currently not supported"
                ));
            }
        };

        this.ctx.via_dice(|mut dice, ctx| {
            dice.via(|dice| {
                async {
                    parse_query_evaluation_result(
                        QUERY_FRONTEND
                            .get()?
                            .eval_aquery(
                                dice,
                                &ctx.working_dir()?,
                                query,
                                &query_args,
                                this.target_platform.dupe(),
                            )
                            .await?,
                        eval,
                    )
                }
                .boxed_local()
            })
        })
    }
}
