/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use buck2_action_impl::dynamic::bxl::EVAL_BXL_FOR_DYNAMIC_OUTPUT;
use buck2_action_impl::dynamic::deferred::dynamic_lambda_ctx_data;
use buck2_action_impl::dynamic::deferred::invoke_dynamic_output_lambda;
use buck2_action_impl::dynamic::deferred::DynamicLambdaArgs;
use buck2_action_impl::dynamic::deferred::DynamicLambdaCtxDataSpec;
use buck2_action_impl::dynamic::deferred::InputArtifactsMaterialized;
use buck2_action_impl::dynamic::params::FrozenDynamicLambdaParams;
use buck2_artifact::dynamic::DynamicLambdaResultsKey;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::analysis::registry::RecordedAnalysisValues;
use buck2_build_api::dynamic_value::DynamicValue;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_common::events::HasEvents;
use buck2_common::scope::scope_and_collect_with_dice;
use buck2_core::base_deferred_key::BaseDeferredKeyBxl;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_error::internal_error_anyhow;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_futures::cancellable_future::CancellationObserver;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::factory::StarlarkEvaluatorProvider;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::soft_error::Buck2StarlarkSoftErrorHandler;
use buck2_interpreter::starlark_profiler::profiler::StarlarkProfilerOpt;
use dice::DiceComputations;
use dupe::Dupe;
use itertools::Itertools;
use starlark::environment::Module;
use starlark::values::OwnedRefFrozenRef;
use starlark::values::ValueTyped;

use crate::bxl::key::BxlDynamicKey;
use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::context::BxlContextCoreData;
use crate::bxl::starlark_defs::context::DynamicBxlContextData;
use crate::bxl::starlark_defs::eval_extra::BxlEvalExtra;

pub(crate) async fn eval_bxl_for_dynamic_output<'v>(
    base_deferred_key: &'v BaseDeferredKeyBxl,
    self_key: DynamicLambdaResultsKey,
    dynamic_lambda: OwnedRefFrozenRef<'v, FrozenDynamicLambdaParams>,
    dice_ctx: &'v mut DiceComputations<'_>,
    action_key: String,
    input_artifacts_materialized: InputArtifactsMaterialized,
    resolved_dynamic_values: HashMap<DynamicValue, FrozenProviderCollectionValue>,
    _digest_config: DigestConfig,
    liveness: CancellationObserver,
) -> anyhow::Result<RecordedAnalysisValues> {
    // TODO(wendyy) emit telemetry, support profiler
    let dynamic_key =
        BxlDynamicKey::from_base_deferred_key_dyn_impl_err(base_deferred_key.clone())?;
    let key = dynamic_key.key();
    let dynamic_data = DynamicBxlContextData {
        exec_deps: dynamic_key
            .0
            .execution_resolution
            .exec_deps_configured
            .clone(),
        toolchains: dynamic_key
            .0
            .execution_resolution
            .toolchain_deps_configured
            .clone(),
    };
    // TODO(cjhopman): Why does this get the digest_config from dice???
    let digest_config = dice_ctx.global_data().get_digest_config();
    let dispatcher = dice_ctx.per_transaction_data().get_dispatcher().dupe();
    let artifact_fs = dice_ctx.get_artifact_fs().await?;
    let eval_ctx = BxlDynamicOutputEvaluator {
        data: BxlContextCoreData::new(key, dice_ctx).await?,
        self_key,
        liveness,
        dynamic_lambda,
        dynamic_data,
        digest_config,
        action_key,
        input_artifacts_materialized,
        resolved_dynamic_values,
        artifact_fs,

        print: EventDispatcherPrintHandler(dispatcher.dupe()),
    };

    // Note: because we use `block_in_place`, that will prevent the inner future from being polled
    // and yielded. So, for cancellation observers to work properly within the dice cancellable
    // future context, we need the future that it's attached to the cancellation context can
    // yield and be polled. To ensure that, we have to spawn the future that then enters block_in_place
    let (_, futs) = unsafe {
        // SAFETY: as long as we don't `forget` the return object from `scope_and_collect`, it is safe

        // Additional cancellation notes:
        // the `scope_and_collect` will block on drop, but it will move the blocking to a tokio
        // blocking thread, freeing up the main worker threads. Additionally, the `spawn_cancellable`
        // on the scope will be dropped at the earliest await point. If we are within the blocking
        // section of bxl, the cancellation observer will be notified and cause the blocking calls
        // to terminate.
        scope_and_collect_with_dice(dice_ctx, |dice_ctx, s| {
            s.spawn_cancellable(
                async move {
                    with_starlark_eval_provider(
                        dice_ctx,
                        &mut StarlarkProfilerOpt::disabled(),
                        format!("bxl_dynamic:{}", "foo"),
                        move |provider, dice_ctx| {
                            tokio::task::block_in_place(|| eval_ctx.do_eval(provider, dice_ctx))
                        },
                    )
                    .await
                },
                || Err(anyhow::anyhow!("cancelled")),
            )
        })
    }
    .await;

    match futs.into_iter().exactly_one() {
        Ok(res) => res?,
        Err(_) => panic!("only spawned one task"),
    }
}

struct BxlDynamicOutputEvaluator<'f> {
    data: BxlContextCoreData,
    self_key: DynamicLambdaResultsKey,
    liveness: CancellationObserver,
    dynamic_lambda: OwnedRefFrozenRef<'f, FrozenDynamicLambdaParams>,
    dynamic_data: DynamicBxlContextData,
    digest_config: DigestConfig,
    action_key: String,
    input_artifacts_materialized: InputArtifactsMaterialized,
    resolved_dynamic_values: HashMap<DynamicValue, FrozenProviderCollectionValue>,
    artifact_fs: ArtifactFs,
    print: EventDispatcherPrintHandler,
}

impl BxlDynamicOutputEvaluator<'_> {
    fn do_eval(
        self,
        provider: &mut dyn StarlarkEvaluatorProvider,
        dice: &mut DiceComputations<'_>,
    ) -> anyhow::Result<RecordedAnalysisValues> {
        let env = Module::new();

        let bxl_dice = Rc::new(RefCell::new(BxlSafeDiceComputations::new(
            dice,
            self.liveness,
        )));

        let analysis_registry = {
            let data = Rc::new(self.data);
            let extra = BxlEvalExtra::new_dynamic(bxl_dice.dupe(), data.dupe());
            let (mut eval, _) = provider.make(&env)?;
            eval.set_print_handler(&self.print);
            eval.set_soft_error_handler(&Buck2StarlarkSoftErrorHandler);
            eval.extra = Some(&extra);

            let dynamic_lambda_ctx_data = dynamic_lambda_ctx_data(
                self.dynamic_lambda,
                self.self_key.dupe(),
                &self.action_key,
                self.input_artifacts_materialized,
                &self.resolved_dynamic_values,
                &self.artifact_fs,
                self.digest_config,
                &env,
            )?;

            let bxl_dynamic_ctx = BxlContext::new_dynamic(
                env.heap(),
                data,
                bxl_dice,
                self.digest_config,
                dynamic_lambda_ctx_data.registry,
                self.dynamic_data,
            )?;

            let ctx = ValueTyped::<BxlContext>::new_err(env.heap().alloc(bxl_dynamic_ctx))?;

            let args = match (
                &dynamic_lambda_ctx_data.lambda.attr_values,
                &dynamic_lambda_ctx_data.spec,
            ) {
                (
                    None,
                    DynamicLambdaCtxDataSpec::Old {
                        outputs,
                        artifact_values,
                    },
                ) => DynamicLambdaArgs::OldPositional {
                    ctx: ctx.to_value(),
                    artifact_values: *artifact_values,
                    outputs: *outputs,
                },
                (Some(_arg), DynamicLambdaCtxDataSpec::New { .. }) => {
                    return Err(anyhow::anyhow!(
                        "New `dynamic_actions` API is not implemented for BXL"
                    ));
                }
                (None, DynamicLambdaCtxDataSpec::New { .. })
                | (Some(_), DynamicLambdaCtxDataSpec::Old { .. }) => {
                    return Err(internal_error_anyhow!("Inconsistent"));
                }
            };

            invoke_dynamic_output_lambda(&mut eval, dynamic_lambda_ctx_data.lambda.lambda(), args)?;

            ctx.take_state_dynamic()?
        };

        let (_frozen_env, recorded_values) = analysis_registry.finalize(&env)?(env)?;
        Ok(recorded_values)
    }
}

pub(crate) fn init_eval_bxl_for_dynamic_output() {
    EVAL_BXL_FOR_DYNAMIC_OUTPUT.init(
        |base_deferred_key,
         self_key,
         dynamic_lambda,
         dice_ctx,
         action_key,
         input_artifacts_materialized,
         resolved_dynamic_values,
         digest_config,
         liveness| {
            Box::pin(eval_bxl_for_dynamic_output(
                base_deferred_key,
                self_key,
                dynamic_lambda,
                dice_ctx,
                action_key,
                input_artifacts_materialized,
                resolved_dynamic_values,
                digest_config,
                liveness,
            ))
        },
    );
}
