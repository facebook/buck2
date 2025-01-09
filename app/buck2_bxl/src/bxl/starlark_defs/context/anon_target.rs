/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use buck2_analysis::analysis::calculation::get_loaded_module;
use buck2_analysis::analysis::env::get_rule_impl;
use buck2_analysis::analysis::env::promise_artifact_mappings;
use buck2_analysis::analysis::env::transitive_validations;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::anon_target::AnonTargetDependentAnalysisResults;
use buck2_build_api::anon_target::AnonTargetDyn;
use buck2_build_api::bxl::anon_target::EVAL_BXL_FOR_ANON_TARGET;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_build_api::interpreter::rule_defs::provider::collection::ProviderCollection;
use buck2_build_api::interpreter::rule_defs::provider::ty::abstract_provider::AbstractProvider;
use buck2_common::events::HasEvents;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_error::conversion::from_any;
use buck2_error::BuckErrorContext;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_futures::cancellation::CancellationObserver;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::soft_error::Buck2StarlarkSoftErrorHandler;
use buck2_interpreter::starlark_profiler::profiler::StarlarkProfilerOpt;
use buck2_interpreter_for_build::attrs::StarlarkAttribute;
use buck2_interpreter_for_build::rule::RuleCallable;
use buck2_node::bzl_or_bxl_path::BzlOrBxlPath;
use dice::DiceComputations;
use dupe::Dupe;
use futures::FutureExt;
use starlark::collections::SmallMap;
use starlark::environment::FrozenModule;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::typing::ParamIsRequired;
use starlark::typing::ParamSpec;
use starlark::util::ArcStr;
use starlark::values::dict::UnpackDictEntries;
use starlark::values::list::ListType;
use starlark::values::list::UnpackList;
use starlark::values::structs::StructRef;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::typing::StarlarkCallable;
use starlark::values::typing::StarlarkCallableChecked;
use starlark::values::typing::StarlarkCallableParamSpec;
use starlark::values::FrozenValue;
use starlark::values::StringValue;
use starlark::values::Value;
use starlark::values::ValueOfUncheckedGeneric;
use starlark::values::ValueTyped;
use starlark::values::ValueTypedComplex;
use starlark_map::ordered_map::OrderedMap;

use crate::bxl::key::BxlKey;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::context::BxlContextCoreData;
use crate::bxl::starlark_defs::context::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::eval_extra::BxlEvalExtra;

struct BxlAnonCallbackParamSpec;

impl StarlarkCallableParamSpec for BxlAnonCallbackParamSpec {
    fn params() -> ParamSpec {
        ParamSpec::new_parts(
            [],
            [],
            None,
            [
                (
                    ArcStr::new_static("bxl_ctx"),
                    ParamIsRequired::Yes,
                    BxlContext::starlark_type_repr(),
                ),
                (
                    ArcStr::new_static("attrs"),
                    ParamIsRequired::Yes,
                    StructRef::starlark_type_repr(),
                ),
            ],
            None,
        )
        .unwrap()
    }
}

#[starlark_module]
pub(crate) fn register_anon_rule(globals: &mut GlobalsBuilder) {
    /// Create a new anonymous rule.
    fn anon_rule<'v>(
        #[starlark(require = named)] r#impl: StarlarkCallableChecked<
            'v,
            BxlAnonCallbackParamSpec,
            ListType<AbstractProvider>,
        >,
        #[starlark(require = named)] attrs: UnpackDictEntries<&'v str, &'v StarlarkAttribute>,
        #[starlark(require = named, default = "")] doc: &str,
        #[starlark(require = named, default = SmallMap::default())]
        artifact_promise_mappings: SmallMap<
            StringValue<'v>,
            StarlarkCallable<'v, (FrozenValue,), UnpackList<FrozenValue>>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<RuleCallable<'v>> {
        RuleCallable::new_bxl_anon(
            StarlarkCallable::unchecked_new(r#impl.0),
            attrs,
            doc,
            artifact_promise_mappings,
            eval,
        )
        .map_err(Into::into)
    }
}

#[derive(buck2_error::Error, Debug)]
enum BxlAnonTargetError {
    #[error("Anon BXL is not supported in bzl files")]
    UnsupportedInBzl,
}

/// `RuleSpec` equivalent for `bxl.anon_target`.
struct AnonImpl {
    module: FrozenModule,
    name: String,
}

impl AnonImpl {
    async fn new(
        dice: &mut DiceComputations<'_>,
        anon_target: Arc<dyn AnonTargetDyn>,
    ) -> buck2_error::Result<Self> {
        let rule_type = anon_target.rule_type();
        let module = get_loaded_module(dice, rule_type).await?;
        Ok(Self {
            module: module.env().dupe(),
            name: rule_type.name.clone(),
        })
    }

    fn invoke<'v>(
        &self,
        eval: &mut Evaluator<'v, '_, '_>,
        bxl_ctx: ValueTyped<'v, BxlContext<'v>>,
        attrs: ValueOfUncheckedGeneric<Value<'v>, StructRef<'static>>,
    ) -> buck2_error::Result<Value<'v>> {
        let anon_impl = get_rule_impl(eval, &self.module, &self.name)?;
        eval.eval_function(
            anon_impl.to_value(),
            &[bxl_ctx.to_value(), attrs.get()],
            &[],
        )
        .map_err(|e| e.into())
    }

    fn promise_artifact_mappings<'v>(
        &self,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<SmallMap<String, Value<'v>>> {
        promise_artifact_mappings(eval, &self.module, &self.name)
    }
}

async fn eval_bxl_for_anon_target(
    dice: &mut DiceComputations<'_>,
    anon_target: Arc<dyn AnonTargetDyn>,
    global_cfg_options: GlobalCfgOptions,
    dependents_analyses: AnonTargetDependentAnalysisResults<'_>,
    execution_platform: ExecutionPlatformResolution,
    liveness: CancellationObserver,
) -> buck2_error::Result<AnalysisResult> {
    let env = Module::new();

    let rule_type = anon_target.rule_type();
    let bxl_spec = match &rule_type.path {
        BzlOrBxlPath::Bxl(bxl_file_path) => BxlFunctionLabel {
            bxl_path: bxl_file_path.clone(),
            name: rule_type.name.clone(),
        },
        BzlOrBxlPath::Bzl(_) => return Err(BxlAnonTargetError::UnsupportedInBzl.into()),
    };
    let bxl_key = BxlKey::new(
        bxl_spec,
        Arc::new(OrderedMap::new()),
        false,
        global_cfg_options,
    );
    let bxl_ctx_core_data = BxlContextCoreData::new(bxl_key.dupe(), dice).await?;

    let dispatcher = dice.per_transaction_data().get_dispatcher().dupe();
    let print = EventDispatcherPrintHandler(dispatcher);
    let digest_config = dice.global_data().get_digest_config();

    let validations_from_deps = dependents_analyses.validations();

    let anon_impl = AnonImpl::new(dice, anon_target.dupe()).await?;

    let (analysis_registry, fulfilled_artifact_mappings) = with_starlark_eval_provider(
        dice,
        &mut StarlarkProfilerOpt::disabled(),
        format!("bxl_anon_analysis:{}", anon_target),
        |provider, dice| {
            let bxl_dice = Rc::new(RefCell::new(BxlSafeDiceComputations::new(dice, liveness)));

            let bxl_ctx_core_data = Rc::new(bxl_ctx_core_data);

            let extra = BxlEvalExtra::new_anon(bxl_dice.dupe(), bxl_ctx_core_data.dupe());
            let (mut eval, _) = provider.make(&env)?;

            eval.set_print_handler(&print);
            eval.set_soft_error_handler(&Buck2StarlarkSoftErrorHandler);
            eval.extra = Some(&extra);

            let analysis_registry = AnalysisRegistry::new_from_owner(
                anon_target.dupe().base_deferred_key(),
                execution_platform.clone(),
            )?;

            let attributes =
                anon_target.resolve_attrs(&env, dependents_analyses, execution_platform.clone())?;

            let bxl_anon_ctx = BxlContext::new_anon(
                env.heap(),
                bxl_ctx_core_data,
                bxl_dice,
                digest_config,
                analysis_registry,
                attributes,
            )?;
            let bxl_ctx = ValueTyped::<BxlContext>::new_err(env.heap().alloc(bxl_anon_ctx))?;

            let action_factory = bxl_ctx.state;

            let list_res = tokio::task::block_in_place(|| -> anyhow::Result<Value<'_>> {
                let invoke_res = anon_impl.invoke(&mut eval, bxl_ctx, attributes)?;
                bxl_ctx.via_dice(|dice, _| {
                    dice.via(|dice| {
                        action_factory
                            .run_promises(
                                dice,
                                &mut eval,
                                format!("bxl_anon_analysis$promise:{}", anon_target),
                            )
                            .boxed_local()
                    })
                })?;
                Ok(invoke_res)
            })
            .map_err(from_any)?;

            let res_typed = ProviderCollection::try_from_value(list_res)?;
            let res = env.heap().alloc(res_typed);

            let fulfilled_artifact_mappings = {
                let promise_artifact_mappings = anon_impl.promise_artifact_mappings(&mut eval)?;

                anon_target.dupe().get_fulfilled_promise_artifacts(
                    promise_artifact_mappings,
                    res,
                    &mut eval,
                )?
            };

            let res = ValueTypedComplex::new(res)
                .internal_error("Just allocated the provider collection")?;

            let analysis_registry = bxl_ctx.take_state_anon()?;
            analysis_registry
                .analysis_value_storage
                .set_result_value(res)?;

            Ok((analysis_registry, fulfilled_artifact_mappings))
        },
    )
    .await?;

    let num_declared_actions = analysis_registry.num_declared_actions();
    let num_declared_artifacts = analysis_registry.num_declared_artifacts();
    let (_frozen_env, recorded_values) = analysis_registry.finalize(&env)?(env)?;

    let validations = transitive_validations(
        validations_from_deps,
        recorded_values.provider_collection()?,
    );

    Ok(AnalysisResult::new(
        recorded_values,
        None,
        fulfilled_artifact_mappings,
        num_declared_actions,
        num_declared_artifacts,
        validations,
    ))
}

pub(crate) fn init_eval_bxl_for_anon_target() {
    EVAL_BXL_FOR_ANON_TARGET.init(
        |dice,
         anon_target: Arc<dyn AnonTargetDyn>,
         global_cfg_options: GlobalCfgOptions,
         dependents_analyses: AnonTargetDependentAnalysisResults<'_>,
         execution_platform,
         liveness| {
            Box::pin(eval_bxl_for_anon_target(
                dice,
                anon_target,
                global_cfg_options,
                dependents_analyses,
                execution_platform,
                liveness,
            ))
        },
    )
}
