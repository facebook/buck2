/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;
use std::sync::Arc;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::deferred::key::DeferredHolderKey;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::deferred::types::Deferred;
use buck2_build_api::deferred::types::DeferredCtx;
use buck2_build_api::deferred::types::DeferredInput;
use buck2_build_api::deferred::types::DeferredInputsRef;
use buck2_build_api::deferred::types::DeferredOutput;
use buck2_build_api::deferred::types::DeferredRegistry;
use buck2_build_api::deferred::types::DeferredValue;
use buck2_build_api::dynamic::lambda::DynamicLambda;
use buck2_build_api::dynamic::lambda::DynamicLambdaError;
use buck2_build_api::dynamic::params::FrozenDynamicLambdaParams;
use buck2_build_api::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::context::AnalysisContext;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::span_async;
use buck2_execute::digest_config::DigestConfig;
use buck2_interpreter::error::BuckStarlarkError;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::soft_error::Buck2StarlarkSoftErrorHandler;
use dice::DiceComputations;
use dupe::Dupe;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::dict::AllocDict;
use starlark::values::type_repr::DictType;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;

use crate::dynamic::bxl::eval_bxl_for_dynamic_output;

pub enum DynamicLambdaArgs<'v> {
    OldPositional {
        ctx: Value<'v>,
        artifacts: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>,
        outputs: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkDeclaredArtifact>>,
    },
    DynamicActionsNamed {
        actions: ValueTyped<'v, AnalysisActions<'v>>,
        artifacts: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>,
        outputs: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkDeclaredArtifact>>,
        arg: Value<'v>,
    },
}

pub fn invoke_dynamic_output_lambda<'v>(
    eval: &mut Evaluator<'v, '_, '_>,
    lambda: Value<'v>,
    args: DynamicLambdaArgs<'v>,
) -> anyhow::Result<()> {
    let pos;
    let named;
    let (pos, named): (&[_], &[(_, _)]) = match args {
        DynamicLambdaArgs::OldPositional {
            ctx,
            artifacts,
            outputs,
        } => {
            pos = [ctx, artifacts.get(), outputs.get()];
            (&pos, &[])
        }
        DynamicLambdaArgs::DynamicActionsNamed {
            actions,
            artifacts,
            outputs,
            arg,
        } => {
            named = [
                ("actions", actions.to_value()),
                ("artifacts", artifacts.get()),
                ("outputs", outputs.get()),
                ("arg", arg),
            ];
            (&[], &named)
        }
    };
    let return_value = eval
        .eval_function(lambda, pos, named)
        .map_err(BuckStarlarkError::new)?;

    if !return_value.is_none() {
        return Err(DynamicLambdaError::LambdaMustReturnNone(
            return_value.to_string_for_type_error(),
        )
        .into());
    }

    Ok(())
}

/// The `Output` from `DynamicLambda`.
#[derive(Clone, Debug, Allocative)]
pub struct DynamicLambdaOutput {}

impl DeferredOutput for DynamicLambdaOutput {}

#[derive(Debug, Allocative)]
pub(crate) struct DynamicLambdaAsDeferred(pub(crate) Arc<DynamicLambda>);

impl provider::Provider for DynamicLambdaAsDeferred {
    fn provide<'a>(&'a self, _demand: &mut provider::Demand<'a>) {
        panic!("dynamic lambda is not really a deferred, it shoudln't reach here")
    }
}

impl Deferred for DynamicLambdaAsDeferred {
    type Output = DynamicLambdaOutput;

    fn inputs(&self) -> DeferredInputsRef<'_> {
        DeferredInputsRef::IndexSet(&self.0.dynamic)
    }

    async fn execute(
        &self,
        deferred_ctx: &mut dyn DeferredCtx,
        dice: &mut DiceComputations<'_>,
    ) -> anyhow::Result<DeferredValue<Self::Output>> {
        if let BaseDeferredKey::BxlLabel(key) = &self.0.owner {
            eval_bxl_for_dynamic_output(key, &self.0, deferred_ctx, dice).await?
        } else {
            let proto_rule = "dynamic_lambda".to_owned();

            let start_event = buck2_data::AnalysisStart {
                target: Some(buck2_data::analysis_start::Target::DynamicLambda(
                    self.0.owner.to_proto().into(),
                )),
                rule: proto_rule.clone(),
            };

            span_async(start_event, async {
                let mut declared_actions = None;
                let mut declared_artifacts = None;

                let output: anyhow::Result<()> = try {
                    let env = Module::new();

                    let analysis_registry = {
                        let heap = env.heap();
                        let print = EventDispatcherPrintHandler(get_dispatcher());
                        let mut eval = Evaluator::new(&env);
                        eval.set_print_handler(&print);
                        eval.set_soft_error_handler(&Buck2StarlarkSoftErrorHandler);
                        let dynamic_lambda_ctx_data =
                            dynamic_lambda_ctx_data(&self.0, deferred_ctx, &env)?;
                        let ctx = AnalysisContext::prepare(
                            heap,
                            dynamic_lambda_ctx_data.lambda.attributes()?,
                            self.0.owner.configured_label(),
                            dynamic_lambda_ctx_data.lambda.plugins()?,
                            dynamic_lambda_ctx_data.registry,
                            dynamic_lambda_ctx_data.digest_config,
                        );

                        let args = match dynamic_lambda_ctx_data.lambda.arg() {
                            None => DynamicLambdaArgs::OldPositional {
                                ctx: ctx.to_value(),
                                artifacts: dynamic_lambda_ctx_data.artifacts,
                                outputs: dynamic_lambda_ctx_data.outputs,
                            },
                            Some(arg) => DynamicLambdaArgs::DynamicActionsNamed {
                                // TODO(nga): no need to create `ctx`
                                //   because we only need `actions` here.
                                actions: ctx.actions,
                                artifacts: dynamic_lambda_ctx_data.artifacts,
                                outputs: dynamic_lambda_ctx_data.outputs,
                                arg,
                            },
                        };

                        invoke_dynamic_output_lambda(
                            &mut eval,
                            dynamic_lambda_ctx_data.lambda.lambda(),
                            args,
                        )?;

                        ctx.assert_no_promises()?;

                        ctx.take_state()
                    };

                    declared_actions = Some(analysis_registry.num_declared_actions());
                    declared_artifacts = Some(analysis_registry.num_declared_artifacts());
                    let (_frozen_env, deferred) = analysis_registry.finalize(&env)?(env)?;
                    let _fake_registry = mem::replace(deferred_ctx.registry(), deferred);
                };

                (
                    output,
                    buck2_data::AnalysisEnd {
                        target: Some(buck2_data::analysis_end::Target::DynamicLambda(
                            self.0.owner.to_proto().into(),
                        )),
                        rule: proto_rule,
                        profile: None,
                        declared_actions,
                        declared_artifacts,
                    },
                )
            })
            .await?
        }

        Ok(DeferredValue::Ready(DynamicLambdaOutput {}))
    }

    fn span(&self) -> Option<buck2_data::span_start_event::Data> {
        let owner = self.0.owner.to_proto().into();
        Some(buck2_data::DynamicLambdaStart { owner: Some(owner) }.into())
    }
}

/// Data used to construct an `AnalysisContext` or `BxlContext` for the dynamic lambda.
pub struct DynamicLambdaCtxData<'v> {
    pub lambda: &'v FrozenDynamicLambdaParams,
    pub outputs: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkDeclaredArtifact>>,
    pub artifacts: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>,
    pub key: &'v BaseDeferredKey,
    pub digest_config: DigestConfig,
    pub registry: AnalysisRegistry<'v>,
}

/// Sets up the data needed to create the dynamic lambda ctx and evaluate the lambda.
pub fn dynamic_lambda_ctx_data<'v>(
    dynamic_lambda: &'v DynamicLambda,
    deferred_ctx: &mut dyn DeferredCtx,
    env: &'v Module,
) -> anyhow::Result<DynamicLambdaCtxData<'v>> {
    let heap = env.heap();
    let mut outputs = Vec::with_capacity(dynamic_lambda.outputs.len());

    let attributes_lambda = &dynamic_lambda
        .attributes_lambda
        .as_ref()
        .internal_error("attributes_lambda not set")?
        .owned_as_ref(env.frozen_heap())
        .value;

    let execution_platform = {
        match &dynamic_lambda.owner {
            BaseDeferredKey::TargetLabel(target) => {
                let configured_target = deferred_ctx.get_configured_target(target).unwrap();

                configured_target.execution_platform_resolution().dupe()
            }
            BaseDeferredKey::BxlLabel(k) => k.execution_platform_resolution().clone(),
            BaseDeferredKey::AnonTarget(_) => {
                return Err(DynamicLambdaError::AnonTargetIncompatible.into());
            }
        }
    };

    // The DeferredCtx has a registry it wants us to use as &mut.
    // The AnalysisRegistry wants ownership of a registry.
    // To overcome the difference, we create a fake registry, swap it with the one in deferred,
    // and swap back after AnalysisRegistry completes.

    let fake_registry = DeferredRegistry::new(DeferredHolderKey::Base(dynamic_lambda.owner.dupe()));

    let deferred = mem::replace(deferred_ctx.registry(), fake_registry);
    let mut registry = AnalysisRegistry::new_from_owner_and_deferred(
        dynamic_lambda.owner.dupe(),
        execution_platform,
        deferred,
    )?;
    registry.set_action_key(Arc::from(deferred_ctx.get_action_key()));

    let mut artifacts = Vec::with_capacity(dynamic_lambda.dynamic.len());
    let fs = deferred_ctx.project_filesystem();
    for x in &dynamic_lambda.dynamic {
        let x = match x {
            DeferredInput::MaterializedArtifact(x) => x,
            DeferredInput::ConfiguredTarget(_) => continue,
            _ => unreachable!("DynamicLambda only depends on artifact and target"),
        };
        let k = StarlarkArtifact::new(x.dupe());
        let path = deferred_ctx.get_materialized_artifact(x).unwrap();
        let v = StarlarkArtifactValue::new(x.dupe(), path.to_owned(), fs.dupe());
        artifacts.push((k, v));
    }

    for x in &*dynamic_lambda.outputs {
        let k = StarlarkArtifact::new(Artifact::from(x.dupe()));
        let declared = registry.declare_dynamic_output(x)?;
        let v = StarlarkDeclaredArtifact::new(None, declared, AssociatedArtifacts::new());
        outputs.push((k, v));
    }

    let artifacts = heap.alloc_typed_unchecked(AllocDict(artifacts)).cast();
    let outputs = heap.alloc_typed_unchecked(AllocDict(outputs)).cast();

    Ok(DynamicLambdaCtxData {
        lambda: attributes_lambda,
        outputs,
        artifacts,
        key: &dynamic_lambda.owner,
        digest_config: deferred_ctx.digest_config(),
        registry,
    })
}
