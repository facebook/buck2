/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::iter;
use std::sync::Arc;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::BoundBuildArtifact;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::analysis::registry::RecordedAnalysisValues;
use buck2_build_api::artifact_groups::calculation::ArtifactGroupCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::dynamic::calculation::dynamic_lambda_result;
use buck2_build_api::dynamic_value::DynamicValue;
use buck2_build_api::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::context::AnalysisContext;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::interpreter::rule_defs::provider::collection::ProviderCollection;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::deferred::dynamic::DynamicLambdaResultsKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::span_async;
use buck2_events::dispatch::span_async_simple;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_futures::cancellation::CancellationObserver;
use buck2_interpreter::from_freeze::from_freeze_error;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::soft_error::Buck2StarlarkSoftErrorHandler;
use dice::CancellationContext;
use dice::DiceComputations;
use dupe::Dupe;
use futures::FutureExt;
use indexmap::IndexSet;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::dict::AllocDict;
use starlark::values::dict::DictType;
use starlark::values::list::AllocList;
use starlark::values::tuple::AllocTuple;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::OwnedRefFrozenRef;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::values::ValueTypedComplex;
use starlark_map::small_map::SmallMap;

use crate::dynamic::attrs::DynamicAttrValue;
use crate::dynamic::attrs::DynamicAttrValues;
use crate::dynamic::bxl::eval_bxl_for_dynamic_output;
use crate::dynamic::dynamic_actions_callable::FrozenStarlarkDynamicActionsCallable;
use crate::dynamic::dynamic_actions_callable::P_ACTIONS;
use crate::dynamic::params::FrozenDynamicLambdaParams;
use crate::dynamic::resolved_dynamic_value::StarlarkResolvedDynamicValue;

pub enum DynamicLambdaArgs<'v> {
    OldPositional {
        ctx: Value<'v>,
        artifact_values: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>,
        outputs: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkDeclaredArtifact>>,
    },
    DynamicActionsNamed {
        actions: ValueTyped<'v, AnalysisActions<'v>>,
        attr_values: Box<[(String, Value<'v>)]>,
    },
    DynamicActionsBxlNamed {
        // cannot import BxlContext because it bxl is depends on this crate
        bxl_ctx: Value<'v>,
        attr_values: Box<[(String, Value<'v>)]>,
    },
}

pub fn invoke_dynamic_output_lambda<'v>(
    eval: &mut Evaluator<'v, '_, '_>,
    lambda: Value<'v>,
    args: DynamicLambdaArgs<'v>,
) -> buck2_error::Result<ProviderCollection<'v>> {
    let pos;
    let named;
    let (pos, named): (&[_], &[(_, _)]) = match &args {
        DynamicLambdaArgs::OldPositional {
            ctx,
            artifact_values,
            outputs,
        } => {
            pos = [*ctx, artifact_values.get(), outputs.get()];
            (&pos, &[])
        }
        DynamicLambdaArgs::DynamicActionsNamed {
            actions,
            attr_values,
        } => {
            named = iter::once((P_ACTIONS.name, actions.to_value()))
                .chain(attr_values.iter().map(|(k, v)| (k.as_str(), *v)))
                .collect::<Vec<(&str, Value)>>();
            (&[], &named)
        }
        DynamicLambdaArgs::DynamicActionsBxlNamed {
            bxl_ctx,
            attr_values,
        } => {
            named = iter::once(("bxl_ctx", bxl_ctx.dupe()))
                .chain(attr_values.iter().map(|(k, v)| (k.as_str(), *v)))
                .collect::<Vec<(&str, Value)>>();
            (&[], &named)
        }
    };
    let return_value = eval.eval_function(lambda, pos, named)?;

    let provider_collection = match args {
        DynamicLambdaArgs::OldPositional { .. } => {
            if !return_value.is_none() {
                return Err(buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "dynamic_output lambda must return `None`, got: `{0}`",
                    return_value.to_string_for_type_error()
                ));
            }
            ProviderCollection::try_from_value_dynamic_output(
                FrozenValue::new_empty_list().to_value(),
            )?
        }
        DynamicLambdaArgs::DynamicActionsNamed { .. } => {
            ProviderCollection::try_from_value_dynamic_output(return_value)?
        }
        DynamicLambdaArgs::DynamicActionsBxlNamed { .. } => {
            ProviderCollection::try_from_value_dynamic_output(return_value)?
        }
    };

    Ok(provider_collection)
}

async fn execute_lambda(
    lambda: OwnedRefFrozenRef<'_, FrozenDynamicLambdaParams>,
    dice: &mut DiceComputations<'_>,
    self_key: DynamicLambdaResultsKey,
    resolved_dynamic_values: HashMap<DynamicValue, FrozenProviderCollectionValue>,
    input_artifacts_materialized: InputArtifactsMaterialized,
    digest_config: DigestConfig,
    liveness: CancellationObserver,
) -> buck2_error::Result<RecordedAnalysisValues> {
    if let BaseDeferredKey::BxlLabel(key) = &lambda.as_ref().static_fields.owner {
        Ok(eval_bxl_for_dynamic_output(
            key,
            self_key,
            lambda,
            dice,
            input_artifacts_materialized,
            resolved_dynamic_values,
            digest_config,
            liveness,
        )
        .await?)
    } else {
        let proto_rule = "dynamic_lambda".to_owned();

        let start_event = buck2_data::AnalysisStart {
            target: Some(buck2_data::analysis_start::Target::DynamicLambda(
                lambda.as_ref().static_fields.owner.to_proto().into(),
            )),
            rule: proto_rule.clone(),
        };

        let artifact_fs = dice.get_artifact_fs().await?;

        span_async(start_event, async {
            let mut declared_actions = None;
            let mut declared_artifacts = None;

            let output: buck2_error::Result<_> = try {
                let env = Module::new();

                let analysis_registry = {
                    let heap = env.heap();
                    let print = EventDispatcherPrintHandler(get_dispatcher());
                    let mut eval = Evaluator::new(&env);
                    eval.set_print_handler(&print);
                    eval.set_soft_error_handler(&Buck2StarlarkSoftErrorHandler);
                    let dynamic_lambda_ctx_data = dynamic_lambda_ctx_data(
                        lambda,
                        self_key,
                        input_artifacts_materialized,
                        &resolved_dynamic_values,
                        &artifact_fs,
                        digest_config,
                        &env,
                    )?;
                    let ctx = AnalysisContext::prepare(
                        heap,
                        dynamic_lambda_ctx_data.lambda.attributes()?,
                        lambda.as_ref().static_fields.owner.configured_label(),
                        dynamic_lambda_ctx_data.lambda.plugins()?,
                        dynamic_lambda_ctx_data.registry,
                        dynamic_lambda_ctx_data.digest_config,
                    );

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
                        (Some(_arg), DynamicLambdaCtxDataSpec::New { attr_values }) => {
                            DynamicLambdaArgs::DynamicActionsNamed {
                                // TODO(nga): no need to create `ctx`
                                //   because we only need `actions` here.
                                actions: ctx.actions,
                                attr_values: attr_values.clone(),
                            }
                        }
                        (None, DynamicLambdaCtxDataSpec::New { .. })
                        | (Some(_), DynamicLambdaCtxDataSpec::Old { .. }) => {
                            Err(internal_error!(
                                "Unexpected combination of attr_values and spec"
                            ))?;
                            unreachable!();
                        }
                    };

                    let providers: ProviderCollection = invoke_dynamic_output_lambda(
                        &mut eval,
                        dynamic_lambda_ctx_data.lambda.lambda(),
                        args,
                    )?;
                    let providers = eval.heap().alloc(providers);
                    let providers = ValueTypedComplex::<ProviderCollection>::new(providers)
                        .internal_error("Just allocated ProviderCollection")?;

                    ctx.assert_no_promises()?;

                    let registry = ctx.take_state();

                    registry
                        .analysis_value_storage
                        .set_result_value(providers)?;

                    registry
                };

                declared_actions = Some(analysis_registry.num_declared_actions());
                declared_artifacts = Some(analysis_registry.num_declared_artifacts());
                let registry_finalizer = analysis_registry.finalize(&env)?;
                let frozen_env = env.freeze().map_err(from_freeze_error)?;
                registry_finalizer(&frozen_env)?
            };

            (
                output,
                buck2_data::AnalysisEnd {
                    target: Some(buck2_data::analysis_end::Target::DynamicLambda(
                        lambda.as_ref().static_fields.owner.to_proto().into(),
                    )),
                    rule: proto_rule,
                    profile: None,
                    declared_actions,
                    declared_artifacts,
                },
            )
        })
        .await
    }
}

pub(crate) async fn prepare_and_execute_lambda(
    ctx: &mut DiceComputations<'_>,
    cancellation: &CancellationContext,
    lambda: OwnedRefFrozenRef<'_, FrozenDynamicLambdaParams>,
    self_holder_key: DynamicLambdaResultsKey,
) -> buck2_error::Result<RecordedAnalysisValues> {
    // This is a bit suboptimal: we wait for all artifacts to be ready in order to
    // materialize any of them. However that is how we execute *all* local actions so in
    // the grand scheme of things that's probably not a huge deal.
    ensure_artifacts_built(&lambda.as_ref().static_fields.artifact_values, ctx).await?;

    span_async_simple(
        buck2_data::DynamicLambdaStart {
            owner: Some(lambda.as_ref().static_fields.owner.to_proto().into()),
        },
        async move {
            let (input_artifacts_materialized, resolved_dynamic_values) = span_async_simple(
                buck2_data::DeferredPreparationStageStart {
                    stage: Some(buck2_data::MaterializedArtifacts {}.into()),
                },
                ctx.try_compute2(
                    |ctx| {
                        Box::pin(materialize_inputs(
                            &lambda.as_ref().static_fields.artifact_values,
                            ctx,
                        ))
                    },
                    |ctx| {
                        Box::pin(resolve_dynamic_values(
                            &lambda.as_ref().static_fields.dynamic_values,
                            ctx,
                        ))
                    },
                ),
                buck2_data::DeferredPreparationStageEnd {},
            )
            .await?;

            cancellation
                .with_structured_cancellation(|observer| {
                    execute_lambda(
                        lambda,
                        ctx,
                        self_holder_key,
                        resolved_dynamic_values,
                        input_artifacts_materialized,
                        ctx.global_data().get_digest_config(),
                        observer,
                    )
                    .boxed()
                })
                .await
        },
        buck2_data::DeferredEvaluationEnd {},
    )
    .await
}

async fn ensure_artifacts_built(
    materialized_artifacts: &IndexSet<Artifact>,
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<()> {
    if materialized_artifacts.is_empty() {
        return Ok(());
    }
    ctx.try_compute_join(materialized_artifacts, |ctx, artifact| {
        async move {
            ctx.ensure_artifact_group(&ArtifactGroup::Artifact(artifact.dupe()))
                .await
        }
        .boxed()
    })
    .await?;

    Ok(())
}

/// Marker to indicate artifacts we pass to dynamic actions are materialized.
/// To not forget to materialize after refactoring.
#[derive(Copy, Clone, Dupe)]
pub struct InputArtifactsMaterialized(());

async fn materialize_inputs(
    materialized_artifacts: &IndexSet<Artifact>,
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<InputArtifactsMaterialized> {
    if materialized_artifacts.is_empty() {
        return Ok(InputArtifactsMaterialized(()));
    }

    let artifact_fs = ctx.get_artifact_fs().await?;

    let mut paths = Vec::with_capacity(materialized_artifacts.len());

    for artifact in materialized_artifacts {
        let path = artifact.resolve_path(&artifact_fs)?;
        paths.push(path.clone());
    }

    ctx.per_transaction_data()
        .get_materializer()
        .ensure_materialized(paths)
        .await?;

    Ok(InputArtifactsMaterialized(()))
}

async fn resolve_dynamic_values(
    dynamic_values: &IndexSet<DynamicValue>,
    ctx: &mut DiceComputations<'_>,
) -> buck2_error::Result<HashMap<DynamicValue, FrozenProviderCollectionValue>> {
    if dynamic_values.is_empty() {
        return Ok(HashMap::new());
    }

    let providers = ctx
        .try_compute_join(dynamic_values, |ctx, dynamic_value| {
            Box::pin(async {
                let result = dynamic_lambda_result(ctx, &dynamic_value.dynamic_lambda_results_key)
                    .await?
                    .analysis_values
                    .provider_collection()?
                    .to_owned();
                buck2_error::Ok((dynamic_value.dupe(), result))
            })
        })
        .await?;

    Ok(HashMap::from_iter(providers))
}

pub enum DynamicLambdaCtxDataSpec<'v> {
    Old {
        outputs: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkDeclaredArtifact>>,
        artifact_values: ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>,
    },
    New {
        attr_values: Box<[(String, Value<'v>)]>,
    },
}

/// Data used to construct an `AnalysisContext` or `BxlContext` for the dynamic lambda.
pub struct DynamicLambdaCtxData<'v> {
    pub lambda: &'v FrozenDynamicLambdaParams,
    pub key: &'v BaseDeferredKey,
    pub spec: DynamicLambdaCtxDataSpec<'v>,
    pub digest_config: DigestConfig,
    pub registry: AnalysisRegistry<'v>,
}

/// Prepare dict of artifact values for dynamic actions.
fn artifact_values<'v>(
    artifact_values: &IndexSet<Artifact>,
    _: InputArtifactsMaterialized,
    artifact_fs: &ArtifactFs,
    heap: &'v Heap,
) -> buck2_error::Result<ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkArtifactValue>>> {
    let mut artifact_values_dict = Vec::with_capacity(artifact_values.len());
    for x in artifact_values {
        let k = StarlarkArtifact::new(x.dupe());
        let path = x.get_path().resolve(artifact_fs)?;
        // `InputArtifactsMaterialized` marker indicates that the artifact is materialized.
        let v = StarlarkArtifactValue::new(x.dupe(), path.to_owned(), artifact_fs.fs().dupe());
        artifact_values_dict.push((k, v));
    }
    Ok(heap
        .alloc_typed_unchecked(AllocDict(artifact_values_dict))
        .cast())
}

/// Prepare dict of output artifacts for dynamic actions.
fn outputs<'v>(
    outputs: &[BoundBuildArtifact],
    registry: &mut AnalysisRegistry<'v>,
    heap: &'v Heap,
) -> buck2_error::Result<ValueOfUnchecked<'v, DictType<StarlarkArtifact, StarlarkDeclaredArtifact>>>
{
    let mut outputs_dict = Vec::with_capacity(outputs.len());
    for x in outputs {
        let k = StarlarkArtifact::new(x.dupe().into_artifact());
        let declared = registry.declare_dynamic_output(x.as_base_artifact())?;
        let v = StarlarkDeclaredArtifact::new(None, declared, AssociatedArtifacts::new());
        outputs_dict.push((k, v));
    }

    Ok(heap.alloc_typed_unchecked(AllocDict(outputs_dict)).cast())
}

fn new_attr_value<'v>(
    value: &DynamicAttrValue<FrozenValue, BoundBuildArtifact>,
    _input_artifacts_materialized: InputArtifactsMaterialized,
    artifact_fs: &ArtifactFs,
    registry: &mut AnalysisRegistry<'v>,
    resolved_dynamic_values: &HashMap<DynamicValue, FrozenProviderCollectionValue>,
    env: &'v Module,
) -> buck2_error::Result<Value<'v>> {
    match value {
        DynamicAttrValue::Output(artifact) => {
            let declared = registry.declare_dynamic_output(artifact.as_base_artifact())?;
            let artifact = env.heap().alloc_typed(StarlarkDeclaredArtifact::new(
                None,
                declared,
                AssociatedArtifacts::new(),
            ));
            Ok(env.heap().alloc(StarlarkOutputArtifact::new(artifact)))
        }
        DynamicAttrValue::ArtifactValue(artifact) => {
            let path = artifact.get_path().resolve(&artifact_fs)?;
            // `InputArtifactsMaterialized` marker indicates that the artifact is materialized.
            Ok(env.heap().alloc(StarlarkArtifactValue::new(
                Artifact::from(artifact.dupe()),
                path.to_owned(),
                artifact_fs.fs().dupe(),
            )))
        }
        DynamicAttrValue::DynamicValue(v) => {
            let v = resolved_dynamic_values
                .get(v)
                .internal_error("Missing resolved dynamic value")?;
            Ok(env.heap().alloc(StarlarkResolvedDynamicValue {
                value: v.add_heap_ref_static(env.frozen_heap()),
            }))
        }
        DynamicAttrValue::Value(v) => Ok(v.to_value()),
        DynamicAttrValue::List(xs) => {
            let xs = xs
                .iter()
                .map(|x| {
                    new_attr_value(
                        x,
                        _input_artifacts_materialized,
                        artifact_fs,
                        registry,
                        resolved_dynamic_values,
                        env,
                    )
                })
                .collect::<buck2_error::Result<Vec<_>>>()?;
            Ok(env.heap().alloc(AllocList(xs)))
        }
        DynamicAttrValue::Dict(xs) => {
            let mut r = SmallMap::with_capacity(xs.len());
            for (k, v) in xs {
                let prev = r.insert_hashed(
                    k.to_value().get_hashed()?,
                    new_attr_value(
                        v,
                        _input_artifacts_materialized,
                        artifact_fs,
                        registry,
                        resolved_dynamic_values,
                        env,
                    )?,
                );
                if prev.is_some() {
                    return Err(buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Duplicate key in dict"
                    ));
                }
            }
            Ok(env.heap().alloc(AllocDict(r)))
        }
        DynamicAttrValue::Tuple(xs) => {
            let xs = xs
                .iter()
                .map(|x| {
                    new_attr_value(
                        x,
                        _input_artifacts_materialized,
                        artifact_fs,
                        registry,
                        resolved_dynamic_values,
                        env,
                    )
                })
                .collect::<buck2_error::Result<Vec<_>>>()?;
            Ok(env.heap().alloc(AllocTuple(xs)))
        }
        DynamicAttrValue::Option(option) => match option {
            Some(v) => new_attr_value(
                v,
                _input_artifacts_materialized,
                artifact_fs,
                registry,
                resolved_dynamic_values,
                env,
            ),
            None => Ok(Value::new_none()),
        },
    }
}

fn new_attr_values<'v>(
    values: &DynamicAttrValues<FrozenValue, BoundBuildArtifact>,
    callable: &FrozenStarlarkDynamicActionsCallable,
    input_artifacts_materialized: InputArtifactsMaterialized,
    artifact_fs: &ArtifactFs,
    registry: &mut AnalysisRegistry<'v>,
    resolved_dynamic_values: &HashMap<DynamicValue, FrozenProviderCollectionValue>,
    env: &'v Module,
) -> buck2_error::Result<Box<[(String, Value<'v>)]>> {
    if values.values.len() != callable.attrs.len() {
        return Err(internal_error!("Parameter count mismatch"));
    }
    callable
        .attrs
        .keys()
        .zip(values.values.iter())
        .map(|(name, value)| {
            Ok((
                name.clone(),
                new_attr_value(
                    value,
                    input_artifacts_materialized,
                    artifact_fs,
                    registry,
                    resolved_dynamic_values,
                    env,
                )?,
            ))
        })
        .collect()
}

/// Sets up the data needed to create the dynamic lambda ctx and evaluate the lambda.
pub fn dynamic_lambda_ctx_data<'v>(
    dynamic_lambda: OwnedRefFrozenRef<'_, FrozenDynamicLambdaParams>,
    self_key: DynamicLambdaResultsKey,
    input_artifacts_materialized: InputArtifactsMaterialized,
    resolved_dynamic_values: &HashMap<DynamicValue, FrozenProviderCollectionValue>,
    artifact_fs: &ArtifactFs,
    digest_config: DigestConfig,
    env: &'v Module,
) -> buck2_error::Result<DynamicLambdaCtxData<'v>> {
    let self_key = Arc::new(self_key);

    if &dynamic_lambda.as_ref().static_fields.owner != self_key.owner() {
        return Err(internal_error!(
            "Dynamic lambda owner `{}` does not match self key `{}`",
            dynamic_lambda.as_ref().static_fields.owner,
            self_key
        ));
    }

    let dynamic_lambda = dynamic_lambda.add_heap_ref(env.frozen_heap());

    let mut registry = AnalysisRegistry::new_from_owner_and_deferred(
        dynamic_lambda.static_fields.execution_platform.dupe(),
        DeferredHolderKey::DynamicLambda(self_key),
    )?;

    let spec = match &dynamic_lambda.attr_values {
        None => {
            let artifact_values = artifact_values(
                &dynamic_lambda.static_fields.artifact_values,
                input_artifacts_materialized,
                artifact_fs,
                env.heap(),
            )?;
            let outputs = outputs(
                &dynamic_lambda.static_fields.outputs,
                &mut registry,
                env.heap(),
            )?;
            if !dynamic_lambda.static_fields.dynamic_values.is_empty() {
                return Err(internal_error!(
                    "Non-empty `dynamic_value` for `dynamic_output`"
                ));
            }
            DynamicLambdaCtxDataSpec::Old {
                outputs,
                artifact_values,
            }
        }
        Some((attr_values, callable)) => DynamicLambdaCtxDataSpec::New {
            attr_values: new_attr_values(
                attr_values,
                callable.as_ref(),
                input_artifacts_materialized,
                artifact_fs,
                &mut registry,
                resolved_dynamic_values,
                env,
            )?,
        },
    };

    Ok(DynamicLambdaCtxData {
        lambda: dynamic_lambda,
        spec,
        key: &dynamic_lambda.static_fields.owner,
        digest_config,
        registry,
    })
}
