/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any;
use std::any::Demand;
use std::mem;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::DeclaredArtifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_artifact::artifact::provide_outputs::ProvideOutputs;
use buck2_artifact::deferred::data::DeferredData;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_events::dispatch::get_dispatcher;
use buck2_execute::digest_config::DigestConfig;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use dice::DiceComputations;
use dupe::Dupe;
use gazebo::prelude::*;
use indexmap::indexset;
use indexmap::IndexSet;
use starlark::collections::SmallMap;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::dict::Dict;
use starlark::values::tuple::TupleRef;
use starlark::values::OwnedFrozenValue;
use starlark::values::Value;
use starlark::values::ValueTypedComplex;
use thiserror::Error;

use crate::actions::key::ActionKeyExt;
use crate::actions::RegisteredAction;
use crate::analysis::registry::AnalysisRegistry;
use crate::deferred::types::BaseKey;
use crate::deferred::types::Deferred;
use crate::deferred::types::DeferredCtx;
use crate::deferred::types::DeferredInput;
use crate::deferred::types::DeferredRegistry;
use crate::deferred::types::DeferredValue;
use crate::dynamic::bxl::eval_bxl_for_dynamic_output;
use crate::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactValue;
use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::context::AnalysisContext;
use crate::interpreter::rule_defs::plugins::AnalysisPlugins;

/// The artifacts that are returned are dynamic actions, which depend on the `DynamicLambda`
/// to get their real `RegisteredAction`.
#[derive(Clone, Debug, Allocative)]
pub(crate) struct DynamicAction {
    // A singleton pointing at a DynamicLambda
    inputs: IndexSet<DeferredInput>,
    index: usize,
}

impl DynamicAction {
    pub fn new(deferred: &DeferredData<DynamicLambdaOutput>, index: usize) -> Self {
        Self {
            inputs: indexset![DeferredInput::Deferred(deferred.deferred_key().dupe())],
            index,
        }
    }
}

/// The lambda captured by `dynamic_output`, alongside the other required data.
#[derive(Clone, Debug, Allocative)]
pub struct DynamicLambda {
    /// the owner that defined this lambda
    owner: BaseDeferredKey,
    /// Things required by the lambda (wrapped in DeferredInput)
    dynamic: IndexSet<DeferredInput>,
    /// Things I am allowed to use as inputs, but don't wait for
    inputs: IndexSet<Artifact>,
    /// Things I produce
    outputs: Vec<BuildArtifact>,
    /// A Starlark pair of the attributes and a lambda function that binds the outputs given a context
    attributes_lambda: OwnedFrozenValue,
}

impl DynamicLambda {
    pub(crate) fn new(
        owner: BaseDeferredKey,
        dynamic: IndexSet<Artifact>,
        inputs: IndexSet<Artifact>,
        outputs: Vec<BuildArtifact>,
    ) -> Self {
        let mut depends = IndexSet::with_capacity(dynamic.len() + 1);
        match &owner {
            BaseDeferredKey::TargetLabel(target) => {
                depends.insert(DeferredInput::ConfiguredTarget(target.dupe()));
            }
            BaseDeferredKey::BxlLabel(_) => {
                // Execution platform resolution is handled when we execute the DynamicLambda
            }
            BaseDeferredKey::AnonTarget(_) => {
                // This will return an error later, so doesn't need to have the dependency
            }
        }
        depends.extend(dynamic.into_iter().map(DeferredInput::MaterializedArtifact));
        Self {
            owner,
            dynamic: depends,
            inputs,
            outputs,
            attributes_lambda: Default::default(),
        }
    }

    pub(crate) fn bind(&mut self, attributes_lambda: OwnedFrozenValue) {
        self.attributes_lambda = attributes_lambda;
    }
}

/// The `Output` from `DynamicLambda`.
#[derive(Clone, Debug, Allocative)]
pub struct DynamicLambdaOutput {
    /// The actions the DynamicLambda produces, in the right order.
    /// `DynamicAction.index` is an index into this Vec.
    output: Vec<ActionKey>,
}

impl any::Provider for DynamicAction {
    fn provide<'a>(&'a self, _demand: &mut Demand<'a>) {}
}

#[async_trait]
impl Deferred for DynamicAction {
    type Output = Arc<RegisteredAction>;

    fn inputs(&self) -> &IndexSet<DeferredInput> {
        &self.inputs
    }

    async fn execute(
        &self,
        ctx: &mut dyn DeferredCtx,
        _dice: &DiceComputations,
    ) -> anyhow::Result<DeferredValue<Self::Output>> {
        let id = match self.inputs.iter().into_singleton() {
            Some(DeferredInput::Deferred(x)) => x,
            _ => unreachable!("DynamicAction must have a single Deferred as as inputs"),
        };
        let val = ctx.get_deferred_data(id).unwrap();
        let key = val
            .downcast::<DynamicLambdaOutput>()?
            .output
            .get(self.index)
            .map_or_else(
                || {
                    Err(anyhow::anyhow!(
                        "Unexpected index in DynamicAction (internal error)"
                    ))
                },
                Ok,
            )?;
        Ok(DeferredValue::Deferred(key.deferred_data().dupe()))
    }
}

#[derive(Debug, Error)]
enum DynamicLambdaError {
    #[error("dynamic_output and anon_target cannot be used together (yet)")]
    AnonTargetIncompatible,
}

impl any::Provider for DynamicLambda {
    fn provide<'a>(&'a self, demand: &mut Demand<'a>) {
        demand.provide_value_with(|| ProvideOutputs(Ok(self.outputs.clone())));
    }
}

#[async_trait]
impl Deferred for DynamicLambda {
    type Output = DynamicLambdaOutput;

    fn inputs(&self) -> &IndexSet<DeferredInput> {
        &self.dynamic
    }

    async fn execute(
        &self,
        deferred_ctx: &mut dyn DeferredCtx,
        dice: &DiceComputations,
    ) -> anyhow::Result<DeferredValue<Self::Output>> {
        let output = if let BaseDeferredKey::BxlLabel(key) = &self.owner {
            eval_bxl_for_dynamic_output(key, self, deferred_ctx, dice).await
        } else {
            let env = Module::new();

            let (analysis_registry, declared_outputs) = {
                let heap = env.heap();
                let print = EventDispatcherPrintHandler(get_dispatcher());
                let mut eval = Evaluator::new(&env);
                eval.set_print_handler(&print);
                let dynamic_lambda_ctx_data = dynamic_lambda_ctx_data(self, deferred_ctx, &env)?;
                let ctx = heap.alloc_typed(AnalysisContext::new(
                    heap,
                    dynamic_lambda_ctx_data.attributes,
                    match &self.owner {
                        BaseDeferredKey::TargetLabel(target) => Some(heap.alloc_typed(
                            StarlarkConfiguredProvidersLabel::new(ConfiguredProvidersLabel::new(
                                target.dupe(),
                                ProvidersName::Default,
                            )),
                        )),
                        BaseDeferredKey::BxlLabel(target) | BaseDeferredKey::AnonTarget(target) => {
                            target.configured_label().map(|configured_target_label| {
                                heap.alloc_typed(StarlarkConfiguredProvidersLabel::new(
                                    ConfiguredProvidersLabel::new(
                                        configured_target_label,
                                        ProvidersName::Default,
                                    ),
                                ))
                            })
                        }
                    },
                    dynamic_lambda_ctx_data.plugins,
                    dynamic_lambda_ctx_data.registry,
                    dynamic_lambda_ctx_data.digest_config,
                ));

                eval.eval_function(
                    dynamic_lambda_ctx_data.lambda,
                    &[
                        ctx.to_value(),
                        dynamic_lambda_ctx_data.artifacts,
                        dynamic_lambda_ctx_data.outputs,
                    ],
                    &[],
                )?;
                ctx.assert_no_promises()?;

                (ctx.take_state(), dynamic_lambda_ctx_data.declared_outputs)
            };

            let (_frozen_env, deferred) = analysis_registry.finalize(&env)?(env)?;
            let _fake_registry = mem::replace(deferred_ctx.registry(), deferred);

            // TODO(ndmitchell): Check we don't use anything not in `inputs`

            let output: anyhow::Result<Vec<_>> = declared_outputs
                .into_iter()
                .map(|x| anyhow::Ok(x.ensure_bound()?.action_key().dupe()))
                .collect();
            output
        };
        Ok(DeferredValue::Ready(DynamicLambdaOutput {
            output: output?,
        }))
    }

    fn span(&self) -> Option<buck2_data::span_start_event::Data> {
        let owner = self.owner.to_proto().into();
        Some(buck2_data::DynamicLambdaStart { owner: Some(owner) }.into())
    }
}

/// Data used to construct an `AnalysisContext` or `BxlContext` for the dynamic lambda.
pub struct DynamicLambdaCtxData<'v> {
    pub attributes: Value<'v>,
    pub lambda: Value<'v>,
    pub outputs: Value<'v>,
    pub plugins: ValueTypedComplex<'v, AnalysisPlugins<'v>>,
    pub artifacts: Value<'v>,
    pub key: &'v BaseDeferredKey,
    pub digest_config: DigestConfig,
    pub declared_outputs: IndexSet<DeclaredArtifact>,
    pub registry: AnalysisRegistry<'v>,
}

/// Sets up the data needed to create the dynamic lambda ctx and evaluate the lambda.
pub fn dynamic_lambda_ctx_data<'v>(
    dynamic_lambda: &'v DynamicLambda,
    deferred_ctx: &'v mut dyn DeferredCtx,
    env: &'v Module,
) -> anyhow::Result<DynamicLambdaCtxData<'v>> {
    let heap = env.heap();
    let mut outputs = SmallMap::with_capacity(dynamic_lambda.outputs.len());
    let mut declared_outputs = IndexSet::with_capacity(dynamic_lambda.outputs.len());

    let data = TupleRef::from_value(
        dynamic_lambda
            .attributes_lambda
            .owned_value(env.frozen_heap()),
    )
    .unwrap();
    assert_eq!(data.len(), 3);
    let attributes = data.content()[0];
    let plugins = ValueTypedComplex::new(data.content()[1]).unwrap();
    let lambda = data.content()[2];

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

    let fake_registry = DeferredRegistry::new(BaseKey::Base(dynamic_lambda.owner.dupe()));

    let deferred = mem::replace(deferred_ctx.registry(), fake_registry);
    let mut registry = AnalysisRegistry::new_from_owner_and_deferred(
        dynamic_lambda.owner.dupe(),
        execution_platform,
        deferred,
    )?;
    registry.set_action_key(Arc::from(deferred_ctx.get_action_key()));

    let mut artifacts = SmallMap::with_capacity(dynamic_lambda.inputs.len());
    let fs = deferred_ctx.project_filesystem();
    for x in &dynamic_lambda.dynamic {
        let x = match x {
            DeferredInput::MaterializedArtifact(x) => x,
            DeferredInput::ConfiguredTarget(_) => continue,
            _ => unreachable!("DynamicLambda only depends on artifact and target"),
        };
        let k = heap.alloc(StarlarkArtifact::new(x.dupe()));
        let path = deferred_ctx.get_materialized_artifact(x).unwrap();
        let v = heap.alloc(StarlarkArtifactValue::new(
            x.dupe(),
            path.to_owned(),
            fs.dupe(),
        ));
        artifacts.insert_hashed(k.get_hashed()?, v);
    }

    for x in &dynamic_lambda.outputs {
        let k = heap.alloc(StarlarkArtifact::new(Artifact::from(x.dupe())));
        let declared = registry.declare_dynamic_output(x.get_path().dupe(), x.output_type());
        declared_outputs.insert(declared.dupe());
        let v = heap.alloc(StarlarkDeclaredArtifact::new(
            None,
            declared,
            AssociatedArtifacts::new(),
        ));
        outputs.insert_hashed(k.get_hashed()?, v);
    }

    let artifacts = Dict::new(artifacts);
    let outputs = Dict::new(outputs);

    Ok(DynamicLambdaCtxData {
        attributes,
        lambda,
        plugins,
        outputs: heap.alloc(outputs),
        artifacts: heap.alloc(artifacts),
        key: &dynamic_lambda.owner,
        digest_config: deferred_ctx.digest_config(),
        declared_outputs,
        registry,
    })
}
