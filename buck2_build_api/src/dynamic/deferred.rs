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
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_execute::base_deferred_key::BaseDeferredKey;
use buck2_interpreter::types::label::LabelGen;
use gazebo::prelude::*;
use indexmap::indexset;
use indexmap::IndexSet;
use starlark::collections::SmallMap;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::dict::Dict;
use starlark::values::tuple::Tuple;
use starlark::values::OwnedFrozenValue;
use thiserror::Error;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::artifact::Artifact;
use crate::actions::key::ActionKey;
use crate::actions::RegisteredAction;
use crate::analysis::registry::AnalysisRegistry;
use crate::bxl;
use crate::deferred::types::BaseKey;
use crate::deferred::types::Deferred;
use crate::deferred::types::DeferredCtx;
use crate::deferred::types::DeferredData;
use crate::deferred::types::DeferredInput;
use crate::deferred::types::DeferredRegistry;
use crate::deferred::types::DeferredValue;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactValue;
use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::context::AnalysisContext;

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
pub(crate) struct DynamicLambda {
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
                // do nothing. This is for grabbing the execution platform, which for bxl, we
                // hard code to a local execution.
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
pub(crate) struct DynamicLambdaOutput {
    /// The actions the DynamicLambda produces, in the right order.
    /// `DynamicAction.index` is an index into this Vec.
    output: Vec<ActionKey>,
}

impl Deferred for DynamicAction {
    type Output = Arc<RegisteredAction>;

    fn inputs(&self) -> &IndexSet<DeferredInput> {
        &self.inputs
    }

    fn execute(&self, ctx: &mut dyn DeferredCtx) -> anyhow::Result<DeferredValue<Self::Output>> {
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

impl Deferred for DynamicLambda {
    type Output = DynamicLambdaOutput;

    fn inputs(&self) -> &IndexSet<DeferredInput> {
        &self.dynamic
    }

    fn execute(
        &self,
        deferred_ctx: &mut dyn DeferredCtx,
    ) -> anyhow::Result<DeferredValue<Self::Output>> {
        let env = Module::new();
        let heap = env.heap();
        let mut eval = Evaluator::new(&env);

        let data =
            Tuple::from_value(self.attributes_lambda.owned_value(env.frozen_heap())).unwrap();
        assert_eq!(data.len(), 2);
        let attributes = data.content()[0];
        let lambda = data.content()[1];

        let execution_platform = {
            match &self.owner {
                BaseDeferredKey::TargetLabel(target) => {
                    let configured_target = deferred_ctx.get_configured_target(target).unwrap();

                    configured_target.execution_platform_resolution().dupe()
                }
                BaseDeferredKey::BxlLabel(_) => {
                    (*bxl::execution_platform::EXECUTION_PLATFORM).dupe()
                }
                BaseDeferredKey::AnonTarget(_) => {
                    return Err(DynamicLambdaError::AnonTargetIncompatible.into());
                }
            }
        };

        // The DeferredCtx has a registry it wants us to use as &mut.
        // The AnalysisRegistry wants ownership of a registry.
        // To overcome the difference, we create a fake registry, swap it with the one in deferred,
        // and swap back after AnalysisRegistry completes.
        let fake_registry = DeferredRegistry::new(BaseKey::Base(self.owner.dupe()));

        let deferred = mem::replace(deferred_ctx.registry(), fake_registry);
        let mut registry = AnalysisRegistry::new_from_owner_and_deferred(
            self.owner.dupe(),
            execution_platform,
            deferred,
        );
        registry.set_action_key(Arc::from(deferred_ctx.get_action_key()));

        let mut artifacts = SmallMap::with_capacity(self.inputs.len());
        let fs = deferred_ctx.project_filesystem();
        for x in &self.dynamic {
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

        let mut outputs = SmallMap::with_capacity(self.outputs.len());
        let mut declared_outputs = IndexSet::with_capacity(self.outputs.len());
        for x in &self.outputs {
            let k = heap.alloc(StarlarkArtifact::new(Artifact::from(x.dupe())));
            let declared = registry.declare_dynamic_output(x.get_path().dupe(), x.output_type());
            declared_outputs.insert(declared.dupe());
            let v = heap.alloc(StarlarkDeclaredArtifact::new(
                None,
                declared,
                Default::default(),
            ));
            outputs.insert_hashed(k.get_hashed()?, v);
        }

        let artifacts = Dict::new(artifacts);
        let outputs = Dict::new(outputs);

        let ctx = heap.alloc_typed(AnalysisContext::new_dynamic(
            heap,
            attributes,
            match &self.owner {
                BaseDeferredKey::TargetLabel(target) => Some(heap.alloc_typed(LabelGen::new(
                    heap,
                    ConfiguredProvidersLabel::new(target.dupe(), ProvidersName::Default),
                ))),
                BaseDeferredKey::BxlLabel(_) => None,
                BaseDeferredKey::AnonTarget(target) => Some(heap.alloc_typed(LabelGen::new(
                    heap,
                    ConfiguredProvidersLabel::new(
                        target.configured_label(),
                        ProvidersName::Default,
                    ),
                ))),
            },
            registry,
        ));

        eval.eval_function(
            lambda,
            &[ctx.to_value(), heap.alloc(artifacts), heap.alloc(outputs)],
            &[],
        )?;
        ctx.assert_no_promises()?;

        let analysis_registry = ctx.take_state();

        let (_frozen_env, deferred) = analysis_registry.finalize(&env)(env)?;
        let _fake_registry = mem::replace(deferred_ctx.registry(), deferred);

        // TODO(ndmitchell): Check we don't use anything not in `inputs`

        let output: anyhow::Result<Vec<_>> = declared_outputs
            .into_iter()
            .map(|x| anyhow::Ok(x.ensure_bound()?.action_key().dupe()))
            .collect();
        Ok(DeferredValue::Ready(DynamicLambdaOutput {
            output: output?,
        }))
    }
}
