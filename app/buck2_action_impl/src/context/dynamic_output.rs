/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_artifact::actions::key::ActionIndex;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::BoundBuildArtifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_build_api::dynamic_value::DynamicValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::unpack_artifact::UnpackArtifactOrDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_core::deferred::dynamic::DynamicLambdaResultsKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::BuckErrorContext;
use dupe::Dupe;
use indexmap::IndexSet;
use starlark::environment::MethodsBuilder;
use starlark::starlark_module;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::none::NoneType;
use starlark::values::typing::StarlarkCallable;
use starlark::values::FrozenValue;
use starlark::values::ValueTyped;
use starlark_map::small_map::SmallMap;

use crate::dynamic::dynamic_actions::StarlarkDynamicActions;
use crate::dynamic::dynamic_actions::StarlarkDynamicActionsData;
use crate::dynamic::dynamic_value::StarlarkDynamicValue;
use crate::dynamic::params::DynamicLambdaParams;
use crate::dynamic::params::DynamicLambdaStaticFields;
use crate::dynamic::storage::DynamicLambdaParamsStorageImpl;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum DynamicOutputError {
    #[error("Output list may not be empty")]
    EmptyOutput,
}

pub(crate) struct DynamicActionsOutputArtifactBinder {
    key: DeferredHolderKey,
    index: u32,
}

impl DynamicActionsOutputArtifactBinder {
    pub(crate) fn new(key: &DynamicLambdaResultsKey) -> Self {
        DynamicActionsOutputArtifactBinder {
            key: DeferredHolderKey::DynamicLambda(Arc::new(key.dupe())),
            index: 0,
        }
    }

    pub(crate) fn bind(
        &mut self,
        output: OutputArtifact,
    ) -> buck2_error::Result<BoundBuildArtifact> {
        // We create ActionKeys that point directly to the dynamic_lambda's
        // output rather than our own. This saves the resolution of the key from
        // needing to first lookup our result just to get forwarded to the lambda's result.
        //
        // This means that we are creating ActionKeys for the lambda and it needs to offset
        // its key's index to account for this (see ActionRegistry where this is done).
        //
        // TODO(cjhopman): We should probably combine ActionRegistry and DynamicRegistry (and
        // probably ArtifactGroupRegistry too).
        let bound = output
            .bind(ActionKey::new(
                self.key.dupe(),
                ActionIndex::new(self.index),
            ))?
            .dupe();
        self.index += 1;
        Ok(bound)
    }
}

fn output_artifacts_to_lambda_build_artifacts(
    dynamic_key: &DynamicLambdaResultsKey,
    outputs: IndexSet<OutputArtifact>,
) -> buck2_error::Result<Box<[BoundBuildArtifact]>> {
    let mut bind = DynamicActionsOutputArtifactBinder::new(dynamic_key);

    outputs
        .into_iter()
        .map(|output| bind.bind(output))
        .collect::<buck2_error::Result<_>>()
}

#[starlark_module]
pub(crate) fn analysis_actions_methods_dynamic_output(methods: &mut MethodsBuilder) {
    /// `dynamic_output` allows a rule to use information that was not available when the rule was
    /// first run at analysis time. Examples include things like Distributed ThinLTO (where the
    /// index file is created by another action) or OCaml builds (where the dependencies are created
    /// by `ocamldeps`).
    ///
    /// The arguments are:
    ///
    /// * `dynamic` - a list of artifacts whose values will be available in the function. These will
    ///   be built before the function is run.
    /// * `inputs` - parameter is ignored.
    /// * `outputs` - a list of unbound artifacts (created with `declare_artifact`) which will be
    ///   bound by the function.
    /// * The function argument is given 3 arguments:
    ///   * `ctx` (context) - which is the same as that passed to the initial rule analysis.
    ///   * `artifacts` - using one of the artifacts from `dynamic` (example usage:
    ///     `artifacts[artifact_from_dynamic])` gives an artifact value containing the methods
    ///     `read_string`, `read_lines`, and `read_json` to obtain the values from the disk in
    ///     various formats.  Anything too complex should be piped through a Python script for
    ///     transformation to JSON.
    ///   * `outputs` - using one of the artifacts from the `dynamic_output`'s `outputs` (example
    ///     usage: `outputs[artifact_from_dynamic_output_outputs]`) gives an unbounded artifact. The
    ///     function argument must use its `outputs` argument to bind output artifacts, rather than
    ///     reusing artifacts from the outputs passed into `dynamic_output` directly.
    /// * The function must call `ctx.actions` (probably `ctx.actions.run`) to bind all outputs. It
    ///   can examine the values of the dynamic variables and depends on the inputs.
    ///   * The function will usually be a `def`, as `lambda` in Starlark does not allow statements,
    /// making it quite underpowered. For full details see
    /// https://buck2.build/docs/rule_authors/dynamic_dependencies/.
    ///
    /// Besides dynamic dependencies, there is a second use case for `dynamic_output`: say that you
    /// have some output artifact, and that the analysis to produce the action that outputs that
    /// artifact is expensive, ie takes a lot of CPU time; you would like to skip that work in
    /// builds that do not actually use that artifact.
    ///
    /// This can be accomplished by putting the analysis for that artifact behind a `dynamic_output`
    /// with an empty `dynamic` list. The `dynamic_output`'s function will not be run unless one of
    /// the actions it outputs is actually requested as part of the build.
    fn dynamic_output<'v>(
        this: &'v AnalysisActions<'v>,
        #[starlark(require = named)] dynamic: UnpackListOrTuple<UnpackArtifactOrDeclaredArtifact>,
        #[starlark(require = named)] inputs: Option<
            UnpackListOrTuple<UnpackArtifactOrDeclaredArtifact>,
        >,
        #[starlark(require = named)] outputs: UnpackListOrTuple<&'v StarlarkOutputArtifact>,
        #[starlark(require = named)] f: StarlarkCallable<
            'v,
            (
                FrozenValue,
                SmallMap<StarlarkArtifact, StarlarkArtifactValue>,
                SmallMap<StarlarkArtifact, StarlarkDeclaredArtifact>,
            ),
            NoneType,
        >,
    ) -> starlark::Result<NoneType> {
        // TODO(nga): delete.
        let _unused = inputs;

        // Parameter validation
        if outputs.items.is_empty() {
            return Err(buck2_error::Error::from(DynamicOutputError::EmptyOutput).into());
        }

        // Conversion
        let artifact_values = dynamic
            .items
            .iter()
            .map(|x| x.artifact())
            .collect::<buck2_error::Result<_>>()?;
        let outputs = outputs
            .items
            .iter()
            .map(|x| x.artifact())
            .collect::<buck2_error::Result<_>>()?;

        let attributes = this.attributes;
        let plugins = this.plugins;

        let mut this = this.state()?;

        let execution_platform = this.actions.execution_platform.dupe();

        let lambda_params_storage =
            DynamicLambdaParamsStorageImpl::get(&mut this.analysis_value_storage)?;

        let key = lambda_params_storage.next_dynamic_actions_key()?;
        let outputs = output_artifacts_to_lambda_build_artifacts(&key, outputs)?;

        // Registration
        let lambda_params = DynamicLambdaParams {
            attributes,
            plugins,
            lambda: f.erase(),
            attr_values: None,
            static_fields: DynamicLambdaStaticFields {
                owner: key.owner().dupe(),
                artifact_values,
                dynamic_values: IndexSet::new(),
                outputs,
                execution_platform,
            },
        };
        lambda_params_storage.set_dynamic_actions(key, lambda_params)?;
        Ok(NoneType)
    }

    /// New version of `dynamic_output`.
    ///
    /// This is work in progress, and will eventually replace the old `dynamic_output`.
    fn dynamic_output_new<'v>(
        this: &'v AnalysisActions<'v>,
        #[starlark(require = pos)] dynamic_actions: ValueTyped<'v, StarlarkDynamicActions<'v>>,
    ) -> starlark::Result<StarlarkDynamicValue> {
        let dynamic_actions = dynamic_actions
            .data
            .try_borrow_mut()
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?
            .take()
            .buck_error_context(
                "dynamic_action data can be used only in one `dynamic_output_new` call",
            )?;
        let StarlarkDynamicActionsData {
            attr_values,
            callable,
        } = dynamic_actions;

        let mut this = this.state()?;

        let execution_platform = this.actions.execution_platform.dupe();

        let lambda_params_storage =
            DynamicLambdaParamsStorageImpl::get(&mut this.analysis_value_storage)?;
        let key = lambda_params_storage.next_dynamic_actions_key()?;

        let attr_values = attr_values.bind(&key)?;

        let outputs = attr_values.outputs().into_iter().collect();
        let artifact_values = attr_values.artifact_values();
        let dynamic_values = attr_values.dynamic_values();

        // Registration
        let lambda_params = DynamicLambdaParams {
            attributes: None,
            plugins: None,
            lambda: callable.implementation.erase().to_callable(),
            attr_values: Some((attr_values, callable)),
            static_fields: DynamicLambdaStaticFields {
                owner: key.owner().dupe(),
                artifact_values,
                dynamic_values,
                outputs,
                execution_platform,
            },
        };

        lambda_params_storage.set_dynamic_actions(key.dupe(), lambda_params)?;

        Ok(StarlarkDynamicValue {
            dynamic_value: DynamicValue {
                dynamic_lambda_results_key: key,
            },
        })
    }
}
