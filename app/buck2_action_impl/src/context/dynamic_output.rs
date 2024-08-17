/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context;
use buck2_artifact::actions::key::ActionIndex;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_artifact::deferred::key::DeferredHolderKey;
use buck2_artifact::dynamic::DynamicLambdaResultsKey;
use buck2_build_api::dynamic::params::DynamicLambdaParams;
use buck2_build_api::dynamic::params::DynamicLambdaStaticFields;
use buck2_build_api::dynamic_value::DynamicValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::unpack_artifact::UnpackArtifactOrDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
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

#[derive(buck2_error::Error, Debug)]
enum DynamicOutputError {
    #[error("Output list may not be empty")]
    EmptyOutput,
}

fn output_artifacts_to_lambda_build_artifacts(
    dynamic_key: &DynamicLambdaResultsKey,
    outputs: IndexSet<OutputArtifact>,
) -> anyhow::Result<Box<[BuildArtifact]>> {
    let dynamic_holder_key = DeferredHolderKey::DynamicLambda(Arc::new(dynamic_key.dupe()));

    outputs
        .iter()
        .enumerate()
        .map(|(output_artifact_index, output)| {
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
                    dynamic_holder_key.dupe(),
                    ActionIndex::new(output_artifact_index.try_into()?),
                ))?
                .as_base_artifact()
                .dupe();
            Ok(bound)
        })
        .collect::<anyhow::Result<_>>()
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
    ) -> anyhow::Result<NoneType> {
        // TODO(nga): delete.
        let _unused = inputs;

        // Parameter validation
        if outputs.items.is_empty() {
            return Err(DynamicOutputError::EmptyOutput.into());
        }

        // Conversion
        let dynamic = dynamic
            .items
            .iter()
            .map(|x| x.artifact())
            .collect::<anyhow::Result<_>>()?;
        let outputs = outputs
            .items
            .iter()
            .map(|x| x.artifact())
            .collect::<anyhow::Result<_>>()?;

        let attributes = this.attributes;
        let plugins = this.plugins;

        let mut this = this.state();

        let key = this.analysis_value_storage.next_dynamic_actions_key()?;
        let outputs = output_artifacts_to_lambda_build_artifacts(&key, outputs)?;

        // Registration
        let lambda_params = DynamicLambdaParams {
            attributes,
            plugins,
            lambda: f.erase(),
            arg: None,
            static_fields: DynamicLambdaStaticFields {
                owner: key.owner().dupe(),
                dynamic,
                dynamic_values: IndexSet::new(),
                outputs,
                execution_platform: this.actions.execution_platform.dupe(),
            },
        };
        this.analysis_value_storage
            .set_dynamic_actions(key, lambda_params)?;
        Ok(NoneType)
    }

    /// New version of `dynamic_output`.
    ///
    /// This is work in progress, and will eventually replace the old `dynamic_output`.
    fn dynamic_output_new<'v>(
        this: &'v AnalysisActions<'v>,
        #[starlark(require = pos)] dynamic_actions: ValueTyped<'v, StarlarkDynamicActions<'v>>,
    ) -> anyhow::Result<StarlarkDynamicValue> {
        let dynamic_actions = dynamic_actions
            .data
            .try_borrow_mut()?
            .take()
            .context("dynamic_action data can be used only in one `dynamic_output_new` call")?;
        let StarlarkDynamicActionsData {
            dynamic,
            dynamic_values,
            outputs,
            arg,
            callable,
        } = dynamic_actions;

        let attributes = this.attributes;
        let plugins = this.plugins;

        let mut this = this.state();

        let key = this.analysis_value_storage.next_dynamic_actions_key()?;
        let outputs = output_artifacts_to_lambda_build_artifacts(&key, outputs)?;

        // Registration
        let lambda_params = DynamicLambdaParams {
            attributes,
            plugins,
            lambda: callable.implementation.erase().to_callable(),
            arg: Some(arg),
            static_fields: DynamicLambdaStaticFields {
                owner: key.owner().dupe(),
                dynamic,
                dynamic_values,
                outputs,
                execution_platform: this.actions.execution_platform.dupe(),
            },
        };

        this.analysis_value_storage
            .set_dynamic_actions(key.dupe(), lambda_params)?;

        Ok(StarlarkDynamicValue {
            dynamic_value: DynamicValue {
                dynamic_lambda_results_key: key,
            },
        })
    }
}
