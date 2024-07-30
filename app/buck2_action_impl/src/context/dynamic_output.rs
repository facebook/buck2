/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_build_api::dynamic::params::DynamicLambdaParams;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::unpack_artifact::UnpackArtifactOrDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use starlark::environment::MethodsBuilder;
use starlark::starlark_module;
use starlark::values::any_complex::StarlarkAnyComplex;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::none::NoneType;
use starlark::values::typing::StarlarkCallable;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::ValueTyped;
use starlark_map::small_map::SmallMap;

use crate::dynamic::dynamic_actions::StarlarkDynamicActions;
use crate::dynamic::dynamic_actions::StarlarkDynamicActionsData;

#[derive(buck2_error::Error, Debug)]
enum DynamicOutputError {
    #[error("Output list may not be empty")]
    EmptyOutput,
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
        heap: &'v Heap,
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
        let outputs = outputs.items.iter().map(|x| x.artifact()).collect();

        // Registration
        let attributes_plugins_lambda = heap.alloc(StarlarkAnyComplex::new(DynamicLambdaParams {
            attributes: this.attributes,
            plugins: this.plugins,
            lambda: f.erase(),
            arg: None,
        }));
        let mut this = this.state();
        this.register_dynamic_output(dynamic, outputs, attributes_plugins_lambda)?;
        Ok(NoneType)
    }

    /// New version of `dynamic_output`.
    ///
    /// This is work in progress, and will eventually replace the old `dynamic_output`.
    fn dynamic_output_new<'v>(
        this: &'v AnalysisActions<'v>,
        #[starlark(require = pos)] dynamic_actions: ValueTyped<'v, StarlarkDynamicActions<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<NoneType> {
        let dynamic_actions = dynamic_actions
            .data
            .try_borrow_mut()?
            .take()
            .context("dynamic_action data can be used only in one `dynamic_output_new` call")?;
        let StarlarkDynamicActionsData {
            dynamic,
            outputs,
            arg,
            callable,
        } = dynamic_actions;
        let attributes_plugin_lambda = heap.alloc(StarlarkAnyComplex::new(DynamicLambdaParams {
            lambda: callable.implementation.erase().to_callable(),
            attributes: this.attributes,
            plugins: this.plugins,
            arg: Some(arg),
        }));

        let mut this = this.state();
        this.register_dynamic_output(dynamic, outputs, attributes_plugin_lambda)?;

        Ok(NoneType)
    }
}
