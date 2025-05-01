/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::digest_config::StarlarkDigestConfig;
use buck2_build_api::interpreter::rule_defs::transitive_set::FrozenTransitiveSetDefinition;
use buck2_build_api::interpreter::rule_defs::transitive_set::TransitiveSet;
use buck2_execute::execute::request::OutputType;
use starlark::environment::MethodsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::FrozenValueTyped;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::values::typing::StarlarkIter;

#[starlark_module]
pub(crate) fn analysis_actions_methods_unsorted(builder: &mut MethodsBuilder) {
    /// Returns an unbound `artifact`, representing where a file will go, which must be bound before analysis terminates.
    /// The usual way of binding an artifact is with `ctx.actions.run`. As an example:
    ///
    /// ```python
    /// my_output = ctx.actions.declare_output("output.o")
    /// ctx.actions.run(["gcc", "-c", my_source, "-o", my_output.as_output()], category = "compile")
    /// ```
    ///
    /// This snippet declares an output with the filename `output.o` (it will be located in the output directory
    /// for this target). Note the use of `as_output` to tag this artifact as being an output in
    /// the action. After binding the artifact you can subsequently use `my_output` as either an
    /// input for subsequent actions, or as the result in a provider.
    ///
    /// Artifacts from a single target may not have the same name, so if you then want a second
    /// artifact also named `output.o` you need to supply a prefix, e.g.
    /// `ctx.actions.declare_output("directory", "output.o")`. The artifact will still report having
    /// name `output.o`, but will be located at `directory/output.o`.
    ///
    /// The `dir` argument should be set to `True` if the binding will be a directory.
    fn declare_output<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] prefix: &str,
        #[starlark(require = pos)] filename: Option<&str>,
        #[starlark(require = named, default = false)] dir: bool,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<StarlarkDeclaredArtifact> {
        // We take either one or two positional arguments, namely (filename) or (prefix, filename).
        // The prefix argument is optional, but first, so we pretend the filename is optional
        // and fix them up here.
        let (prefix, filename) = match filename {
            None => (None, prefix),
            Some(filename) => (Some(prefix), filename),
        };

        let output_type = if dir {
            OutputType::Directory
        } else {
            OutputType::FileOrDirectory
        };
        let artifact = this.state()?.declare_output(
            prefix,
            filename,
            output_type,
            eval.call_stack_top_location(),
        )?;

        Ok(StarlarkDeclaredArtifact::new(
            eval.call_stack_top_location(),
            artifact,
            AssociatedArtifacts::new(),
        ))
    }

    /// Creates a new transitive set. For details, see https://buck2.build/docs/rule_authors/transitive_sets/.
    fn tset<'v>(
        this: &AnalysisActions<'v>,
        #[starlark(require = pos)] definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition>,
        #[starlark(require = named)] value: Option<Value<'v>>,
        #[starlark(require = named)] children: Option<
            ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>,
        >,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, TransitiveSet<'v>>> {
        let mut this = this.state()?;
        this.create_transitive_set(definition, value, children.map(|v| v.get()), eval)
    }

    /// Allocate a new input tag. Used with the `dep_files` argument to `run`.
    fn artifact_tag<'v>(this: &AnalysisActions<'v>) -> starlark::Result<ArtifactTag> {
        let _ = this;
        Ok(ArtifactTag::new())
    }

    /// Obtain this daemon's digest configuration. This allows rules to discover what digests the
    /// daemon may be able to e.g. defer download because they conform to its RE backend's expected
    /// digest format.
    fn digest_config<'v>(this: &AnalysisActions<'v>) -> starlark::Result<StarlarkDigestConfig> {
        Ok(StarlarkDigestConfig {
            digest_config: this.digest_config,
        })
    }
}
