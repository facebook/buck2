/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use buck2_interpreter::types::label::Label;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::codemap::FileSpan;
use starlark::collections::StarlarkHasher;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_type;
use starlark::values::Demand;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;
use thiserror::Error;

use crate::actions::artifact::artifact_type::Artifact;
use crate::artifact_groups::promise::PromiseArtifact;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifactHelpers;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ArtifactFingerprint;
use crate::interpreter::rule_defs::artifact::ArtifactError;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

#[derive(Debug, Error)]
enum PromiseArtifactError {
    #[error("cannot access {1} on unresolved promise artifact ({0})")]
    MethodUnsupported(StarlarkPromiseArtifact, &'static str),
    #[error(
        "tried to expand unresolved promise artifact `{0}` to command line args, this should only happen after starlark objects are frozen and promises are resolved"
    )]
    UnresolvedAddedToCommandLine(StarlarkPromiseArtifact),
    #[error("promise artifact ({0}) cannot be projected")]
    CannotProject(StarlarkPromiseArtifact),
}

/// An artifact wrapper for a StarlarkPromise that will resolve to an artifact
///
/// The `StarlarkPromiseArtifact` is created with `ctx.actions.promise_artifact()` and will
/// not have been bound until promises are resolved at the end of analysis.
#[derive(Debug, NoSerialize, ProvidesStaticType, Trace, Allocative, Clone)]
pub struct StarlarkPromiseArtifact {
    declaration_location: Option<FileSpan>,
    artifact: PromiseArtifact,
}

starlark_simple_value!(StarlarkPromiseArtifact);

impl Display for StarlarkPromiseArtifact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<promise artifact")?;
        if let Some(location) = &self.declaration_location {
            write!(f, " declared at {}", location)?;
        }

        if let Some(v) = self.artifact.get() {
            write!(f, " resolved to {}", v)?;
        }
        write!(f, ">")?;
        Ok(())
    }
}

impl StarlarkPromiseArtifact {
    pub fn new(declaration_location: Option<FileSpan>, artifact: PromiseArtifact) -> Self {
        Self {
            declaration_location,
            artifact,
        }
    }

    fn as_artifact(&self) -> ArtifactGroup {
        match self.artifact.get() {
            Some(artifact) => ArtifactGroup::Artifact(artifact.dupe()),
            None => ArtifactGroup::Promise(self.artifact.dupe()),
        }
    }
}

impl StarlarkArtifactLike for StarlarkPromiseArtifact {
    fn get_bound_artifact(&self) -> anyhow::Result<Artifact> {
        match self.artifact.get() {
            Some(v) => Ok(v.dupe()),
            None => Err(PromiseArtifactError::MethodUnsupported(
                self.clone(),
                "get_bound_artifact",
            )
            .into()),
        }
    }

    /// Gets any associated artifacts that should be materialized along with the bound artifact
    fn get_associated_artifacts(&self) -> Option<&AssociatedArtifacts> {
        None
    }

    fn as_command_line_like(&self) -> &dyn CommandLineArgLike {
        self
    }

    fn fingerprint(&self) -> ArtifactFingerprint<'_> {
        {
            let id = self.artifact.id().dupe();
            ArtifactFingerprint::Promise { id }
        }
    }

    fn as_output_error(&self) -> anyhow::Error {
        ArtifactError::PromiseArtifactAsOutput {
            artifact_repr: self.to_string(),
        }
        .into()
    }
}

impl CommandLineArgLike for StarlarkPromiseArtifact {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        match self.artifact.get() {
            Some(v) => {
                cli.push_arg(ctx.resolve_artifact(&v)?.into_string());
                Ok(())
            }
            None => Err(PromiseArtifactError::UnresolvedAddedToCommandLine(self.clone()).into()),
        }
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        visitor.visit_input(self.as_artifact(), None);
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

impl<'v> StarlarkValue<'v> for StarlarkPromiseArtifact {
    starlark_type!("promise_artifact");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(artifact_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        StarlarkArtifactLike::equals(self, other)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        StarlarkArtifactLike::write_hash(self, hasher)
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }

    fn matches_type(&self, ty: &str) -> bool {
        Self::TYPE == ty || ty == "artifact"
    }
}

/// A single input or output for an action
#[starlark_module]
fn artifact_methods(builder: &mut MethodsBuilder) {
    /// The base name of this artifact. e.g. for an artifact at `foo/bar`, this is `bar`
    #[starlark(attribute)]
    fn basename<'v>(
        this: &StarlarkPromiseArtifact,
        heap: &Heap,
    ) -> anyhow::Result<StringValue<'v>> {
        match this.artifact.get() {
            Some(v) => StarlarkArtifactHelpers::basename(v, heap),
            None => Err(PromiseArtifactError::MethodUnsupported(this.clone(), "basename").into()),
        }
    }

    /// The file extension of this artifact. e.g. for an artifact at foo/bar.sh,
    /// this is `.sh`. If no extension is present, `""` is returned.
    #[starlark(attribute)]
    fn extension<'v>(
        this: &StarlarkPromiseArtifact,
        heap: &Heap,
    ) -> anyhow::Result<StringValue<'v>> {
        match this.artifact.get() {
            Some(v) => StarlarkArtifactHelpers::extension(v, heap),
            None => Err(PromiseArtifactError::MethodUnsupported(this.clone(), "extension").into()),
        }
    }

    /// Whether the artifact represents a source file
    #[starlark(attribute)]
    fn is_source(this: &StarlarkPromiseArtifact) -> anyhow::Result<bool> {
        Ok(false)
    }

    /// The `Label` of the rule that originally created this artifact. May also be None in
    /// the case of source files, or if the artifact has not be used in an action, or if the
    /// action was not created by a rule.
    #[starlark(attribute)]
    fn owner<'v>(this: &StarlarkPromiseArtifact) -> anyhow::Result<Option<Label>> {
        match this.artifact.get() {
            Some(v) => StarlarkArtifactHelpers::owner(v),
            None => Err(PromiseArtifactError::MethodUnsupported(this.clone(), "owner").into()),
        }
    }

    /// Returns a `StarlarkOutputArtifact` instance, or fails if the artifact is
    /// either an `Artifact`, or is a bound `DeclaredArtifact` (You cannot bind twice)
    fn as_output<'v>(
        this: &'v StarlarkPromiseArtifact,
    ) -> anyhow::Result<StarlarkOutputArtifact<'v>> {
        Err(this.as_output_error())
    }

    /// The interesting part of the path, relative to somewhere in the output directory.
    /// For an artifact declared as `foo/bar`, this is `foo/bar`.
    #[starlark(attribute)]
    fn short_path<'v>(
        this: &StarlarkPromiseArtifact,
        heap: &Heap,
    ) -> anyhow::Result<StringValue<'v>> {
        match this.artifact.get() {
            Some(v) => StarlarkArtifactHelpers::short_path(v, heap),
            None => Err(PromiseArtifactError::MethodUnsupported(this.clone(), "short_path").into()),
        }
    }

    /// Create an artifact that lives at path relative from this artifact
    /// For example, if artifact foo is a directory containing a file bar, then foo.project("bar") yields the file bar.
    /// It is possible for projected artifacts to hide the prefix in order to have the short name of the resulting artifact only contain the projected path, by passing hide_prefix = True to project().
    fn project<'v>(
        this: &'v StarlarkPromiseArtifact,
        path: &str,
        #[starlark(require = named, default = false)] hide_prefix: bool,
    ) -> anyhow::Result<StarlarkDeclaredArtifact> {
        let _ = (path, hide_prefix);
        Err(PromiseArtifactError::CannotProject(this.clone()).into())
    }

    // Returns a `StarlarkPromiseArtifact` instance which is identical to the original artifact, except with no associated artifacts
    fn without_associated_artifacts(
        this: &StarlarkPromiseArtifact,
    ) -> anyhow::Result<StarlarkPromiseArtifact> {
        Ok(this.clone())
    }
}
