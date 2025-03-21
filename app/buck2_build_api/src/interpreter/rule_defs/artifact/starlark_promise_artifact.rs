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
use std::sync::Arc;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::codemap::FileSpan;
use starlark::collections::StarlarkHasher;
use starlark::environment::Methods;
use starlark::environment::MethodsStatic;
use starlark::values::list::UnpackList;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Demand;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;

use crate::artifact_groups::promise::PromiseArtifact;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use crate::interpreter::rule_defs::artifact::methods::artifact_methods;
use crate::interpreter::rule_defs::artifact::methods::EitherStarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifactHelpers;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ArtifactFingerprint;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::ArtifactError;
use crate::interpreter::rule_defs::cmd_args::command_line_arg_like_type::command_line_arg_like_impl;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum PromiseArtifactError {
    #[error("cannot access {1} on unresolved promise artifact ({0})")]
    MethodUnsupported(StarlarkPromiseArtifact, &'static str),
    #[error(
        "tried to expand unresolved promise artifact `{0}` to command line args, this should only happen after starlark objects are frozen and promises are resolved"
    )]
    UnresolvedAddedToCommandLine(StarlarkPromiseArtifact),
    #[error("promise artifact ({0}) cannot be projected")]
    CannotProject(StarlarkPromiseArtifact),
    #[error(
        "`short_path` was called on promise artifact ({0}), but no short path was promised. To access the short path on an unresolved promise artifact, `short_path` arg needs to be passed into `artifact_promise()`"
    )]
    NoShortPathPromised(StarlarkPromiseArtifact),
    #[error("The promise artifact ({0}) short_path has no file name")]
    PromisedShortPathHasNoFileName(StarlarkPromiseArtifact),
    #[error("Promise artifacts do not support associated artifacts")]
    CannotAddAssociatedArtifacts,
}

/// An artifact wrapper for a StarlarkPromise that will resolve to an artifact
///
/// The `StarlarkPromiseArtifact` is created with `ctx.actions.artifact_promise()` and will
/// not have been bound until promises are resolved at the end of analysis.
#[derive(
    Debug,
    NoSerialize,
    ProvidesStaticType,
    Trace,
    Allocative,
    Clone,
    Hash,
    Eq,
    PartialEq
)]
pub struct StarlarkPromiseArtifact {
    pub declaration_location: Option<FileSpan>,
    pub artifact: PromiseArtifact,
    pub short_path: Option<ForwardRelativePathBuf>,
}

starlark_simple_value!(StarlarkPromiseArtifact);

impl Display for StarlarkPromiseArtifact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<promise artifact")?;
        if let Some(location) = &self.declaration_location {
            write!(f, " declared at {}", location)?;
        }
        if let Some(short_path) = &self.short_path {
            write!(f, " with short_path `{}`", short_path)?;
        }
        if let Some(v) = self.artifact.get() {
            write!(f, " resolved to {}", v)?;
        }
        write!(f, ">")?;
        Ok(())
    }
}

impl StarlarkPromiseArtifact {
    pub fn new(
        declaration_location: Option<FileSpan>,
        artifact: PromiseArtifact,
        short_path: Option<ForwardRelativePathBuf>,
    ) -> Self {
        Self {
            declaration_location,
            artifact,
            short_path,
        }
    }

    pub fn as_artifact(&self) -> ArtifactGroup {
        match self.artifact.get() {
            Some(artifact) => ArtifactGroup::Artifact(artifact.dupe()),
            None => ArtifactGroup::Promise(Arc::new(self.artifact.dupe())),
        }
    }

    fn short_path_err(&self) -> buck2_error::Result<&ForwardRelativePath> {
        self.short_path
            .as_deref()
            .with_buck_error_context(|| PromiseArtifactError::NoShortPathPromised(self.clone()))
    }

    fn file_name_err(&self) -> buck2_error::Result<&FileName> {
        self.short_path_err()?
            .file_name()
            .with_buck_error_context(|| {
                PromiseArtifactError::PromisedShortPathHasNoFileName(self.clone())
            })
    }
}

impl StarlarkArtifactLike for StarlarkPromiseArtifact {
    fn get_bound_artifact(&self) -> buck2_error::Result<Artifact> {
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

    fn as_output_error(&self) -> buck2_error::Error {
        ArtifactError::PromiseArtifactAsOutput {
            artifact_repr: self.to_string(),
        }
        .into()
    }

    fn get_artifact_group(&self) -> buck2_error::Result<ArtifactGroup> {
        Ok(self.as_artifact())
    }

    fn basename<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>> {
        match self.artifact.get() {
            Some(v) => StarlarkArtifactHelpers::basename(v, heap),
            None => Ok(heap.alloc_str(self.file_name_err()?.as_str())),
        }
    }

    fn extension<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>> {
        match self.artifact.get() {
            Some(v) => StarlarkArtifactHelpers::extension(v, heap),
            None => Ok(StarlarkArtifactHelpers::alloc_extension(
                self.file_name_err()?.extension(),
                heap,
            )),
        }
    }

    fn is_source<'v>(&'v self) -> buck2_error::Result<bool> {
        Ok(false)
    }

    fn owner<'v>(&'v self) -> buck2_error::Result<Option<StarlarkConfiguredProvidersLabel>> {
        match self.artifact.get() {
            Some(v) => StarlarkArtifactHelpers::owner(v),
            None => Err(PromiseArtifactError::MethodUnsupported(self.clone(), "owner").into()),
        }
    }

    fn short_path<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>> {
        match self.artifact.get() {
            Some(v) => StarlarkArtifactHelpers::short_path(v, heap),
            None => Ok(heap.alloc_str(self.short_path_err()?.as_str())),
        }
    }

    fn as_output<'v>(
        &'v self,
        _this: Value<'v>,
    ) -> buck2_error::Result<StarlarkOutputArtifact<'v>> {
        Err(self.as_output_error())
    }

    fn project<'v>(
        &'v self,
        path: &ForwardRelativePath,
        hide_prefix: bool,
    ) -> buck2_error::Result<EitherStarlarkArtifact> {
        let _ = (path, hide_prefix);
        Err(PromiseArtifactError::CannotProject(self.clone()).into())
    }

    fn without_associated_artifacts<'v>(&'v self) -> buck2_error::Result<EitherStarlarkArtifact> {
        Ok(EitherStarlarkArtifact::PromiseArtifact(self.clone()))
    }

    fn with_associated_artifacts<'v>(
        &'v self,
        artifacts: UnpackList<ValueAsArtifactLike<'v>>,
    ) -> buck2_error::Result<EitherStarlarkArtifact> {
        let _unused = artifacts;
        Err(PromiseArtifactError::CannotAddAssociatedArtifacts.into())
    }
}

impl CommandLineArgLike for StarlarkPromiseArtifact {
    fn register_me(&self) {
        command_line_arg_like_impl!(StarlarkPromiseArtifact::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineContext,
    ) -> buck2_error::Result<()> {
        match self.artifact.get() {
            Some(v) => {
                cli.push_arg(ctx.resolve_artifact(v)?.into_string());
                Ok(())
            }
            None => Err(PromiseArtifactError::UnresolvedAddedToCommandLine(self.clone()).into()),
        }
    }

    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> buck2_error::Result<()> {
        visitor.visit_input(self.as_artifact(), None);
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}

#[starlark_value(type = "promise_artifact")]
impl<'v> StarlarkValue<'v> for StarlarkPromiseArtifact {
    type Canonical = StarlarkArtifact;

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(artifact_methods)
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        StarlarkArtifactLike::equals(self, other)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        StarlarkArtifactLike::write_hash(self, hasher)
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }
}
