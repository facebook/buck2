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
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::DeclaredArtifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_error::buck2_error;
use buck2_error::BuckErrorContext;
use buck2_execute::path::artifact_path::ArtifactPath;
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
use starlark::values::AllocValue;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueTyped;

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

/// A wrapper for an artifact that has been declared in the user's implementation function.
///
/// The `DeclaredArtifact` may or may not have been bound yet, but must be by the end of the
/// user's implementation function. This turns into a `StarlarkArtifact` when frozen, and
/// /must/ have had `ensure_bound()` called on it successfully before freezing.
#[derive(Clone, Debug, Dupe, NoSerialize, ProvidesStaticType, Trace, Allocative)]
pub struct StarlarkDeclaredArtifact {
    // FileSpan is not Hash, and the location is not really relevant for equality.
    // We also expect each artifact to be declared at one unique location anyway.
    pub(super) declaration_location: Option<FileSpan>,
    pub(super) artifact: DeclaredArtifact,
    // A set of ArtifactGroups that should be materialized along with the main artifact
    pub(super) associated_artifacts: AssociatedArtifacts,
}

impl Display for StarlarkDeclaredArtifact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<build artifact {}", self.artifact)?;
        if let Some(location) = &self.declaration_location {
            write!(f, " declared at {}", location)?;
        }
        if let Some(label) = self.artifact.owner() {
            write!(f, " bound to {}", label)?;
        }
        write!(f, ">")
    }
}

impl StarlarkDeclaredArtifact {
    pub fn new(
        declaration_location: Option<FileSpan>,
        artifact: DeclaredArtifact,
        associated_artifacts: AssociatedArtifacts,
    ) -> Self {
        StarlarkDeclaredArtifact {
            declaration_location,
            artifact,
            associated_artifacts,
        }
    }

    pub fn output_artifact(&self) -> OutputArtifact {
        self.artifact.as_output()
    }

    pub fn get_artifact_path(&self) -> ArtifactPath<'_> {
        self.artifact.get_path()
    }

    pub fn with_extended_associated_artifacts(
        &self,
        extra_associated_artifacts: AssociatedArtifacts,
    ) -> Self {
        let merged = self.associated_artifacts.union(extra_associated_artifacts);
        Self {
            declaration_location: self.declaration_location.clone(),
            artifact: self.artifact.dupe(),
            associated_artifacts: merged,
        }
    }
}

impl StarlarkArtifactLike for StarlarkDeclaredArtifact {
    fn as_output_error(&self) -> buck2_error::Error {
        // This shouldn't ever be called for StarlarkDeclaredArtifact
        buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "error trying to use declared artifact as an output, this indicates an internal buck error"
        )
    }

    fn get_bound_artifact(&self) -> buck2_error::Result<Artifact> {
        Ok(self.artifact.dupe().ensure_bound()?.into_artifact())
    }

    fn get_associated_artifacts(&self) -> Option<&AssociatedArtifacts> {
        Some(&self.associated_artifacts)
    }

    fn as_command_line_like(&self) -> &dyn CommandLineArgLike {
        self
    }

    fn fingerprint(&self) -> ArtifactFingerprint<'_> {
        {
            let path = self.artifact.get_path();
            let associated_artifacts = self.get_associated_artifacts();
            ArtifactFingerprint::Normal {
                path,
                associated_artifacts,
            }
        }
    }

    fn get_artifact_group(&self) -> buck2_error::Result<ArtifactGroup> {
        Ok(ArtifactGroup::Artifact(self.get_bound_artifact()?))
    }

    fn basename<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>> {
        self.artifact
            .get_path()
            .with_filename(|filename| Ok(heap.alloc_str(filename?.as_str())))
    }

    fn extension<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>> {
        self.artifact.get_path().with_filename(|filename| {
            Ok(StarlarkArtifactHelpers::alloc_extension(
                filename?.extension(),
                heap,
            ))
        })
    }

    fn is_source<'v>(&'v self) -> buck2_error::Result<bool> {
        Ok(false)
    }

    fn owner<'v>(&'v self) -> buck2_error::Result<Option<StarlarkConfiguredProvidersLabel>> {
        match self.artifact.owner() {
            None => Ok(None),
            Some(x) => Ok(match x {
                BaseDeferredKey::TargetLabel(t) => Some(StarlarkConfiguredProvidersLabel::new(
                    ConfiguredProvidersLabel::new(t, ProvidersName::Default),
                )),
                BaseDeferredKey::AnonTarget(_) | BaseDeferredKey::BxlLabel(_) => None,
            }),
        }
    }

    fn short_path<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>> {
        self.artifact
            .get_path()
            .with_short_path(|short_path| Ok(heap.alloc_str(short_path.as_str())))
    }

    fn as_output<'v>(&'v self, this: Value<'v>) -> buck2_error::Result<StarlarkOutputArtifact<'v>> {
        Ok(StarlarkOutputArtifact::new(
            ValueTyped::<StarlarkDeclaredArtifact>::new_err(this)
                .internal_error("Type must have been checked earlier")?,
        ))
    }

    fn project<'v>(
        &'v self,
        path: &ForwardRelativePath,
        hide_prefix: bool,
    ) -> buck2_error::Result<EitherStarlarkArtifact> {
        // Not sure if this.declaration_location is or the project() call is more appropriate here.
        Ok(EitherStarlarkArtifact::DeclaredArtifact(
            StarlarkDeclaredArtifact {
                declaration_location: self.declaration_location.dupe(),
                artifact: self.artifact.project(path, hide_prefix),
                associated_artifacts: self.associated_artifacts.dupe(),
            },
        ))
    }

    fn without_associated_artifacts<'v>(&'v self) -> buck2_error::Result<EitherStarlarkArtifact> {
        Ok(EitherStarlarkArtifact::DeclaredArtifact(
            StarlarkDeclaredArtifact {
                declaration_location: self.declaration_location.dupe(),
                artifact: self.artifact.dupe(),
                associated_artifacts: AssociatedArtifacts::new(),
            },
        ))
    }

    fn with_associated_artifacts<'v>(
        &'v self,
        artifacts: UnpackList<ValueAsArtifactLike<'v>>,
    ) -> buck2_error::Result<EitherStarlarkArtifact> {
        let artifacts = artifacts
            .items
            .iter()
            .map(|a| a.0.get_artifact_group())
            .collect::<Result<Vec<_>, _>>()?;

        let artifacts = AssociatedArtifacts::from(artifacts);

        Ok(EitherStarlarkArtifact::DeclaredArtifact(
            StarlarkDeclaredArtifact {
                declaration_location: self.declaration_location.dupe(),
                artifact: self.artifact.dupe(),
                associated_artifacts: self.associated_artifacts.union(artifacts),
            },
        ))
    }
}

impl CommandLineArgLike for StarlarkDeclaredArtifact {
    fn register_me(&self) {
        command_line_arg_like_impl!(StarlarkDeclaredArtifact::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        _cli: &mut dyn CommandLineBuilder,
        _ctx: &mut dyn CommandLineContext,
    ) -> buck2_error::Result<()> {
        // TODO: proper error message
        Err(buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "proper error here; we should not be adding mutable starlark objects to clis"
        ))
    }

    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> buck2_error::Result<()> {
        visitor.visit_input(
            ArtifactGroup::Artifact(self.artifact.dupe().ensure_bound()?.into_artifact()),
            None,
        );
        self.associated_artifacts
            .iter()
            .for_each(|ag| visitor.visit_input(ag.dupe(), None));
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

impl Freeze for StarlarkDeclaredArtifact {
    type Frozen = StarlarkArtifact;
    fn freeze(self, _freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        // ensure_bound() moves out of self and so we can't construct the error
        // after calling that, so we need to check first.
        if !self.artifact.is_bound() {
            return Err(FreezeError::new(
                ArtifactError::DeclaredArtifactWasNotBound {
                    repr: self.to_string(),
                }
                .to_string(),
            ));
        }
        // TODO(minglunli): Check to see if the error carries a context
        let artifact = match self.artifact.ensure_bound() {
            Ok(artifact) => artifact,
            Err(e) => return Err(FreezeError::new(e.to_string())),
        }
        .into_artifact();
        Ok(StarlarkArtifact {
            artifact,
            associated_artifacts: self.associated_artifacts,
        })
    }
}

impl<'v> AllocValue<'v> for StarlarkDeclaredArtifact {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

#[starlark_value(type = "artifact", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkDeclaredArtifact {
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
