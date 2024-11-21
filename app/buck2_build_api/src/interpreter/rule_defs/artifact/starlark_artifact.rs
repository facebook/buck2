/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::artifact_type::BaseArtifactKind;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_execute::path::artifact_path::ArtifactPath;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use dupe::Dupe;
use serde::Serialize;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::collections::StarlarkHasher;
use starlark::environment::Methods;
use starlark::environment::MethodsStatic;
use starlark::values::list::UnpackList;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Demand;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Value;

use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use crate::interpreter::rule_defs::artifact::methods::artifact_methods;
use crate::interpreter::rule_defs::artifact::methods::EitherStarlarkArtifact;
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

/// A wrapper for an `Artifact` that is guaranteed to be bound, such as outputs
/// from dependencies, or source files.
#[derive(Debug, Dupe, Clone, PartialEq, ProvidesStaticType, Allocative)]
pub struct StarlarkArtifact {
    pub(crate) artifact: Artifact,
    // A set of ArtifactGroups that should be materialized along with the main artifact
    pub(crate) associated_artifacts: AssociatedArtifacts,
}

starlark_simple_value!(StarlarkArtifact);

impl StarlarkArtifact {
    pub fn new(artifact: Artifact) -> Self {
        StarlarkArtifact {
            artifact,
            associated_artifacts: AssociatedArtifacts::new(),
        }
    }

    pub fn artifact(&self) -> Artifact {
        self.artifact.dupe()
    }

    pub fn get_artifact_path(&self) -> ArtifactPath<'_> {
        self.artifact.get_path()
    }
}

impl Display for StarlarkArtifact {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // FIXME(ndmitchell): This display is not the same as the underlying Artifact, but they should probably be the same
        write!(
            f,
            "<{} ",
            if self.artifact.is_source() {
                "source"
            } else {
                "build artifact"
            }
        )?;

        // Historically this has used the full path whereas everything uses the short path.
        self.artifact
            .get_path()
            .with_full_path(|p| write!(f, "{}", p))?;

        if let Some(owner) = self.artifact.owner() {
            write!(f, " bound to {}", owner)?;
        }

        write!(f, ">")?;

        Ok(())
    }
}

impl Serialize for StarlarkArtifact {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_str(self)
    }
}

impl StarlarkArtifactLike for StarlarkArtifact {
    fn as_output_error(&self) -> buck2_error::Error {
        match self.artifact.as_parts().0 {
            BaseArtifactKind::Source(_) => ArtifactError::SourceArtifactAsOutput {
                repr: self.to_string(),
            }
            .into(),
            BaseArtifactKind::Build(b) => ArtifactError::BoundArtifactAsOutput {
                artifact_repr: self.to_string(),
                existing_owner: b.get_path().owner().owner().dupe(),
            }
            .into(),
        }
    }

    fn get_bound_artifact(&self) -> buck2_error::Result<Artifact> {
        Ok(self.artifact.dupe())
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
        StarlarkArtifactHelpers::basename(&self.artifact, heap)
    }

    fn extension<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>> {
        StarlarkArtifactHelpers::extension(&self.artifact, heap)
    }

    fn is_source<'v>(&'v self) -> buck2_error::Result<bool> {
        Ok(self.artifact.is_source())
    }

    fn owner<'v>(&'v self) -> buck2_error::Result<Option<StarlarkConfiguredProvidersLabel>> {
        StarlarkArtifactHelpers::owner(&self.artifact)
    }

    fn short_path<'v>(&'v self, heap: &'v Heap) -> buck2_error::Result<StringValue<'v>> {
        StarlarkArtifactHelpers::short_path(&self.artifact, heap)
    }

    fn as_output<'v>(
        &'v self,
        _this: Value<'v>,
    ) -> buck2_error::Result<StarlarkOutputArtifact<'v>> {
        match self.artifact.as_parts().0 {
            BaseArtifactKind::Source(_) => Err(ArtifactError::SourceArtifactAsOutput {
                repr: self.to_string(),
            }
            .into()),
            BaseArtifactKind::Build(b) => Err(ArtifactError::BoundArtifactAsOutput {
                artifact_repr: self.to_string(),
                existing_owner: b.get_path().owner().owner().dupe(),
            }
            .into()),
        }
    }

    fn project<'v>(
        &'v self,
        path: &ForwardRelativePath,
        hide_prefix: bool,
    ) -> buck2_error::Result<EitherStarlarkArtifact> {
        Ok(EitherStarlarkArtifact::Artifact(StarlarkArtifact {
            artifact: self.artifact.dupe().project(path, hide_prefix),
            associated_artifacts: self.associated_artifacts.dupe(),
        }))
    }

    fn without_associated_artifacts<'v>(&'v self) -> buck2_error::Result<EitherStarlarkArtifact> {
        Ok(EitherStarlarkArtifact::Artifact(StarlarkArtifact {
            artifact: self.artifact.dupe(),
            associated_artifacts: AssociatedArtifacts::new(),
        }))
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

        Ok(EitherStarlarkArtifact::Artifact(StarlarkArtifact {
            artifact: self.artifact.dupe(),
            associated_artifacts: self.associated_artifacts.union(artifacts),
        }))
    }
}

impl CommandLineArgLike for StarlarkArtifact {
    fn register_me(&self) {
        command_line_arg_like_impl!(StarlarkArtifact::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineContext,
    ) -> buck2_error::Result<()> {
        cli.push_arg(ctx.resolve_artifact(&self.artifact)?.into_string());
        Ok(())
    }

    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> buck2_error::Result<()> {
        visitor.visit_input(ArtifactGroup::Artifact(self.artifact.dupe()), None);
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

#[starlark_value(type = "artifact")]
impl<'v> StarlarkValue<'v> for StarlarkArtifact {
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

pub(crate) struct StarlarkArtifactHelpers;
impl StarlarkArtifactHelpers {
    /// The base name of this artifact. e.g. for an artifact at `foo/bar`, this is `bar`
    pub(crate) fn basename<'v>(
        artifact: &Artifact,
        heap: &'v Heap,
    ) -> buck2_error::Result<StringValue<'v>> {
        artifact
            .get_path()
            .with_filename(|filename| Ok(heap.alloc_str(filename?.as_str())))
    }

    /// The file extension of this artifact. e.g. for an artifact at foo/bar.sh,
    /// this is `.sh`. If no extension is present, `""` is returned.
    pub(crate) fn extension<'v>(
        artifact: &Artifact,
        heap: &'v Heap,
    ) -> buck2_error::Result<StringValue<'v>> {
        artifact.get_path().with_filename(|filename| {
            Ok(StarlarkArtifactHelpers::alloc_extension(
                filename?.extension(),
                heap,
            ))
        })
    }

    /// The `Label` of the rule that originally created this artifact. May also be None in
    /// the case of source files, or if the artifact has not be used in an action, or if the
    /// action was not created by a rule.
    pub(crate) fn owner(
        artifact: &Artifact,
    ) -> buck2_error::Result<Option<StarlarkConfiguredProvidersLabel>> {
        match artifact.owner() {
            None => Ok(None),
            Some(BaseDeferredKey::TargetLabel(target)) => {
                Ok(Some(StarlarkConfiguredProvidersLabel::new(
                    ConfiguredProvidersLabel::new(target.dupe(), ProvidersName::Default),
                )))
            }
            Some(BaseDeferredKey::AnonTarget(_) | BaseDeferredKey::BxlLabel(_)) => Ok(None),
        }
    }

    /// The interesting part of the path, relative to somewhere in the output directory.
    /// For an artifact declared as `foo/bar`, this is `foo/bar`.
    pub(crate) fn short_path<'v>(
        artifact: &Artifact,
        heap: &'v Heap,
    ) -> buck2_error::Result<StringValue<'v>> {
        artifact
            .get_path()
            .with_short_path(|short_path| Ok(heap.alloc_str(short_path.as_str())))
    }

    pub(crate) fn alloc_extension<'v>(extension: Option<&str>, heap: &'v Heap) -> StringValue<'v> {
        match extension {
            None => heap.alloc_str(""),
            Some(x) => heap.alloc_str_concat(".", x),
        }
    }
}
