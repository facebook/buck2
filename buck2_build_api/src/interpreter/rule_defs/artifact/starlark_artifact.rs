/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::sync::Arc;

use buck2_common::sorted_index_set::SortedIndexSet;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_execute::base_deferred_key::BaseDeferredKey;
use buck2_interpreter::types::label::Label;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use serde::Serialize;
use serde::Serializer;
use starlark::collections::StarlarkHasher;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_type;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::actions::artifact::path::ArtifactPath;
use crate::actions::artifact::Artifact;
use crate::actions::artifact::BaseArtifactKind;
use crate::actions::artifact::OutputArtifact;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ArtifactFingerprint;
use crate::interpreter::rule_defs::artifact::ArtifactError;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

/// A wrapper for an `Artifact` that is guaranteed to be bound, such as outputs
/// from dependencies, or source files.
#[derive(Debug, Dupe, Clone, PartialEq, ProvidesStaticType)]
pub struct StarlarkArtifact {
    pub(crate) artifact: Artifact,
    // A set of ArtifactGroups that should be materialized along with the main artifact
    pub(crate) associated_artifacts: Arc<SortedIndexSet<ArtifactGroup>>,
}

starlark_simple_value!(StarlarkArtifact);

impl<'v> UnpackValue<'v> for StarlarkArtifact {
    fn expected() -> String {
        format!(
            "either {} or {}",
            StarlarkArtifact::get_type_value_static().as_str(),
            StarlarkDeclaredArtifact::get_type_value_static().as_str()
        )
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(x) = value.downcast_ref::<StarlarkArtifact>() {
            Some(x.dupe())
        } else if let Some(x) = value.downcast_ref::<StarlarkDeclaredArtifact>() {
            x.get_bound_artifact().ok().map(|a| StarlarkArtifact {
                artifact: a,
                associated_artifacts: x.associated_artifacts.dupe(),
            })
        } else {
            None
        }
    }
}

impl StarlarkArtifact {
    pub fn new(artifact: Artifact) -> Self {
        StarlarkArtifact {
            artifact,
            associated_artifacts: Default::default(),
        }
    }

    pub fn artifact(&self) -> Artifact {
        self.artifact.dupe()
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
    fn output_artifact(&self) -> anyhow::Result<OutputArtifact> {
        match self.artifact.as_parts().0 {
            BaseArtifactKind::Source(_) => Err(ArtifactError::SourceArtifactAsOutput {
                repr: self.to_string(),
            }
            .into()),
            BaseArtifactKind::Build(b) => Err(ArtifactError::BoundArtifactAsOutput {
                artifact_repr: self.to_string(),
                existing_owner: b.get_path().owner().dupe(),
            }
            .into()),
        }
    }

    // All uses of this are to be migrated in upcoming changes
    fn get_bound_deprecated(&self) -> anyhow::Result<Artifact> {
        Ok(self.artifact.dupe())
    }

    fn get_bound_artifact(&self) -> anyhow::Result<Artifact> {
        Ok(self.artifact.dupe())
    }

    fn get_bound_artifact_and_associated_artifacts(
        &self,
    ) -> anyhow::Result<(Artifact, &Arc<SortedIndexSet<ArtifactGroup>>)> {
        Ok((self.get_bound_artifact()?, &self.associated_artifacts))
    }

    fn as_command_line_like(&self) -> &dyn CommandLineArgLike {
        self
    }

    fn get_artifact_path(&self) -> ArtifactPath<'_> {
        self.artifact.get_path()
    }

    fn fingerprint(&self) -> ArtifactFingerprint<'_> {
        ArtifactFingerprint {
            path: self.artifact.get_path(),
            associated_artifacts: &self.associated_artifacts,
        }
    }
}

impl CommandLineArgLike for StarlarkArtifact {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        cli.add_arg_string(cli.resolve_artifact(&self.artifact)?.into_string());
        Ok(())
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
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
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

impl<'v> StarlarkValue<'v> for StarlarkArtifact {
    starlark_type!("artifact");

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
}

#[derive(Error, Debug)]
enum CannotProject {
    #[error("Source artifacts cannot be projected")]
    SourceArtifact,
    #[error("This artifact was declared by another rule: `{0}`")]
    DeclaredElsewhere(BaseDeferredKey),
}

/// A single input or output for an action
#[starlark_module]
fn artifact_methods(builder: &mut MethodsBuilder) {
    /// The base name of this artifact. e.g. for an artifact at `foo/bar`, this is `bar`
    #[starlark(attribute)]
    fn basename<'v>(this: &'v StarlarkArtifact, heap: &Heap) -> anyhow::Result<StringValue<'v>> {
        this.artifact
            .get_path()
            .with_filename(|filename| Ok(heap.alloc_str(filename?.as_str())))
    }

    /// The file extension of this artifact. e.g. for an artifact at foo/bar.sh,
    /// this is `.sh`. If no extension is present, `""` is returned.
    #[starlark(attribute)]
    fn extension<'v>(this: &StarlarkArtifact, heap: &Heap) -> anyhow::Result<StringValue<'v>> {
        this.artifact.get_path().with_filename(|filename| {
            Ok(match filename?.extension() {
                None => heap.alloc_str(""),
                Some(x) => heap.alloc_str_concat(".", x),
            })
        })
    }

    /// Whether the artifact represents a source file
    #[starlark(attribute)]
    fn is_source(this: &StarlarkArtifact) -> anyhow::Result<bool> {
        Ok(this.artifact.is_source())
    }

    /// The `Label` of the rule that originally created this artifact. May also be None in
    /// the case of source files, or if the artifact has not be used in an action, or if the
    //     /// action was not created by a rule.
    #[starlark(attribute)]
    fn owner<'v>(this: &StarlarkArtifact, heap: &Heap) -> anyhow::Result<Option<Label<'v>>> {
        match this.artifact.owner() {
            None => Ok(None),
            Some(BaseDeferredKey::TargetLabel(target)) => Ok(Some(Label::new(
                heap,
                ConfiguredProvidersLabel::new(target.dupe(), ProvidersName::Default),
            ))),
            Some(BaseDeferredKey::BxlLabel(_)) => Ok(None),
        }
    }

    /// Returns a `StarlarkOutputArtifact` instance, or fails if the artifact is
    /// either an `Artifact`, or is a bound `Artifact` (You cannot bind twice)
    fn as_output(this: &StarlarkArtifact) -> anyhow::Result<StarlarkOutputArtifact> {
        Ok(StarlarkOutputArtifact::new(this.output_artifact()?))
    }

    /// The interesting part of the path, relative to somewhere in the output directory.
    /// For an artifact declared as `foo/bar`, this is `foo/bar`.
    #[starlark(attribute)]
    fn short_path<'v>(this: &'v StarlarkArtifact, heap: &Heap) -> anyhow::Result<StringValue<'v>> {
        this.artifact
            .get_path()
            .with_short_path(|short_path| Ok(heap.alloc_str(short_path.as_str())))
    }

    fn project<'v>(
        this: &'v StarlarkArtifact,
        #[starlark(require = pos)] path: &str,
    ) -> anyhow::Result<StarlarkArtifact> {
        let err = anyhow::Error::from(match this.artifact.owner() {
            Some(owner) => CannotProject::DeclaredElsewhere(owner.dupe()),
            None => CannotProject::SourceArtifact,
        });

        Err(err.context(format!(
            "Cannot project path `{}` in artifact `{}`",
            path, this
        )))
    }
}
