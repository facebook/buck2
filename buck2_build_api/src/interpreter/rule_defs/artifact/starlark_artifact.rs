/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

use buck2_core::{
    fs::paths::{ForwardRelativePath, ForwardRelativePathBuf},
    provider::{ConfiguredProvidersLabel, ProvidersName},
};
use either::Either;
use gazebo::{any::AnyLifetime, cell::ARef, prelude::*};
use serde::{Serialize, Serializer};
use starlark::{
    collections::StarlarkHasher,
    environment::{Methods, MethodsBuilder, MethodsStatic},
    starlark_type,
    values::{StarlarkValue, StringValue, UnpackValue, Value, ValueLike},
};
use thiserror::Error;

use crate::{
    actions::artifact::{Artifact, ArtifactKind, OutputArtifact},
    artifact_groups::ArtifactGroup,
    deferred::BaseDeferredKey,
    interpreter::rule_defs::{
        artifact::{
            ArtifactError, StarlarkArtifactLike, StarlarkDeclaredArtifact, StarlarkOutputArtifact,
        },
        cmd_args::{
            CommandLineArgLike, CommandLineArtifactVisitor, CommandLineBuilder,
            WriteToFileMacroVisitor,
        },
        label::Label,
    },
    path::{BuckOutPath, BuckPath},
};

#[derive(Error, Debug)]
enum ArtifactErrors {
    #[error("Artifact has no filename component, `{0}`")]
    EmptyFilename(ForwardRelativePathBuf),
}

/// A wrapper for an `Artifact` that is guaranteed to be bound, such as outputs
/// from dependencies, or source files.
#[derive(Debug, Dupe, Clone, PartialEq, AnyLifetime)]
pub struct StarlarkArtifact {
    pub(crate) artifact: Artifact,
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
            x.get_bound().ok().map(StarlarkArtifact::new)
        } else {
            None
        }
    }
}

impl StarlarkArtifact {
    pub fn new(artifact: Artifact) -> Self {
        StarlarkArtifact { artifact }
    }

    fn get_path(&self) -> &ForwardRelativePath {
        match self.artifact.0.as_ref() {
            ArtifactKind::Source(s) => s.get_path().path().as_ref(),
            ArtifactKind::Build(b) => b.get_path().path(),
        }
    }

    pub fn artifact(&self) -> Artifact {
        self.artifact.dupe()
    }
}

impl Display for StarlarkArtifact {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // FIXME(ndmitchell): This display is not the same as the underlying Artifact, but they should probably be the same
        match self.artifact.0.as_ref() {
            ArtifactKind::Source(s) => {
                let package = s.get_path().package().cell_relative_path().as_str();
                write!(f, "<source {}", package)?;
                if !package.is_empty() {
                    write!(f, "/")?;
                }
                write!(f, "{}>", s.get_path().path().as_str())
            }
            ArtifactKind::Build(b) => {
                write!(
                    f,
                    "<build artifact {} bound to {}>",
                    b.get_path().path().as_str(),
                    b.get_path().owner()
                )
            }
        }
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
        match self.artifact.0.as_ref() {
            ArtifactKind::Source(_) => Err(ArtifactError::SourceArtifactAsOutput {
                repr: self.to_string(),
            }
            .into()),
            ArtifactKind::Build(b) => Err(ArtifactError::BoundArtifactAsOutput {
                artifact_repr: self.to_string(),
                existing_owner: b.get_path().owner().dupe(),
            }
            .into()),
        }
    }

    fn get_bound(&self) -> anyhow::Result<Artifact> {
        Ok(self.artifact.dupe())
    }

    fn as_command_line_like(&self) -> &dyn CommandLineArgLike {
        self
    }

    fn fingerprint(&self) -> Either<ARef<BuckOutPath>, &BuckPath> {
        match self.artifact.0.as_ref() {
            ArtifactKind::Source(x) => Either::Right(x.get_path()),
            ArtifactKind::Build(x) => Either::Left(ARef::new_ptr(x.get_path())),
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

    fn get_methods(&self) -> Option<&'static Methods> {
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

/// A single input or output for an action
#[starlark_module]
fn artifact_methods(builder: &mut MethodsBuilder) {
    /// The base name of this artifact. e.g. for an artifact at `foo/bar`, this is `bar`
    #[starlark(attribute)]
    fn basename<'v>(this: &'v StarlarkArtifact) -> anyhow::Result<&'v str> {
        match this.get_path().file_name() {
            Some(x) => Ok(x.as_str()),
            None => Err(ArtifactErrors::EmptyFilename(this.get_path().to_owned()).into()),
        }
    }

    /// The file extension of this artifact. e.g. for an artifact at foo/bar.sh,
    /// this is `.sh`. If no extension is present, `""` is returned.
    #[starlark(attribute)]
    fn extension<'v>(this: &StarlarkArtifact) -> anyhow::Result<StringValue<'v>> {
        match this.get_path().extension() {
            None => Ok(heap.alloc_str("")),
            Some(x) => Ok(heap.alloc_str_concat(".", x)),
        }
    }

    /// Whether the artifact represents a source file
    #[starlark(attribute)]
    fn is_source(this: &StarlarkArtifact) -> anyhow::Result<bool> {
        match this.artifact.0.as_ref() {
            ArtifactKind::Source(_) => Ok(true),
            ArtifactKind::Build(_) => Ok(false),
        }
    }

    /// The `Label` of the rule that originally created this artifact. May also be None in
    /// the case of source files, or if the artifact has not be used in an action, or if the
    //     /// action was not created by a rule.
    #[starlark(attribute)]
    fn owner<'v>(this: &StarlarkArtifact) -> anyhow::Result<Option<Label<'v>>> {
        match this.artifact.0.as_ref() {
            ArtifactKind::Source(_) => Ok(None),
            ArtifactKind::Build(b) => match b.get_path().owner() {
                BaseDeferredKey::TargetLabel(target) => Ok(Some(Label::new(
                    heap,
                    ConfiguredProvidersLabel::new(target.dupe(), ProvidersName::Default),
                ))),
                BaseDeferredKey::BxlLabel(_) => Ok(None),
            },
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
    fn short_path<'v>(this: &'v StarlarkArtifact) -> anyhow::Result<&'v str> {
        match this.artifact.0.as_ref() {
            ArtifactKind::Source(x) => Ok(x.get_path().path().as_str()),
            ArtifactKind::Build(x) => Ok(x.get_path().short_path().as_str()),
        }
    }
}
