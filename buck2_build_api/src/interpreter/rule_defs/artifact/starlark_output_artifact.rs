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
use anyhow::Context;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use starlark::starlark_type;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::actions::artifact::Artifact;
use crate::actions::artifact::BoundBuildArtifact;
use crate::actions::artifact::OutputArtifact;
use crate::interpreter::rule_defs::artifact::ArtifactError;
use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilderContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

/// Thin wrapper around `OutputArtifact`.
///
/// Allows actions to distinguish between inputs and outputs, and can validate whether the
/// underlying artifact is bound or not yet. This freezes into a `FrozenStarlarkOutputArtifact`.
#[derive(Debug, Clone, Dupe, ProvidesStaticType, Trace, NoSerialize, Allocative)]
// Can't use starlark value macros since we change type during freezing.
pub struct StarlarkOutputArtifact {
    pub(super) artifact: OutputArtifact,
}

impl Display for StarlarkOutputArtifact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<output artifact for {}>", self.artifact.get_path())
    }
}

impl<'v> UnpackValue<'v> for StarlarkOutputArtifact {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        #[allow(clippy::manual_map)]
        if let Some(x) = value.downcast_ref::<StarlarkOutputArtifact>() {
            Some(x.dupe())
        } else if let Some(x) = value.downcast_ref::<StarlarkDeclaredArtifact>() {
            Some(StarlarkOutputArtifact::new(x.artifact.as_output()))
        } else {
            None
        }
    }
}

impl StarlarkOutputArtifact {
    pub fn new(artifact: OutputArtifact) -> Self {
        Self { artifact }
    }

    fn ensure_bound(&self) -> anyhow::Result<BoundBuildArtifact> {
        match (*self.artifact).dupe().ensure_bound() {
            Ok(b) => Ok(b),
            Err(_) => Err(ArtifactError::DeclaredArtifactWasNotBound {
                repr: self.to_string(),
            }
            .into()),
        }
    }

    pub fn artifact(&self) -> OutputArtifact {
        self.artifact.dupe()
    }
}

impl Freeze for StarlarkOutputArtifact {
    type Frozen = FrozenStarlarkOutputArtifact;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let artifact = self
            .ensure_bound()
            .context("artifact should have been bound before freezing")?;

        Ok(FrozenStarlarkOutputArtifact { artifact })
    }
}

impl<'v> AllocValue<'v> for StarlarkOutputArtifact {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl CommandLineArgLike for StarlarkOutputArtifact {
    fn add_to_command_line(
        &self,
        _cli: &mut dyn CommandLineBuilder,
        _ctx: &mut dyn CommandLineBuilderContext,
    ) -> anyhow::Result<()> {
        // TODO: proper error message
        Err(anyhow::anyhow!(
            "proper error here; we should not be adding mutable starlark objects to clis"
        ))
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        visitor.visit_output(self.artifact.dupe(), None);
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

/// A wrapper like `StarlarkOutputArtifact` that is guaranteed to be bound.
#[derive(Debug, PartialEq, ProvidesStaticType, Allocative)]
#[derive(NoSerialize)] // TODO bound artifacts should be serializable
pub struct FrozenStarlarkOutputArtifact {
    artifact: BoundBuildArtifact,
}

impl Display for FrozenStarlarkOutputArtifact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<output artifact for {}>", self.artifact)
    }
}

starlark_simple_value!(FrozenStarlarkOutputArtifact);

impl<'v> StarlarkValue<'v> for FrozenStarlarkOutputArtifact {
    starlark_type!("output_artifact");
}

impl<'v> StarlarkValue<'v> for StarlarkOutputArtifact {
    starlark_type!("output_artifact");
}

impl FrozenStarlarkOutputArtifact {
    pub(crate) fn artifact(&self) -> Artifact {
        self.artifact.dupe().into_artifact()
    }
}

impl CommandLineArgLike for FrozenStarlarkOutputArtifact {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineBuilderContext,
    ) -> anyhow::Result<()> {
        cli.add_arg_string(ctx.resolve_artifact(&self.artifact())?.into_string());
        Ok(())
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        visitor.visit_output(self.artifact.dupe().into_declared_artifact().into(), None);
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
