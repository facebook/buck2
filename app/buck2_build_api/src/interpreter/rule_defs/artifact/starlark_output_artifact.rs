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
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::starlark_type;
use starlark::values::Coerce;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::FrozenValueTyped;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;

use crate::actions::artifact::artifact_type::OutputArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

/// Thin wrapper around `OutputArtifact`.
///
/// Allows actions to distinguish between inputs and outputs, and can validate whether the
/// underlying artifact is bound or not yet.
#[derive(
    Debug,
    Clone,
    Dupe,
    ProvidesStaticType,
    Trace,
    NoSerialize,
    Allocative,
    Freeze,
    Coerce
)]
#[repr(C)]
pub struct StarlarkOutputArtifactGen<V> {
    pub(super) declared_artifact: V,
}

starlark_complex_value!(pub StarlarkOutputArtifact);

impl<'v> Display for StarlarkOutputArtifact<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<output artifact for {}>",
            self.inner().get_artifact_path()
        )
    }
}

impl Display for FrozenStarlarkOutputArtifact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<output artifact for {}>",
            self.inner().get_artifact_path()
        )
    }
}

impl<'v> UnpackValue<'v> for StarlarkOutputArtifact<'v> {
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        #[allow(clippy::manual_map)]
        if let Some(x) = value.downcast_ref::<StarlarkOutputArtifact>() {
            Some(x.dupe())
        } else if let Some(x) = ValueTyped::<StarlarkDeclaredArtifact>::new(value) {
            Some(StarlarkOutputArtifact::new(x))
        } else {
            None
        }
    }
}

impl<'v> StarlarkOutputArtifact<'v> {
    pub fn new(v: ValueTyped<'v, StarlarkDeclaredArtifact>) -> Self {
        Self {
            declared_artifact: v.to_value(),
        }
    }

    pub(crate) fn inner(&self) -> ValueTyped<'v, StarlarkDeclaredArtifact> {
        ValueTyped::new(self.declared_artifact).unwrap()
    }

    pub fn artifact(&self) -> OutputArtifact {
        self.inner().output_artifact()
    }
}

impl FrozenStarlarkOutputArtifact {
    pub(crate) fn inner(&self) -> FrozenValueTyped<StarlarkArtifact> {
        FrozenValueTyped::new(self.declared_artifact).unwrap()
    }

    pub fn artifact(&self) -> OutputArtifact {
        self.inner().artifact().as_output_artifact().unwrap()
    }
}

impl<'v> CommandLineArgLike for StarlarkOutputArtifact<'v> {
    fn add_to_command_line(
        &self,
        _cli: &mut dyn CommandLineBuilder,
        _ctx: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        // TODO: proper error message
        Err(anyhow::anyhow!(
            "proper error here; we should not be adding mutable starlark objects to clis"
        ))
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        visitor.visit_output(self.artifact(), None);
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

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkOutputArtifactGen<V>
where
    Self: ProvidesStaticType + Display + CommandLineArgLike,
{
    starlark_type!("output_artifact");

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }
}

impl CommandLineArgLike for FrozenStarlarkOutputArtifact {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        cli.push_arg(
            ctx.resolve_artifact(&self.inner().artifact())?
                .into_string(),
        );
        Ok(())
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        visitor.visit_output(self.artifact(), None);
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
