/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::values::Coerce;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::FrozenValueTyped;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::type_repr::StarlarkTypeRepr;

use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::cmd_args::ArtifactPathMapper;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use crate::interpreter::rule_defs::cmd_args::command_line_arg_like_type::command_line_arg_like_impl;

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
pub struct StarlarkOutputArtifactGen<V: ValueLifetimeless> {
    declared_artifact: V, // StarlarkDeclaredArtifact or FrozenStarlarkArtifact
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

impl<'v> StarlarkOutputArtifact<'v> {
    pub fn new(v: ValueTyped<'v, StarlarkDeclaredArtifact<'v>>) -> Self {
        Self {
            declared_artifact: v.to_value(),
        }
    }

    pub(crate) fn inner(&self) -> ValueTyped<'v, StarlarkDeclaredArtifact<'v>> {
        ValueTyped::new_err(self.declared_artifact).expect("Type checked at construction time")
    }

    pub fn artifact(&self) -> OutputArtifact<'v> {
        self.inner().output_artifact()
    }
}

impl FrozenStarlarkOutputArtifact {
    pub fn inner(&self) -> FrozenValueTyped<StarlarkArtifact>
    // Ensures that this stops compiling if this changes
    where
        for<'v> StarlarkDeclaredArtifact<'v>: Freeze<Frozen = StarlarkArtifact>,
    {
        // Unwrap justified by construction of the type and the where clause
        FrozenValueTyped::new_err(self.declared_artifact).unwrap()
    }
}

impl<'v> CommandLineArgLike<'v> for StarlarkOutputArtifact<'v> {
    fn register_me(&self) {
        command_line_arg_like_impl!(StarlarkOutputArtifact::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        _cli: &mut dyn CommandLineBuilder,
        _ctx: &mut dyn CommandLineContext,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        // TODO: proper error message
        Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "proper error here; we should not be adding mutable starlark objects to clis"
        ))
    }

    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor<'v>,
    ) -> buck2_error::Result<()> {
        visitor.visit_declared_output(self.artifact(), None);
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}

#[starlark_value(type = "OutputArtifact")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkOutputArtifactGen<V>
where
    Self: ProvidesStaticType<'v> + Display + CommandLineArgLike<'v>,
{
    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }
}

impl<'v> CommandLineArgLike<'v> for FrozenStarlarkOutputArtifact {
    fn register_me(&self) {
        command_line_arg_like_impl!(FrozenStarlarkOutputArtifact::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineContext,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        // We do not need to use the ArtifactPathMapper here as output artifacts are always
        // resolved to a known path since their content hash is not yet available.
        cli.push_location(ctx.resolve_output_artifact(&self.inner().artifact())?);
        Ok(())
    }

    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> buck2_error::Result<()> {
        visitor.visit_frozen_output(self.inner().artifact(), None);
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
        _artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
}

#[starlark_module]
pub(crate) fn register_output_artifact(globals: &mut GlobalsBuilder) {
    const OutputArtifact: StarlarkValueAsType<StarlarkOutputArtifact> = StarlarkValueAsType::new();
}
