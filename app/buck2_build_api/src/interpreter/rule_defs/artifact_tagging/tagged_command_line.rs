/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::starlark_type;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::ValueLike;

use super::TaggedValueGen;
use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

/// TaggedCommandLine wraps a CommandLineArgLike to apply a given ArtifactTag to all its inputs and
/// outputs.
#[derive(
    Debug,
    Clone,
    Trace,
    Coerce,
    Freeze,
    Display,
    ProvidesStaticType,
    Allocative
)]
#[derive(NoSerialize)] // TODO make artifacts serializable
#[repr(C)]
#[display(fmt = "TaggedCommandLine({})", inner)]
pub struct TaggedCommandLineGen<V> {
    inner: TaggedValueGen<V>,
}

impl<V> TaggedCommandLineGen<V> {
    pub fn new(inner: TaggedValueGen<V>) -> Self {
        Self { inner }
    }
}

starlark_complex_value!(pub TaggedCommandLine);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TaggedCommandLineGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("tagged_command_line");

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for TaggedCommandLineGen<V> {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        self.inner
            .value()
            .to_value()
            .as_command_line_err()?
            .add_to_command_line(cli, context)
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        let mut visitor = self.inner.wrap_visitor(visitor);

        self.inner
            .value()
            .to_value()
            .as_command_line_err()?
            .visit_artifacts(&mut visitor)
    }

    fn contains_arg_attr(&self) -> bool {
        self.inner
            .value()
            .to_value()
            .as_command_line()
            .map_or(false, |inner| inner.contains_arg_attr())
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        self.inner
            .value()
            .to_value()
            .as_command_line_err()?
            .visit_write_to_file_macros(visitor)
    }
}
