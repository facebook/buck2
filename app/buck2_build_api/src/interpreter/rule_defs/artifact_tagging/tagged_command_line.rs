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
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;

use super::TaggedValueGen;
use crate::interpreter::rule_defs::cmd_args::command_line_arg_like_type::command_line_arg_like_impl;
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
#[display("TaggedCommandLine({})", inner)]
pub struct TaggedCommandLineGen<V: ValueLifetimeless> {
    inner: TaggedValueGen<V>,
}

impl<V: ValueLifetimeless> TaggedCommandLineGen<V> {
    pub fn new(inner: TaggedValueGen<V>) -> Self {
        Self { inner }
    }
}

starlark_complex_value!(pub TaggedCommandLine);

#[starlark_value(type = "tagged_command_line")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for TaggedCommandLineGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for TaggedCommandLineGen<V> {
    fn register_me(&self) {
        command_line_arg_like_impl!(TaggedCommandLine::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
    ) -> buck2_error::Result<()> {
        ValueAsCommandLineLike::unpack_value_err(self.inner.value().to_value())?
            .0
            .add_to_command_line(cli, context)
    }

    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> buck2_error::Result<()> {
        let mut visitor = self.inner.wrap_visitor(visitor);

        ValueAsCommandLineLike::unpack_value_err(self.inner.value().to_value())?
            .0
            .visit_artifacts(&mut visitor)
    }

    fn contains_arg_attr(&self) -> bool {
        ValueAsCommandLineLike::unpack(self.inner.value().to_value())
            .map_or(false, |inner| inner.0.contains_arg_attr())
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> buck2_error::Result<()> {
        ValueAsCommandLineLike::unpack_value_err(self.inner.value().to_value())?
            .0
            .visit_write_to_file_macros(visitor)
    }
}
