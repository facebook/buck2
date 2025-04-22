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
use starlark::environment::GlobalsBuilder;
use starlark::values::Demand;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::type_repr::StarlarkTypeRepr;

use super::StarlarkTaggedValueGen;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use crate::interpreter::rule_defs::cmd_args::command_line_arg_like_type::command_line_arg_like_impl;
use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;

/// StarlarkTaggedCommandLine wraps a CommandLineArgLike to apply a given ArtifactTag to all its inputs and
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
#[display("StarlarkTaggedCommandLine({})", inner)]
pub struct StarlarkTaggedCommandLineGen<V: ValueLifetimeless> {
    inner: StarlarkTaggedValueGen<V>,
}

impl<V: ValueLifetimeless> StarlarkTaggedCommandLineGen<V> {
    pub fn new(inner: StarlarkTaggedValueGen<V>) -> Self {
        Self { inner }
    }
}

starlark_complex_value!(pub StarlarkTaggedCommandLine);

#[starlark_value(type = "TaggedCommandLine")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkTaggedCommandLineGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for StarlarkTaggedCommandLineGen<V> {
    fn register_me(&self) {
        command_line_arg_like_impl!(StarlarkTaggedCommandLine::starlark_type_repr());
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
            .is_some_and(|inner| inner.0.contains_arg_attr())
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

#[starlark_module]
pub(crate) fn register_tagged_command_line(globals: &mut GlobalsBuilder) {
    const TaggedCommandLine: StarlarkValueAsType<StarlarkTaggedCommandLine> =
        StarlarkValueAsType::new();
}
