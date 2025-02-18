/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Provides the starlark values representing resolved attrs.arg() attributes.

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_node::attrs::attr_type::arg::ConfiguredStringWithMacros;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Demand;
use starlark::values::FrozenRef;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use static_assertions::assert_eq_size;

use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkArtifactLike;
use crate::interpreter::rule_defs::cmd_args::arg_builder::ArgBuilder;
use crate::interpreter::rule_defs::cmd_args::command_line_arg_like_type::command_line_arg_like_impl;
use crate::interpreter::rule_defs::cmd_args::space_separated::SpaceSeparatedCommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::value::FrozenCommandLineArg;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use crate::interpreter::rule_defs::provider::builtin::default_info::FrozenDefaultInfo;
use crate::interpreter::rule_defs::resolve_query_macro::ResolvedQueryMacro;

// TODO(cjhopman): Consider making DefaultOutputs implement CommandLineArgLike
// itself, and then a resolved macro is just a CommandLineArgLike.

// TODO(cjhopman): Consider making ResolvedMacro, ResolvedStringWithMacros etc
// parameterized on a Value type so that we can have non-frozen things. At that
// point we could get rid of the Query variant for ResolvedMacro.

#[derive(Debug, PartialEq, Allocative)]
pub enum ResolvedMacro<'v> {
    Location(FrozenRef<'v, FrozenDefaultInfo>),
    Source(Artifact),
    /// Holds an arg-like value
    ArgLike(FrozenCommandLineArg),
    /// Holds a resolved query placeholder
    Query(ResolvedQueryMacro),
}

assert_eq_size!(ResolvedMacro, [usize; 2]);

impl<'v> Display for ResolvedMacro<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolvedMacro::Location(_) => {
                // Unfortunately we don't keep the location here, which makes it harder to show
                write!(f, "$(location ...)")
            }
            ResolvedMacro::Source(a) => write!(f, "$(source {})", a),
            ResolvedMacro::ArgLike(x) => Display::fmt(x, f),
            ResolvedMacro::Query(x) => Display::fmt(x, f),
        }
    }
}

pub fn add_output_to_arg(
    builder: &mut dyn ArgBuilder,
    ctx: &mut dyn CommandLineContext,
    artifact: &StarlarkArtifact,
) -> buck2_error::Result<()> {
    let path = ctx
        .resolve_artifact(&artifact.get_bound_artifact()?)?
        .into_string();
    builder.push_str(&path);
    Ok(())
}

fn add_outputs_to_arg(
    builder: &mut dyn ArgBuilder,
    ctx: &mut dyn CommandLineContext,
    outputs_list: &[StarlarkArtifact],
) -> buck2_error::Result<()> {
    for (i, value) in outputs_list.iter().enumerate() {
        if i != 0 {
            builder.push_str(" ");
        }
        add_output_to_arg(builder, ctx, value)?;
    }
    Ok(())
}

impl<'v> ResolvedMacro<'v> {
    pub fn add_to_arg(
        &self,
        builder: &mut dyn ArgBuilder,
        ctx: &mut dyn CommandLineContext,
    ) -> buck2_error::Result<()> {
        match self {
            Self::Source(artifact) => {
                let s = ctx.resolve_artifact(artifact)?.into_string();
                builder.push_str(&s);
            }
            Self::Location(info) => {
                let outputs = &info.default_outputs();

                add_outputs_to_arg(builder, ctx, outputs)?;
            }
            Self::ArgLike(command_line_like) => {
                let mut cli_builder = SpaceSeparatedCommandLineBuilder::wrap(builder);
                command_line_like
                    .as_command_line_arg()
                    .add_to_command_line(&mut cli_builder, ctx)?;
            }
            Self::Query(value) => value.add_to_arg(builder, ctx)?,
        };

        Ok(())
    }

    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> buck2_error::Result<()> {
        match self {
            Self::Location(info) => {
                info.for_each_output(&mut |i| visitor.visit_input(i, None))?;
            }
            Self::ArgLike(command_line_like) => {
                command_line_like
                    .as_command_line_arg()
                    .visit_artifacts(visitor)?;
            }
            Self::Query(value) => value.visit_artifacts(visitor)?,
            Self::Source(artifact) => {
                visitor.visit_input(ArtifactGroup::Artifact(artifact.dupe()), None)
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Allocative)]
pub enum ResolvedStringWithMacrosPart<'v> {
    String(ArcStr),
    Macro(/* write_to_file */ bool, ResolvedMacro<'v>),
}

impl<'v> Display for ResolvedStringWithMacrosPart<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(x) => f.write_str(x),
            Self::Macro(b, x) => {
                if *b {
                    write!(f, "@")?;
                }
                Display::fmt(x, f)
            }
        }
    }
}

#[derive(Debug, PartialEq, ProvidesStaticType, NoSerialize, Allocative)]
pub struct ResolvedStringWithMacros {
    parts: Vec<ResolvedStringWithMacrosPart<'static>>,
    configured_macros: Option<ConfiguredStringWithMacros>,
}

starlark_simple_value!(ResolvedStringWithMacros);

impl Display for ResolvedStringWithMacros {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"")?;
        for x in &self.parts {
            Display::fmt(x, f)?;
        }
        write!(f, "\"")
    }
}

impl ResolvedStringWithMacros {
    pub fn new(
        parts: Vec<ResolvedStringWithMacrosPart<'static>>,
        configured_macros: Option<&ConfiguredStringWithMacros>,
    ) -> Self {
        Self {
            parts,
            configured_macros: configured_macros.cloned(),
        }
    }

    /// Access the `&str` in this ResolvedStringWithMacros, *if* this ResolvedStringWithMacros is
    /// secretely just one String.
    pub fn downcast_str(&self) -> Option<&str> {
        let mut iter = self.parts.iter();
        match (iter.next(), iter.next()) {
            (Some(ResolvedStringWithMacrosPart::String(s)), None) => Some(s),
            _ => None,
        }
    }

    pub fn configured_macros(&self) -> &Option<ConfiguredStringWithMacros> {
        &self.configured_macros
    }
}

impl CommandLineArgLike for ResolvedStringWithMacros {
    fn register_me(&self) {
        command_line_arg_like_impl!(ResolvedStringWithMacros::starlark_type_repr());
    }

    fn add_to_command_line(
        &self,
        cmdline_builder: &mut dyn CommandLineBuilder,
        ctx: &mut dyn CommandLineContext,
    ) -> buck2_error::Result<()> {
        struct Builder {
            arg: String,
        }

        impl Builder {
            fn push_path(&mut self, ctx: &mut dyn CommandLineContext) -> buck2_error::Result<()> {
                let next_path = ctx.next_macro_file_path()?;
                self.push_str(next_path.as_str());
                Ok(())
            }
        }

        impl ArgBuilder for Builder {
            /// Add the string representation to the list of command line arguments.
            fn push_str(&mut self, s: &str) {
                self.arg.push_str(s)
            }
        }

        let mut builder = Builder { arg: String::new() };

        for part in &*self.parts {
            match part {
                ResolvedStringWithMacrosPart::String(s) => {
                    builder.arg.push_str(s);
                }
                ResolvedStringWithMacrosPart::Macro(write_to_file, val) => {
                    if *write_to_file {
                        builder.push_str("@");
                        builder.push_path(ctx)?;
                    } else {
                        val.add_to_arg(&mut builder, ctx)?;
                    }
                }
            }
        }

        let Builder { arg } = builder;
        cmdline_builder.push_arg(arg);
        Ok(())
    }

    fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> buck2_error::Result<()> {
        for part in &*self.parts {
            if let ResolvedStringWithMacrosPart::Macro(_, val) = part {
                val.visit_artifacts(visitor)?;
            }
        }

        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        true
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> buck2_error::Result<()> {
        for part in &*self.parts {
            match part {
                ResolvedStringWithMacrosPart::String(_) => {
                    // nop
                }
                ResolvedStringWithMacrosPart::Macro(write_to_file, val) => {
                    if *write_to_file {
                        visitor.visit_write_to_file_macro(val)?;
                    } else {
                        // nop
                    }
                }
            }
        }
        Ok(())
    }
}

#[starlark_value(type = "resolved_macro")]
impl<'v> StarlarkValue<'v> for ResolvedStringWithMacros {
    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        match ResolvedStringWithMacros::from_value(other) {
            None => Ok(false),
            Some(other) => Ok(*self == *other),
        }
    }

    fn provide(&'v self, demand: &mut Demand<'_, 'v>) {
        demand.provide_value::<&dyn CommandLineArgLike>(self);
    }
}

#[starlark_module]
pub(crate) fn register_string_with_macros(globals: &mut GlobalsBuilder) {
    const ResolvedStringWithMacros: StarlarkValueAsType<ResolvedStringWithMacros> =
        StarlarkValueAsType::new();
}
