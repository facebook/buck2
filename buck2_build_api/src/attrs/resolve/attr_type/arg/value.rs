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
use anyhow::Context;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_node::attrs::attr_type::arg::ConfiguredMacro;
use buck2_node::attrs::attr_type::arg::ConfiguredStringWithMacros;
use buck2_node::attrs::attr_type::arg::ConfiguredStringWithMacrosPart;
use either::Either;
use gazebo::any::ProvidesStaticType;
use starlark::starlark_type;
use starlark::values::FrozenRef;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;

use crate::attrs::resolve::attr_type::arg::query::ConfiguredQueryMacroBaseExt;
use crate::attrs::resolve::attr_type::arg::query::ResolvedQueryMacro;
use crate::attrs::resolve::attr_type::arg::ArgBuilder;
use crate::attrs::resolve::attr_type::arg::SpaceSeparatedCommandLineBuilder;
use crate::attrs::resolve::ctx::AttrResolutionContext;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilderContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;
use crate::interpreter::rule_defs::cmd_args::FrozenCommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use crate::interpreter::rule_defs::provider::builtin::default_info::FrozenDefaultInfo;
use crate::interpreter::rule_defs::provider::builtin::run_info::RunInfoCallable;
use crate::interpreter::rule_defs::provider::builtin::template_placeholder_info::FrozenTemplatePlaceholderInfo;

// TODO(cjhopman): Consider making DefaultOutputs implement CommandLineArgLike
// itself, and then a resolved macro is just a CommandLineArgLike.

// TODO(cjhopman): Consider making ResolvedMacro, ResolvedStringWithMacros etc
// parameterized on a Value type so that we can have non-frozen things. At that
// point we could get rid of the Query variant for ResolvedMacro.

#[derive(Debug, PartialEq, Allocative)]
pub enum ResolvedMacro {
    Location(FrozenRef<'static, FrozenDefaultInfo>),
    /// Holds an arg-like value
    ArgLike(FrozenRef<'static, dyn FrozenCommandLineArgLike>),
    /// Holds a resolved query placeholder
    Query(ResolvedQueryMacro),
}

impl Display for ResolvedMacro {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolvedMacro::Location(_) => {
                // Unfortunately we don't keep the location here, which makes it harder to show
                write!(f, "$(location ...)")
            }
            ResolvedMacro::ArgLike(x) => Display::fmt(x, f),
            ResolvedMacro::Query(x) => Display::fmt(x, f),
        }
    }
}

pub fn add_output_to_arg(
    builder: &mut dyn ArgBuilder,
    artifact: &StarlarkArtifact,
) -> anyhow::Result<()> {
    let path = builder
        .resolve_artifact(&artifact.get_bound_artifact()?)?
        .into_string();
    builder.push_str(&path);
    Ok(())
}

fn add_outputs_to_arg(
    builder: &mut dyn ArgBuilder,
    outputs_list: &[FrozenRef<'static, StarlarkArtifact>],
) -> anyhow::Result<()> {
    for (i, value) in outputs_list.iter().enumerate() {
        if i != 0 {
            builder.push_str(" ");
        }
        add_output_to_arg(builder, value)?;
    }
    Ok(())
}

#[derive(Debug, thiserror::Error)]
enum ResolvedMacroError {
    #[error("Can't expand unrecognized macros (`{0}`).")]
    UnrecognizedMacroUnimplemented(String),
    #[error("Expected a RunInfo provider from target `{0}`.")]
    ExpectedRunInfo(String),

    #[error("There was no TemplatePlaceholderInfo for {0}.")]
    KeyedPlaceholderInfoMissing(ConfiguredProvidersLabel),
    #[error("There was no mapping for {0} in the TemplatePlaceholderInfo for {1}.")]
    KeyedPlaceholderMappingMissing(String, ConfiguredProvidersLabel),
    #[error(
        "The mapping for {0} in the TemplatePlaceholderInfo for {1} was not a dictionary (required because requested arg `{2}`)."
    )]
    KeyedPlaceholderMappingNotADict(String, ConfiguredProvidersLabel, String),
    #[error(
        "The mapping for {0} in the TemplatePlaceholderInfo for {1} had no mapping for arg `{2}`."
    )]
    KeyedPlaceholderArgMissing(String, ConfiguredProvidersLabel, String),
    #[error("There was no mapping for {0}.")]
    UnkeyedPlaceholderUnresolved(String),
}

impl ResolvedMacro {
    fn resolved(
        configured_macro: &ConfiguredMacro,
        ctx: &dyn AttrResolutionContext,
    ) -> anyhow::Result<ResolvedMacro> {
        match configured_macro {
            ConfiguredMacro::Location(target) => {
                let providers_value = ctx.get_dep(target)?;
                let providers = providers_value.provider_collection();
                Ok(ResolvedMacro::Location(providers.default_info()))
            }
            ConfiguredMacro::Exe { label, .. } => {
                // Don't need to consider exec_dep as it already was applied when configuring the label.
                let providers_value = ctx.get_dep(label)?;
                let providers = providers_value.provider_collection();
                let run_info = match providers.get_provider(RunInfoCallable::provider_id_t()) {
                    Some(value) => value,
                    None => {
                        return Err(ResolvedMacroError::ExpectedRunInfo(label.to_string()).into());
                    }
                };
                // A RunInfo is an arg-like value.
                Ok(ResolvedMacro::ArgLike(
                    run_info.map(|x| x as &dyn FrozenCommandLineArgLike),
                ))
            }
            ConfiguredMacro::UserUnkeyedPlaceholder(name) => {
                let provider = ctx.resolve_unkeyed_placeholder(name).ok_or_else(|| {
                    ResolvedMacroError::UnkeyedPlaceholderUnresolved(name.to_owned())
                })?;
                Ok(ResolvedMacro::ArgLike(provider))
            }
            ConfiguredMacro::UserKeyedPlaceholder(name, label, arg) => {
                let providers = ctx.get_dep(label)?;
                let placeholder_info =
                    FrozenTemplatePlaceholderInfo::from_providers(providers.provider_collection())
                        .ok_or_else(|| {
                            ResolvedMacroError::KeyedPlaceholderInfoMissing(label.clone())
                        })?;
                let keyed_variables = placeholder_info.keyed_variables();
                let either_cmd_or_mapping =
                    keyed_variables.get(name.as_str()).ok_or_else(|| {
                        ResolvedMacroError::KeyedPlaceholderMappingMissing(
                            name.clone(),
                            label.clone(),
                        )
                    })?;

                let value = match (arg, either_cmd_or_mapping) {
                    (None, Either::Left(mapping)) => mapping,
                    (Some(arg), Either::Left(_)) => {
                        return Err(ResolvedMacroError::KeyedPlaceholderMappingNotADict(
                            name.clone(),
                            label.clone(),
                            arg.clone(),
                        )
                        .into());
                    }
                    (arg, Either::Right(mapping)) => {
                        let arg = arg.as_deref().unwrap_or("DEFAULT");
                        mapping.get(arg).ok_or_else(|| {
                            ResolvedMacroError::KeyedPlaceholderArgMissing(
                                name.clone(),
                                label.clone(),
                                arg.to_owned(),
                            )
                        })?
                    }
                };

                Ok(ResolvedMacro::ArgLike(*value))
            }
            ConfiguredMacro::Query(query) => Ok(ResolvedMacro::Query(query.resolve(ctx)?)),
            ConfiguredMacro::UnrecognizedMacro(name, _args) => Err(anyhow::anyhow!(
                ResolvedMacroError::UnrecognizedMacroUnimplemented(name.clone())
            )),
        }
    }

    pub(crate) fn add_to_arg(&self, builder: &mut dyn ArgBuilder) -> anyhow::Result<()> {
        match self {
            Self::Location(info) => {
                let outputs = &info.default_outputs();

                add_outputs_to_arg(builder, outputs)?;
            }
            Self::ArgLike(command_line_like) => {
                let mut cli_builder = SpaceSeparatedCommandLineBuilder::wrap(builder);
                command_line_like.add_to_command_line(&mut cli_builder)?;
            }
            Self::Query(value) => value.add_to_arg(builder)?,
        };

        Ok(())
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        match self {
            Self::Location(info) => {
                info.for_each_output(&mut |i| {
                    visitor.visit_input(i, None);
                    Ok(())
                })?;
            }
            Self::ArgLike(command_line_like) => {
                command_line_like.visit_artifacts(visitor)?;
            }
            Self::Query(value) => value.visit_artifacts(visitor)?,
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Allocative)]
pub(crate) enum ResolvedStringWithMacrosPart {
    String(String),
    Macro(/* write_to_file */ bool, ResolvedMacro),
}

impl Display for ResolvedStringWithMacrosPart {
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
    parts: Vec<ResolvedStringWithMacrosPart>,
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
    pub(crate) fn new(parts: Vec<ResolvedStringWithMacrosPart>) -> Self {
        Self { parts }
    }

    pub(crate) fn resolved<'v>(
        configured_macros: &ConfiguredStringWithMacros,
        ctx: &dyn AttrResolutionContext<'v>,
    ) -> anyhow::Result<Value<'v>> {
        let resolved_parts = match configured_macros {
            ConfiguredStringWithMacros::StringPart(s) => {
                vec![ResolvedStringWithMacrosPart::String(
                    s.clone().into_string(),
                )]
            }
            ConfiguredStringWithMacros::ManyParts(ref parts) => {
                let mut resolved_parts = Vec::with_capacity(parts.len());
                for part in parts.iter() {
                    match part {
                        ConfiguredStringWithMacrosPart::String(s) => {
                            resolved_parts.push(ResolvedStringWithMacrosPart::String(s.clone()));
                        }
                        ConfiguredStringWithMacrosPart::Macro(write_to_file, m) => {
                            resolved_parts.push(ResolvedStringWithMacrosPart::Macro(
                                *write_to_file,
                                ResolvedMacro::resolved(m, ctx)
                                    .with_context(|| format!("When resolving `{}`.", part))?,
                            ));
                        }
                    }
                }
                resolved_parts
            }
        };

        Ok(ctx
            .heap()
            .alloc(ResolvedStringWithMacros::new(resolved_parts)))
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
}

impl CommandLineArgLike for ResolvedStringWithMacros {
    fn add_to_command_line(
        &self,
        cmdline_builder: &mut dyn CommandLineBuilder,
    ) -> anyhow::Result<()> {
        struct Builder<'a> {
            cmdline_builder: &'a mut dyn CommandLineBuilder,
            arg: String,
        }

        impl Builder<'_> {
            fn push_path(&mut self) -> anyhow::Result<()> {
                let next_path = self.cmdline_builder.next_macro_file_path()?;
                self.push_str(next_path.as_str());
                Ok(())
            }
        }

        impl CommandLineBuilderContext for Builder<'_> {
            fn resolve_project_path(
                &self,
                path: ProjectRelativePathBuf,
            ) -> anyhow::Result<CommandLineLocation> {
                self.cmdline_builder.resolve_project_path(path)
            }

            fn fs(&self) -> &ExecutorFs {
                self.cmdline_builder.fs()
            }

            fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf> {
                self.cmdline_builder.next_macro_file_path()
            }
        }

        impl ArgBuilder for Builder<'_> {
            /// Add the string representation to the list of command line arguments.
            fn push_str(&mut self, s: &str) {
                self.arg.push_str(s)
            }
        }

        let mut builder = Builder {
            cmdline_builder,
            arg: String::new(),
        };

        for part in &*self.parts {
            match part {
                ResolvedStringWithMacrosPart::String(s) => {
                    builder.arg.push_str(s);
                }
                ResolvedStringWithMacrosPart::Macro(write_to_file, val) => {
                    if *write_to_file {
                        builder.push_str("@");
                        builder.push_path()?;
                    } else {
                        val.add_to_arg(&mut builder)?;
                    }
                }
            }
        }

        let Builder {
            cmdline_builder,
            arg,
        } = builder;
        cmdline_builder.add_arg_string(arg);
        Ok(())
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
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
    ) -> anyhow::Result<()> {
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

impl<'v> StarlarkValue<'v> for ResolvedStringWithMacros {
    starlark_type!("resolved_macro");

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match ResolvedStringWithMacros::from_value(other) {
            None => Ok(false),
            Some(other) => Ok(*self == *other),
        }
    }
}
