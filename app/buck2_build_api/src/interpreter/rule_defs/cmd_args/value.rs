/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

use allocative::Allocative;
use dupe::Dupe;
use serde::Serializer;
use starlark::__derive_refs::serde::Serialize;
use starlark::coerce::Coerce;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

fn serialize_as_display<V: Display, S>(v: &V, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    s.collect_str(v)
}

/// A tiny wrapper around `Value`/`FrozenValue` that proxies `CommandLineArgLike` calls.
///
/// This should be unnecessary, however I'm not smart enough to figure out how to get
/// things to live long enough, in `ValueAsCommandLineArgLike`, so I'm moving on with my life
/// for now. All values contained in here are guaranteed to implement `CommandLineArgLike`.
#[derive(
    Debug,
    Clone,
    Copy,
    Dupe,
    Trace,
    derive_more::Display,
    Serialize,
    Allocative,
    Coerce
)]
#[serde(transparent)]
#[repr(transparent)]
pub(crate) struct CommandLineArg<'v>(#[serde(serialize_with = "serialize_as_display")] Value<'v>);

impl<'v> Freeze for CommandLineArg<'v> {
    type Frozen = FrozenCommandLineArg;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<FrozenCommandLineArg> {
        Ok(FrozenCommandLineArg(self.0.freeze(freezer)?))
    }
}

impl<'v> CommandLineArg<'v> {
    pub(crate) fn try_from_value(value: Value<'v>) -> anyhow::Result<Self> {
        value.to_value().as_command_line_err()?;
        Ok(Self(value))
    }

    fn visit_inner<R>(self, f: impl FnOnce(&dyn CommandLineArgLike) -> R) -> R {
        f(self.0.to_value().as_command_line().unwrap())
    }
}

impl<'v> CommandLineArgLike for CommandLineArg<'v> {
    fn add_to_command_line(
        &self,
        cli: &mut dyn CommandLineBuilder,
        context: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        self.visit_inner(|x| x.add_to_command_line(cli, context))
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        self.visit_inner(|x| x.visit_artifacts(visitor))
    }

    fn contains_arg_attr(&self) -> bool {
        self.visit_inner(|x| x.contains_arg_attr())
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        self.visit_inner(|x| x.visit_write_to_file_macros(visitor))
    }
}

#[derive(Debug, Allocative)]
#[repr(transparent)]
pub(crate) struct FrozenCommandLineArg(FrozenValue);

unsafe impl<'v> Coerce<CommandLineArg<'v>> for FrozenCommandLineArg {}
