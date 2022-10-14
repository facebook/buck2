/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use starlark::starlark_type;
use starlark::values::Freeze;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use super::ArtifactTag;
use crate::actions::artifact::OutputArtifact;
use crate::artifact_groups::ArtifactGroup;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;

/// TaggedArtifacts wraps a CommandLineArgLike to apply a given ArtifactTag to all its inputs and
/// outputs.
#[derive(Debug, Clone, Trace, Coerce, Freeze, Display, ProvidesStaticType)]
#[derive(NoSerialize)] // TODO make artifacts serializable
#[repr(C)]
#[display(fmt = "TaggedArtifacts({}, tagged {})", inner, tag)]
pub struct TaggedArtifactsGen<V> {
    inner: V,
    tag: ArtifactTag,
    inputs_only: bool,
}

impl<'v> TaggedArtifacts<'v> {
    pub fn new(inner: Value<'v>, tag: ArtifactTag) -> Self {
        Self {
            inner,
            tag,
            inputs_only: false,
        }
    }

    pub fn inputs_only(inner: Value<'v>, tag: ArtifactTag) -> Self {
        Self {
            inner,
            tag,
            inputs_only: true,
        }
    }
}

starlark_complex_value!(pub TaggedArtifacts);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TaggedArtifactsGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("tagged_artifacts");
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for TaggedArtifactsGen<V> {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        self.inner
            .to_value()
            .as_command_line_err()?
            .add_to_command_line(cli)
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        let mut visitor = TaggedArtifactsVisitor {
            inner: visitor,
            tag: &self.tag,
            inputs_only: self.inputs_only,
        };

        self.inner
            .to_value()
            .as_command_line_err()?
            .visit_artifacts(&mut visitor)
    }

    fn contains_arg_attr(&self) -> bool {
        self.inner
            .to_value()
            .as_command_line()
            .map_or(false, |inner| inner.contains_arg_attr())
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        self.inner
            .to_value()
            .as_command_line_err()?
            .visit_write_to_file_macros(visitor)
    }
}

/// Wrap an existing CommandLineArtifactVisitor into one that adds an ArtifactTag.
struct TaggedArtifactsVisitor<'a, 'b> {
    inner: &'b mut dyn CommandLineArtifactVisitor,
    tag: &'a ArtifactTag,
    inputs_only: bool,
}

impl<'a, 'b> CommandLineArtifactVisitor for TaggedArtifactsVisitor<'a, 'b> {
    /// Ignore the inner tag, set our own. Nesting input groups generally isn't a great idea, but
    /// we can't statically prevent it.
    fn visit_input(&mut self, input: ArtifactGroup, _tag: Option<&ArtifactTag>) {
        self.inner.visit_input(input, Some(self.tag))
    }

    /// Same as above, no nesting here.
    fn visit_output(&mut self, artifact: OutputArtifact, _tag: Option<&ArtifactTag>) {
        let tag = if self.inputs_only {
            None
        } else {
            Some(self.tag)
        };
        self.inner.visit_output(artifact, tag)
    }
}
