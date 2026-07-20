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
use std::fmt::Display;

use allocative::Allocative;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_util::size_assert;
use buck2_util::thin_box::ThinBoxSlice;
use pagable::PagableDeserialize;
use pagable::PagableSerialize;
use starlark::pagable::StarlarkDeserialize;
use starlark::pagable::StarlarkDeserializeContext;
use starlark::pagable::StarlarkSerialize;
use starlark::pagable::StarlarkSerializeContext;
use starlark::values::StarlarkPagable;

use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkInputArtifactLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::resolved_macro::add_output_to_arg;

#[derive(Debug, PartialEq, Allocative, StarlarkPagable)]
pub struct ResolvedQueryMacroTargetAndOutputs {
    #[starlark_pagable(pagable)]
    pub sep: Box<str>,
    // Mixed: `ConfiguredTargetLabel` is pagable-only (`buck2_core` cannot
    // depend on `starlark`), outputs are starlark-aware.
    #[starlark_pagable(
        serialize_with = "serialize_target_outputs",
        deserialize_with = "deserialize_target_outputs"
    )]
    pub list: Box<[(ConfiguredTargetLabel, Box<[StarlarkArtifact]>)]>,
}

fn serialize_target_outputs(
    field: &[(ConfiguredTargetLabel, Box<[StarlarkArtifact]>)],
    ctx: &mut dyn StarlarkSerializeContext,
) -> starlark::Result<()> {
    PagableSerialize::pagable_serialize(&field.len(), ctx.pagable())?;
    for (target, outputs) in field.iter() {
        PagableSerialize::pagable_serialize(target, ctx.pagable())?;
        StarlarkSerialize::starlark_serialize(outputs, ctx)?;
    }
    Ok(())
}

fn deserialize_target_outputs(
    ctx: &mut dyn StarlarkDeserializeContext<'_>,
) -> starlark::Result<Box<[(ConfiguredTargetLabel, Box<[StarlarkArtifact]>)]>> {
    let len = usize::pagable_deserialize(ctx.pagable())?;
    let mut v = Vec::with_capacity(len);
    for _ in 0..len {
        let target =
            <ConfiguredTargetLabel as PagableDeserialize>::pagable_deserialize(ctx.pagable())?;
        let outputs = <Box<[StarlarkArtifact]> as StarlarkDeserialize>::starlark_deserialize(ctx)?;
        v.push((target, outputs));
    }
    Ok(v.into_boxed_slice())
}

#[derive(Debug, PartialEq, Allocative, StarlarkPagable)]
pub enum ResolvedQueryMacro {
    // `ThinBoxSlice` lives in `buck2_util` (cannot depend on `starlark`),
    // so the per-element starlark bridging lives here at the use site.
    Outputs(
        #[starlark_pagable(
            serialize_with = "serialize_thinbox_starlark",
            deserialize_with = "deserialize_thinbox_starlark"
        )]
        ThinBoxSlice<Box<[StarlarkArtifact]>>,
    ),
    Targets(#[starlark_pagable(pagable)] ThinBoxSlice<ConfiguredTargetLabel>),
    TargetsAndOutputs(Box<ResolvedQueryMacroTargetAndOutputs>),
}

fn serialize_thinbox_starlark<T: StarlarkSerialize + 'static>(
    field: &ThinBoxSlice<T>,
    ctx: &mut dyn StarlarkSerializeContext,
) -> starlark::Result<()> {
    PagableSerialize::pagable_serialize(&field.len(), ctx.pagable())?;
    for item in field.iter() {
        StarlarkSerialize::starlark_serialize(item, ctx)?;
    }
    Ok(())
}

fn deserialize_thinbox_starlark<T: StarlarkDeserialize + 'static>(
    ctx: &mut dyn StarlarkDeserializeContext<'_>,
) -> starlark::Result<ThinBoxSlice<T>> {
    let len = usize::pagable_deserialize(ctx.pagable())?;
    let mut items = Vec::with_capacity(len);
    for _ in 0..len {
        items.push(T::starlark_deserialize(ctx)?);
    }
    Ok(ThinBoxSlice::from_iter(items))
}

size_assert::words_of_type!(ResolvedQueryMacro, 2);

impl Display for ResolvedQueryMacro {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO(ndmitchell): Include the information in the format output
        match self {
            ResolvedQueryMacro::Outputs(_) => {
                write!(f, "$(query_outputs ...)")
            }
            ResolvedQueryMacro::Targets(_) => write!(f, "$(query_targets ...)"),

            ResolvedQueryMacro::TargetsAndOutputs(_) => {
                write!(f, "$(query_targets_and_outputs ...)")
            }
        }
    }
}

impl ResolvedQueryMacro {
    pub fn add_to_arg(&self, fmt: &mut CommandLineBuilder) -> buck2_error::Result<()> {
        match self {
            Self::Outputs(list) => {
                let mut first = true;
                for target_outputs in list.iter() {
                    for output in target_outputs.iter() {
                        if !first {
                            fmt.push_str(" ");
                        }
                        first = false;
                        add_output_to_arg(fmt, output)?;
                    }
                }
            }
            Self::TargetsAndOutputs(target_and_outputs) => {
                let ResolvedQueryMacroTargetAndOutputs { sep, list } = &**target_and_outputs;
                let mut first = true;
                for (target, target_outputs) in list.iter() {
                    for output in target_outputs.iter() {
                        if !first {
                            fmt.push_str(sep);
                        }
                        first = false;
                        fmt.push_string(target.unconfigured().to_string());
                        fmt.push_str(sep);
                        add_output_to_arg(fmt, output)?;
                    }
                }
            }
            Self::Targets(list) => {
                // This is defined to add the plain (unconfigured) labels.
                for (i, target) in list.iter().enumerate() {
                    if i != 0 {
                        fmt.push_str(" ");
                    }
                    fmt.push_string(target.unconfigured().to_string());
                }
            }
        }
        Ok(())
    }

    pub fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor<'_>,
    ) -> buck2_error::Result<()> {
        match self {
            Self::Outputs(list) => {
                for target_outputs in list.iter() {
                    for artifact in target_outputs.iter() {
                        artifact.as_command_line_like().visit_artifacts(visitor)?;
                    }
                }
            }
            Self::TargetsAndOutputs(targets_and_outputs) => {
                for (_, target_outputs) in &*targets_and_outputs.list {
                    for artifact in target_outputs.iter() {
                        artifact.as_command_line_like().visit_artifacts(visitor)?;
                    }
                }
            }
            Self::Targets(..) => {
                // no inputs
            }
        }
        Ok(())
    }
}
