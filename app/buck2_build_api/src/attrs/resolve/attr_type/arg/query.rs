/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_node::attrs::attr_type::arg::QueryExpansion;
use buck2_node::attrs::attr_type::query::QueryMacroBase;
use buck2_util::thin_box::ThinBoxSlice;
use dupe::Dupe;
use starlark::values::FrozenRef;
use static_assertions::assert_eq_size;

use crate::attrs::resolve::attr_type::arg::value::add_output_to_arg;
use crate::attrs::resolve::attr_type::arg::ArgBuilder;
use crate::attrs::resolve::attr_type::query::ConfiguredQueryAttrBaseExt;
use crate::attrs::resolve::ctx::AnalysisQueryResult;
use crate::attrs::resolve::ctx::AttrResolutionContext;
use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineContext;

#[derive(Debug, PartialEq, Allocative)]
pub struct ResolvedQueryMacroTargetAndOutputs {
    sep: Box<str>,
    list: Box<
        [(
            ConfiguredTargetLabel,
            Box<[FrozenRef<'static, StarlarkArtifact>]>,
        )],
    >,
}

#[derive(Debug, PartialEq, Allocative)]
pub enum ResolvedQueryMacro {
    Outputs(ThinBoxSlice<Box<[FrozenRef<'static, StarlarkArtifact>]>>),
    Targets(ThinBoxSlice<ConfiguredTargetLabel>),
    TargetsAndOutputs(Box<ResolvedQueryMacroTargetAndOutputs>),
}

assert_eq_size!(ResolvedQueryMacro, [usize; 2]);

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
    pub fn add_to_arg(
        &self,
        builder: &mut dyn ArgBuilder,
        ctx: &mut dyn CommandLineContext,
    ) -> anyhow::Result<()> {
        match self {
            Self::Outputs(list) => {
                let mut first = true;
                for target_outputs in list.iter() {
                    for output in target_outputs.iter() {
                        if !first {
                            builder.push_str(" ");
                        }
                        first = false;
                        add_output_to_arg(builder, ctx, output)?;
                    }
                }
            }
            Self::TargetsAndOutputs(target_and_outputs) => {
                let ResolvedQueryMacroTargetAndOutputs { sep, list } = &**target_and_outputs;
                let mut first = true;
                for (target, target_outputs) in list.iter() {
                    for output in target_outputs.iter() {
                        if !first {
                            builder.push_str(sep);
                        }
                        first = false;
                        builder.push_str(&target.unconfigured().to_string());
                        builder.push_str(sep);
                        add_output_to_arg(builder, ctx, output)?;
                    }
                }
            }
            Self::Targets(list) => {
                // This is defined to add the plain (unconfigured) labels.
                for (i, target) in list.iter().enumerate() {
                    if i != 0 {
                        builder.push_str(" ");
                    }
                    builder.push_str(&target.unconfigured().to_string());
                }
            }
        }
        Ok(())
    }

    pub fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> anyhow::Result<()> {
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

pub(crate) trait ConfiguredQueryMacroBaseExt {
    fn resolve(&self, ctx: &dyn AttrResolutionContext) -> anyhow::Result<ResolvedQueryMacro>;
}

impl ConfiguredQueryMacroBaseExt for QueryMacroBase<ConfiguredProvidersLabel> {
    fn resolve(&self, ctx: &dyn AttrResolutionContext) -> anyhow::Result<ResolvedQueryMacro> {
        let query_result: Arc<AnalysisQueryResult> = self.query.resolve(ctx)?;

        match &self.expansion_type {
            QueryExpansion::Output => Ok(ResolvedQueryMacro::Outputs(
                query_result
                    .iter()
                    .map(|(_, providers)| {
                        providers
                            .provider_collection()
                            .default_info()
                            .default_outputs()
                            .into_boxed_slice()
                    })
                    .collect(),
            )),
            QueryExpansion::Target => Ok(ResolvedQueryMacro::Targets(
                query_result
                    .iter()
                    .map(|(target, _)| target.dupe())
                    .collect(),
            )),
            QueryExpansion::TargetAndOutput(separator) => {
                Ok(ResolvedQueryMacro::TargetsAndOutputs(Box::new(
                    ResolvedQueryMacroTargetAndOutputs {
                        sep: match separator {
                            Some(separator) => separator.to_owned().into_boxed_str(),
                            None => " ".to_owned().into_boxed_str(),
                        },
                        list: query_result
                            .iter()
                            .map(|(target, providers)| {
                                (
                                    target.dupe(),
                                    providers
                                        .provider_collection()
                                        .default_info()
                                        .default_outputs()
                                        .into_boxed_slice(),
                                )
                            })
                            .collect(),
                    },
                )))
            }
        }
    }
}
