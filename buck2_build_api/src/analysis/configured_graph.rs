/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_node::compatibility::MaybeCompatible;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dice::DiceComputations;
use gazebo::prelude::*;
use owning_ref::ArcRef;

use crate::artifact_groups::deferred::DeferredTransitiveSetData;
use crate::artifact_groups::deferred::TransitiveSetKey;
use crate::artifact_groups::ArtifactGroup;
use crate::calculation::Calculation;
use crate::deferred::types::AnyValue;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::provider::builtin::template_placeholder_info::TemplatePlaceholderInfo;
use crate::query::analysis::environment::ConfiguredGraphQueryEnvironmentDelegate;

pub struct AnalysisDiceQueryDelegate<'c> {
    pub ctx: &'c DiceComputations,
}

impl<'c> AnalysisDiceQueryDelegate<'c> {
    pub(crate) fn ctx(&self) -> &DiceComputations {
        self.ctx
    }
}

pub struct AnalysisConfiguredGraphQueryDelegate<'a> {
    pub dice_query_delegate: Arc<AnalysisDiceQueryDelegate<'a>>,
    pub resolved_literals: HashMap<&'a str, ConfiguredTargetNode>,
}

#[async_trait]
impl<'a> ConfiguredGraphQueryEnvironmentDelegate for AnalysisConfiguredGraphQueryDelegate<'a> {
    fn eval_literal(&self, literal: &str) -> anyhow::Result<ConfiguredTargetNode> {
        self.resolved_literals
            .get(literal)
            .duped()
            .ok_or_else(|| anyhow::anyhow!(""))
    }

    async fn dice_lookup_transitive_set(
        &self,
        key: TransitiveSetKey,
    ) -> SharedResult<ArcRef<dyn AnyValue, DeferredTransitiveSetData>> {
        self.dice_query_delegate
            .ctx
            .compute_deferred_data(&key)
            .await
    }

    async fn get_template_info_provider_artifacts(
        &self,
        configured_label: &ConfiguredTargetLabel,
        template_name: &str,
    ) -> anyhow::Result<Vec<ArtifactGroup>> {
        let providers_label =
            ConfiguredProvidersLabel::new(configured_label.dupe(), ProvidersName::Default);

        let providers = self
            .dice_query_delegate
            .ctx()
            .get_providers(&providers_label);

        let mut artifacts = vec![];

        match providers.await? {
            MaybeCompatible::Incompatible(reason) => {
                eprintln!("{}", reason.skipping_message(configured_label));
            }
            MaybeCompatible::Compatible(providers) => {
                let providers_collection = providers.provider_collection();

                if let Some(template_placeholder_info) =
                    TemplatePlaceholderInfo::from_providers(providers_collection)
                {
                    if let Some(template_info) = template_placeholder_info
                        .keyed_variables()
                        .get(template_name)
                    {
                        let mut cmd_visitor = SimpleCommandLineArtifactVisitor::new();
                        if let either::Either::Left(command_line_arg) = template_info {
                            CommandLineArgLike::visit_artifacts(
                                command_line_arg.as_ref(),
                                &mut cmd_visitor,
                            )?;
                        } else if let either::Either::Right(map) = template_info {
                            for (_, command_line_arg) in map.iter() {
                                CommandLineArgLike::visit_artifacts(
                                    command_line_arg.as_ref(),
                                    &mut cmd_visitor,
                                )?;
                            }
                        }

                        for input in cmd_visitor.inputs {
                            artifacts.push(input);
                        }
                    }
                }
            }
        }

        Ok(artifacts)
    }
}
