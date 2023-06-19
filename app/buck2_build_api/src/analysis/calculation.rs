/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Rule analysis related Dice calculations
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;

use async_trait::async_trait;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_ref::ConfiguredGraphNodeRef;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use dupe::Dupe;

use crate::analysis::AnalysisResult;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;

pub static EVAL_ANALYSIS_QUERY: LateBinding<
    for<'a> fn(
        &'a DiceComputations,
        &'a str,
        HashMap<String, ConfiguredTargetNode>,
    ) -> Pin<
        Box<dyn Future<Output = anyhow::Result<TargetSet<ConfiguredGraphNodeRef>>> + Send + 'a>,
    >,
> = LateBinding::new("EVAL_ANALYSIS_QUERY");

#[async_trait]
pub trait RuleAnalsysisCalculationImpl: Send + Sync + 'static {
    async fn get_analysis_result(
        &self,
        ctx: &DiceComputations,
        target: &ConfiguredTargetLabel,
    ) -> anyhow::Result<MaybeCompatible<AnalysisResult>>;
}

pub static RULE_ANALYSIS_CALCULATION: LateBinding<&'static dyn RuleAnalsysisCalculationImpl> =
    LateBinding::new("RULE_ANALYSIS_CALCULATION");

#[async_trait]
pub trait RuleAnalysisCalculation {
    /// Returns the analysis result for a ConfiguredTargetLabel. This is the full set of Providers
    /// returned by the target's rule implementation function.
    async fn get_analysis_result(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> anyhow::Result<MaybeCompatible<AnalysisResult>>;

    /// Return the analysis result for a configuration rule `TargetLabel`
    /// (e. g. `constraint_value`).
    async fn get_configuration_analysis_result(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<AnalysisResult>;

    /// Returns the provider collection for a ConfiguredProvidersLabel. This is the full set of Providers
    /// returned by the target's rule implementation function.
    async fn get_providers(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<MaybeCompatible<FrozenProviderCollectionValue>>;
}

#[async_trait]
impl RuleAnalysisCalculation for DiceComputations {
    async fn get_analysis_result(
        &self,
        target: &ConfiguredTargetLabel,
    ) -> anyhow::Result<MaybeCompatible<AnalysisResult>> {
        RULE_ANALYSIS_CALCULATION
            .get()?
            .get_analysis_result(self, target)
            .await
    }

    async fn get_configuration_analysis_result(
        &self,
        target: &TargetLabel,
    ) -> anyhow::Result<AnalysisResult> {
        // Analysis for configuration nodes is always done with the unbound configuration.
        let target = target.configure_pair(ConfigurationNoExec::unbound().cfg_pair().dupe());
        Ok(self
            .get_analysis_result(&target)
            .await?
            .require_compatible()?)
    }

    async fn get_providers(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<MaybeCompatible<FrozenProviderCollectionValue>> {
        let analysis = self.get_analysis_result(target.target()).await?;

        analysis.try_map(|analysis| analysis.lookup_inner(target))
    }
}
