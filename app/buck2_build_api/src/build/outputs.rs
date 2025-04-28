/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use derive_more::Display;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::Key;

use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::artifact_groups::ArtifactGroup;
use crate::build::BuildProviderType;
use crate::build::ProvidersToBuild;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::provider::builtin::run_info::FrozenRunInfo;
use crate::interpreter::rule_defs::provider::test_provider::TestProvider;

/// Gets the list of outputs for a top-level build/run/install/test/etc target.
pub async fn get_outputs_for_top_level_target(
    ctx: &mut DiceComputations<'_>,
    providers_label: &ConfiguredProvidersLabel,
    providers_to_build: &ProvidersToBuild,
) -> buck2_error::Result<MaybeCompatible<Arc<Vec<(ArtifactGroup, BuildProviderType)>>>> {
    #[derive(Allocative, Debug, Display, Clone, Eq, PartialEq, Hash)]
    #[display("TopLevelTargetOutputsKey({}, {:?})", &self.0, &self.1)]
    struct TopLevelTargetOutputsKey(ConfiguredProvidersLabel, ProvidersToBuild);
    #[async_trait]
    impl Key for TopLevelTargetOutputsKey {
        type Value =
            buck2_error::Result<MaybeCompatible<Arc<Vec<(ArtifactGroup, BuildProviderType)>>>>;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            _cancellation: &CancellationContext,
        ) -> Self::Value {
            let providers_label = &self.0;
            let providers_to_build = &self.1;
            let providers = match ctx.get_providers(providers_label).await? {
                MaybeCompatible::Incompatible(reason) => {
                    return Ok(MaybeCompatible::Incompatible(reason));
                }
                MaybeCompatible::Compatible(v) => v,
            };
            let mut outputs = Vec::new();
            let collection = providers.provider_collection();
            if providers_to_build.default {
                collection
                    .default_info()?
                    .for_each_default_output_artifact_only(&mut |o| {
                        outputs.push((ArtifactGroup::Artifact(o), BuildProviderType::Default))
                    })?;
            }
            if providers_to_build.default_other {
                collection
                    .default_info()?
                    .for_each_default_output_other_artifacts_only(&mut |o| {
                        outputs.push((o, BuildProviderType::DefaultOther))
                    })?;
                collection.default_info()?.for_each_other_output(&mut |o| {
                    outputs.push((o, BuildProviderType::DefaultOther))
                })?;
            }
            if providers_to_build.run {
                if let Some(runinfo) = providers
                    .provider_collection()
                    .builtin_provider::<FrozenRunInfo>()
                {
                    let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
                    runinfo.visit_artifacts(&mut artifact_visitor)?;
                    for input in artifact_visitor.inputs {
                        outputs.push((input, BuildProviderType::Run));
                    }
                }
            }
            if providers_to_build.tests {
                if let Some(test_provider) = <dyn TestProvider>::from_collection(collection) {
                    let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
                    test_provider.visit_artifacts(&mut artifact_visitor)?;
                    for input in artifact_visitor.inputs {
                        outputs.push((input, BuildProviderType::Test));
                    }
                }
            }
            Ok(MaybeCompatible::Compatible(Arc::new(outputs)))
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            false
        }
    }
    ctx.compute(&TopLevelTargetOutputsKey(
        providers_label.clone(),
        providers_to_build.clone(),
    ))
    .await?
}
