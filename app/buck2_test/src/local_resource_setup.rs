/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use anyhow::Context;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::builtin::external_runner_test_info::FrozenExternalRunnerTestInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::local_resource_info::FrozenLocalResourceInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::local_resource_info::LocalResourceInfoCallable;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::soft_error;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_test_api::data::RequiredLocalResources;
use dice::DiceTransaction;
use dupe::Dupe;
use indexmap::IndexMap;

/// Container for everything needed to set up a local resource.
#[derive(Debug)]
pub(crate) struct LocalResourceSetupContext {
    /// Configured target providing a local resource.
    pub target: ConfiguredTargetLabel,
    /// Setup CLI command.
    pub cmd: Vec<String>,
    /// Artifacts referenced in setup command.
    pub input_artifacts: Vec<ArtifactGroup>,
    /// Mapping from keys in JSON output of setup command to environment variable names
    /// which should be added to executions dependent on this local resource.
    pub env_var_mapping: IndexMap<String, String>,
    /// Timeout for setup command.
    pub timeout: Option<Duration>,
}

pub(crate) async fn required_local_resources_setup_contexts(
    dice: &DiceTransaction,
    executor_fs: &ExecutorFs<'_>,
    test_info: &FrozenExternalRunnerTestInfo,
    required_local_resources: &RequiredLocalResources,
) -> anyhow::Result<Vec<LocalResourceSetupContext>> {
    let providers = required_providers(dice, test_info, required_local_resources).await?;
    let mut cmd_line_context = DefaultCommandLineContext::new(executor_fs);
    let mut result = vec![];
    for (source_target_label, provider) in providers {
        let setup_command_line = provider.setup_command_line();
        let mut cmd: Vec<String> = vec![];
        setup_command_line.add_to_command_line(&mut cmd, &mut cmd_line_context)?;

        let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
        setup_command_line.visit_artifacts(&mut artifact_visitor)?;

        result.push(LocalResourceSetupContext {
            target: source_target_label.dupe(),
            cmd,
            input_artifacts: artifact_visitor.inputs.into_iter().collect(),
            env_var_mapping: provider.env_var_mapping(),
            timeout: provider.setup_timeout(),
        })
    }
    Ok(result)
}

async fn required_providers<'v>(
    dice: &DiceTransaction,
    test_info: &'v FrozenExternalRunnerTestInfo,
    required_local_resources: &'v RequiredLocalResources,
) -> anyhow::Result<Vec<(&'v ConfiguredTargetLabel, &'v FrozenLocalResourceInfo)>> {
    let available_resources = test_info.local_resources();

    let targets = required_local_resources
        .resources
        .iter()
        .map(|resource_type| &resource_type.name as &'v str)
        .map(|type_name| {
            available_resources.get(type_name).copied().ok_or_else(|| {
                anyhow::Error::msg(format!(
                    "Required local resource of type `{}` not found.",
                    type_name
                ))
            })
        })
        .filter_map(|r| match r {
            Ok(Some(x)) => Some(Ok(x)),
            Ok(None) => None,
            Err(e) => {
                let _ignore = soft_error!("missing_required_local_resource", e, quiet: true);
                None
            }
        })
        .collect::<Result<Vec<_>, anyhow::Error>>()?;

    let futs = targets.iter().map(|t| get_local_resource_info(dice, t));

    futures::future::join_all(futs)
        .await
        .into_iter()
        .collect::<Result<Vec<_>, _>>()
}

async fn get_local_resource_info<'v>(
    dice: &DiceTransaction,
    target: &'v ConfiguredProvidersLabel,
) -> anyhow::Result<(&'v ConfiguredTargetLabel, &'v FrozenLocalResourceInfo)> {
    let providers = dice.get_providers(target).await?.require_compatible()?;
    let providers = providers.provider_collection();
    Ok((
        target.target(),
        providers
            .get_provider(LocalResourceInfoCallable::provider_id_t())
            .context(format!(
                "Target `{}` expected to contain `LocalResourceInfo` provider",
                target
            ))?
            .as_ref(),
    ))
}
