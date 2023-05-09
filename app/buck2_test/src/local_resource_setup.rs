/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::builtin::external_runner_test_info::FrozenExternalRunnerTestInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::local_resource_info::FrozenLocalResourceInfo;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_test_api::data::RequiredLocalResources;
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
}

pub(crate) fn required_local_resources_setup_contexts(
    cmd_line_context: &mut dyn CommandLineContext,
    test_info: &FrozenExternalRunnerTestInfo,
    required_local_resources: &RequiredLocalResources,
) -> anyhow::Result<Vec<LocalResourceSetupContext>> {
    let providers = required_providers(test_info, required_local_resources)?;
    let mut result = vec![];
    for provider in providers {
        let setup_command_line = provider.setup_command_line();
        let mut cmd: Vec<String> = vec![];
        setup_command_line.add_to_command_line(&mut cmd, cmd_line_context)?;

        let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
        setup_command_line.visit_artifacts(&mut artifact_visitor)?;

        result.push(LocalResourceSetupContext {
            target: provider.source_target_label(),
            cmd,
            input_artifacts: artifact_visitor.inputs.into_iter().collect(),
            env_var_mapping: provider.env_var_mapping(),
        })
    }
    Ok(result)
}

fn required_providers<'v>(
    test_info: &'v FrozenExternalRunnerTestInfo,
    required_local_resources: &'v RequiredLocalResources,
) -> anyhow::Result<Vec<&'v FrozenLocalResourceInfo>> {
    let available_resources = test_info.local_resources();
    required_local_resources
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
        .collect()
}
