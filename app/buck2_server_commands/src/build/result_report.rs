/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Processing and reporting the the results of the build

use std::collections::BTreeSet;

use buck2_build_api::build::BuildProviderType;
use buck2_build_api::build::BuildTargetResult;
use buck2_build_api::build::ConfiguredBuildTargetResult;
use buck2_build_api::build::ProviderArtifacts;
use buck2_build_api::interpreter::rule_defs::cmd_args::AbsCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::provider::builtin::run_info::FrozenRunInfo;
use buck2_certs::validate::CertState;
use buck2_certs::validate::check_cert_state;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::pattern::pattern::Modifiers;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact::fs::ExecutorFs;
use dupe::Dupe;
use fxhash::FxHashMap;
use starlark_map::small_map::SmallMap;

mod proto {
    pub(crate) use buck2_cli_proto::BuildTarget;
    pub(crate) use buck2_cli_proto::build_target::BuildOutput;
    pub(crate) use buck2_cli_proto::build_target::build_output::BuildOutputProviders;
}

/// Simple container for multiple [`buck2_error::Error`]s
pub(crate) struct BuildErrors {
    pub(crate) errors: Vec<buck2_error::Error>,
}

#[derive(Copy, Clone, Dupe)]
pub(crate) struct ResultReporterOptions {
    pub(crate) return_outputs: bool,
}

/// Collects build results into a Result<Vec<proto::BuildTarget>, buck2_error::Errors>. If any targets
/// fail, then the error case will be returned, otherwise a vec of all the successful results.
pub(crate) struct ResultReporter<'a> {
    artifact_fs: &'a ArtifactFs,
    options: ResultReporterOptions,
    results: Vec<proto::BuildTarget>,
}

pub(crate) struct BuildTargetsAndErrors {
    pub(crate) build_targets: Vec<proto::BuildTarget>,
    pub(crate) build_errors: BuildErrors,
}

impl<'a> ResultReporter<'a> {
    pub(crate) async fn convert(
        artifact_fs: &'a ArtifactFs,
        cert_state: CertState,
        options: ResultReporterOptions,
        build_result: &BuildTargetResult,
    ) -> buck2_error::Result<BuildTargetsAndErrors> {
        let mut out = Self {
            artifact_fs,
            options,
            results: Vec::new(),
        };

        let mut non_action_errors = vec![];
        let mut action_errors = vec![];
        non_action_errors.extend(build_result.other_errors.values().flatten().cloned());

        for (k, v) in &build_result.configured {
            // We omit skipped targets here.
            let Some(v) = v else { continue };
            non_action_errors.extend(v.errors.iter().cloned());
            action_errors.extend(v.outputs.iter().filter_map(|x| x.as_ref().err()).cloned());

            out.collect_result(k, v, build_result.configured_to_pattern_modifiers.get(k))?;
        }

        let mut error_list = if let Some(e) = non_action_errors.pop() {
            // FIXME(JakobDegen): We'd like to return more than one error here, but we have
            // to get better at error deduplication first
            vec![e]
        } else {
            // FIXME: Only one non-action error or all action errors is returned currently
            action_errors
        };

        if !error_list.is_empty() {
            if let Some(e) = check_cert_state(cert_state).await {
                error_list.push(e.into());
            }
        }

        Ok(BuildTargetsAndErrors {
            build_targets: out.results,
            build_errors: BuildErrors { errors: error_list },
        })
    }

    fn collect_result(
        &mut self,
        label: &ConfiguredProvidersLabel,
        result: &ConfiguredBuildTargetResult,
        pattern_modifiers: Option<&BTreeSet<Modifiers>>,
    ) -> buck2_error::Result<()> {
        let outputs = result
            .outputs
            .iter()
            .filter_map(|output| output.as_ref().ok());

        let mut artifact_path_mapping = FxHashMap::default();

        // NOTE: We use an SmallMap here to preserve the order the rule author wrote, all
        // the while avoiding duplicates.
        let mut artifacts = SmallMap::new();

        for output in outputs {
            let ProviderArtifacts {
                values,
                provider_type,
            } = output;

            if !self.options.return_outputs && !matches!(provider_type, BuildProviderType::Run) {
                continue;
            }

            if matches!(provider_type, BuildProviderType::DefaultOther) {
                continue;
            }

            for (artifact, value) in values.iter() {
                if matches!(provider_type, BuildProviderType::Run) {
                    artifact_path_mapping.insert(artifact, value.content_based_path_hash());
                }

                if self.options.return_outputs {
                    let entry =
                        artifacts
                            .entry(artifact)
                            .or_insert_with(|| proto::BuildOutputProviders {
                                default_info: false,
                                run_info: false,
                                other: false,
                                test_info: false,
                            });

                    match provider_type {
                        BuildProviderType::Default => {
                            entry.default_info = true;
                        }
                        BuildProviderType::DefaultOther => {
                            entry.other = true;
                        }
                        BuildProviderType::Run => {
                            entry.run_info = true;
                        }
                        BuildProviderType::Test => {
                            entry.test_info = true;
                        }
                    }
                }
            }
        }

        let artifact_fs = self.artifact_fs;

        let mut outputs: Vec<proto::BuildOutput> = Vec::new();
        for (a, providers) in artifacts.into_iter() {
            let output = proto::BuildOutput {
                path: a.resolve_configuration_hash_path(artifact_fs)?.to_string(),
                providers: Some(providers),
            };
            outputs.push(output);
        }

        let target = label.unconfigured().to_string();
        let configuration = label.cfg().to_string();

        let configured_graph_size = match &result.graph_properties {
            Some(Ok(MaybeCompatible::Compatible(v))) => Some(v.configured_graph_size),
            Some(Ok(MaybeCompatible::Incompatible(..))) => None,
            Some(Err(e)) => {
                // We don't expect an error on this unless something else on this target
                // failed.
                tracing::debug!(
                    "Graph size calculation error failed for {}: {:#}",
                    target,
                    e
                );
                None
            }
            None => None,
        };

        let run_args = if let Some(providers) = result.provider_collection.as_ref() {
            if let Some(runinfo) = providers
                .provider_collection()
                .builtin_provider::<FrozenRunInfo>()
            {
                // Produce arguments to run on a local machine.
                let path_separator = if cfg!(windows) {
                    PathSeparatorKind::Windows
                } else {
                    PathSeparatorKind::Unix
                };
                let executor_fs = ExecutorFs::new(self.artifact_fs, path_separator);
                let mut cli = Vec::<String>::new();
                let mut ctx = AbsCommandLineContext::new(&executor_fs);
                match runinfo.add_to_command_line(&mut cli, &mut ctx, &artifact_path_mapping) {
                    Ok(_) => cli,
                    Err(_) => {
                        // If we have action errors, then it's possible that we weren't able to produce
                        // the run info because we couldn't resolve a content-based path, and that's okay
                        // because we don't expect to be able to use it anyway.
                        Vec::new()
                    }
                }
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        match pattern_modifiers {
            Some(modifiers) => {
                for modifier in modifiers.iter() {
                    let target_with_modifiers = match modifier.as_slice() {
                        Some(modifiers) => format!("{}?{}", target, modifiers.join("+")),
                        None => target.clone(),
                    };

                    self.results.push(proto::BuildTarget {
                        target: target_with_modifiers,
                        configuration: configuration.clone(),
                        run_args: run_args.clone(),
                        target_rule_type_name: result.target_rule_type_name.clone(),
                        outputs: outputs.clone(),
                        configured_graph_size,
                    });
                }
            }
            None => self.results.push(proto::BuildTarget {
                target,
                configuration,
                run_args,
                target_rule_type_name: result.target_rule_type_name.clone(),
                outputs,
                configured_graph_size,
            }),
        }
        Ok(())
    }
}
